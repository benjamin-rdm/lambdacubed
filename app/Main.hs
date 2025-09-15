{-# LANGUAGE DataKinds #-}

module Main (main) where

import App.Config qualified as C
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.Trans.Class (lift)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Game.Block.Atlas (Atlas (..), SpecialIndices (..), buildBlockAtlas)
import Game.ChunkWorkers
import Game.Physics
import Game.World
import Game.WorldManager
import Game.WorldSource (generatedTerrian)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Rendering.Camera
import Rendering.Mesh (Mesh (..))
import Rendering.Render (AtlasIndex (..), MonadRender (..), RenderState (..))
import Rendering.Shader.Outline (OutlineUs, loadOutlineProgram)
import Rendering.Shader.Sky (SkyUs, bindSkyStatics, loadSkyProgram)
import Rendering.Shader.Terrain (TerrainUs, bindTerrainStatics, loadTerrainProgramWith)
import Rendering.Shader.Typed (ProgramU (..), setFloat, setMat4, withProgram)
import Rendering.Shader.UI (UiUs, bindUiStatics, loadUIProgram)
import Rendering.Texture (loadTextureAtUnit, withTextureUnit)
import Utils.Monad

data Env = Env
  { envWin :: !GLFW.Window,
    envCamRef :: !(IORef Camera),
    envPlayerRef :: !(IORef Player),
    envClickStateRef :: !(IORef (Bool, Bool)),
    envTimeRef :: !(IORef Double),
    envFpsRef :: !(IORef (Int, Double)),
    envOutlineRef :: !(IORef Bool),
    envKeyPrevRef :: !(IORef (Map.Map GLFW.Key Bool)),
    envCMRef :: !(IORef WorldState),
    envCMConfig :: !WorldConfig,
    envAspectRef :: !(IORef Float),
    envRender :: !RenderState
  }

newtype AppT m a = AppM {unAppM :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

type AppM = AppT IO

type GameM = WorldT AppM

runGameM :: GameM a -> AppM a
runGameM act = do
  Env {envCMRef, envCMConfig} <- askEnv
  cm <- liftIO $ readIORef envCMRef
  (v, cm') <- runWorld envCMConfig cm act
  liftIO $ writeIORef envCMRef cm'
  pure v

class (Monad m) => MonadEnv m where
  askEnv :: m Env

instance MonadEnv AppM where
  askEnv :: AppM Env
  askEnv = AppM ask

instance MonadEnv GameM where
  askEnv :: GameM Env
  askEnv = lift . lift $ askEnv

instance MonadRender AppM where
  askRender :: AppM RenderState
  askRender = do
    Env {envRender} <- askEnv
    pure envRender

runAppM :: Env -> AppM a -> IO a
runAppM env act = runReaderT (unAppM act) env

getCurrentTime :: IO Double
getCurrentTime = fromMaybe 0 <$> GLFW.getTime

getWindowAspectRatio :: GLFW.Window -> IO Float
getWindowAspectRatio win = do
  (w, h) <- GLFW.getFramebufferSize win
  pure $ if h == 0 then 1 else fromIntegral w / fromIntegral h

setupPositionUVAttributes :: IO ()
setupPositionUVAttributes = do
  GL.vertexAttribPointer (GL.AttribLocation 0)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (4 * 4) (plusPtr nullPtr 0))
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribPointer (GL.AttribLocation 1)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (4 * 4) (plusPtr nullPtr (2 * 4)))
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

createVAOWithVertices :: [Float] -> GL.NumComponents -> IO (GL.VertexArrayObject, GL.BufferObject)
createVAOWithVertices vertices componentCount = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  withArray vertices $ \ptr -> do
    let bytes = fromIntegral (length vertices * 4)
    GL.bufferData GL.ArrayBuffer $= (bytes, ptr, GL.StaticDraw)

  let stride :: GL.GLsizei
      stride = fromIntegral componentCount * 4
  GL.vertexAttribPointer (GL.AttribLocation 0)
    $= (GL.ToFloat, GL.VertexArrayDescriptor componentCount GL.Float stride (plusPtr nullPtr 0))
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.bindVertexArrayObject $= Nothing

  pure (vao, vbo)

isKeyPressed :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyPressed win key = (== GLFW.KeyState'Pressed) <$> GLFW.getKey win key

isKeyJustPressed :: GLFW.Window -> IORef (Map.Map GLFW.Key Bool) -> GLFW.Key -> IO Bool
isKeyJustPressed win ref key = do
  curr <- isKeyPressed win key
  prevMap <- readIORef ref
  let prev = Map.findWithDefault False key prevMap
  writeIORef ref (Map.insert key curr prevMap)
  pure (curr && not prev)

cursorPosCallback :: IORef Camera -> GLFW.CursorPosCallback
cursorPosCallback ref _ xpos ypos = modifyIORef' ref (applyMousePos (xpos, ypos))

updateTiming :: IORef Double -> IO Float
updateTiming timeRef = do
  tNow <- getCurrentTime
  tPrev <- readIORef timeRef
  let dt = realToFrac (tNow - tPrev) :: Float
  writeIORef timeRef tNow
  pure dt

getKeyboardInput :: GLFW.Window -> IO InputState
getKeyboardInput win = do
  w <- isKeyPressed win GLFW.Key'W
  s <- isKeyPressed win GLFW.Key'S
  a <- isKeyPressed win GLFW.Key'A
  d <- isKeyPressed win GLFW.Key'D
  sp <- isKeyPressed win GLFW.Key'Space
  pure $ InputState w s a d sp

handleOutlineToggle :: GLFW.Window -> IORef Bool -> IORef (Map.Map GLFW.Key Bool) -> IO ()
handleOutlineToggle win outlineEnabledRef keyPrevRef = do
  justPressed <- isKeyJustPressed win keyPrevRef GLFW.Key'O
  when justPressed $ modifyIORef' outlineEnabledRef not

updatePlayerAndCamera :: (MonadEnv m, MonadWorld m, MonadIO m) => m ()
updatePlayerAndCamera = do
  Env {envCamRef, envPlayerRef} <- askEnv
  cam <- liftIO $ readIORef envCamRef
  let Camera _ front _ _ _ = cam
  dt <- do Env {envTimeRef} <- askEnv; liftIO $ updateTiming envTimeRef
  input <- do Env {envWin} <- askEnv; liftIO $ getKeyboardInput envWin
  player0 <- liftIO $ readIORef envPlayerRef
  player1 <- stepPlayer front input dt player0
  liftIO $ writeIORef envPlayerRef player1
  Player (V3 px py pz) _ <- liftIO $ readIORef envPlayerRef
  liftIO $ modifyIORef' envCamRef $ \(Camera _ f up pxy ppy) -> Camera (V3 px py (pz + playerEyeHeight)) f up pxy ppy

updateMouseState :: GLFW.Window -> IORef (Bool, Bool) -> IO (Bool, Bool)
updateMouseState win clickStateRef = do
  lmb <- (== GLFW.MouseButtonState'Pressed) <$> GLFW.getMouseButton win GLFW.MouseButton'1
  rmb <- (== GLFW.MouseButtonState'Pressed) <$> GLFW.getMouseButton win GLFW.MouseButton'2
  (prevL, prevR) <- readIORef clickStateRef
  let clickL = lmb && not prevL
      clickR = rmb && not prevR
  writeIORef clickStateRef (lmb, rmb)
  pure (clickL, clickR)

handleBlockBreak :: (MonadWorld m) => V3 Int -> m ()
handleBlockBreak hit = void $ setBlockAtWorld hit Air

handleBlockPlace :: (MonadWorld m) => V3 Int -> V3 Int -> m ()
handleBlockPlace hit normal = do
  let placePos = hit + normal
  -- TODO: Shouldn't this always be ==Air?
  whenM ((== Air) <$> blockAt placePos) (void $ setBlockAtWorld placePos Stone)

handleMouseClicks :: (MonadIO m, MonadWorld m, MonadEnv m) => m ()
handleMouseClicks = do
  Env {envWin, envCamRef, envClickStateRef} <- askEnv
  (clickL, clickR) <- liftIO $ updateMouseState envWin envClickStateRef
  when (clickL || clickR) $ do
    Camera camPos camFront _ _ _ <- liftIO $ readIORef envCamRef
    whenJustM
      (raycastBlock camPos camFront C.interactionDistance)
      ( \(hit, normal) ->
          if clickL
            then handleBlockBreak hit
            else handleBlockPlace hit normal
      )

processInput :: (MonadIO m, MonadWorld m, MonadEnv m) => m ()
processInput = do
  updatePlayerAndCamera
  handleMouseClicks
  Env {envWin, envOutlineRef, envKeyPrevRef} <- askEnv
  liftIO $ handleOutlineToggle envWin envOutlineRef envKeyPrevRef

initializeWindow :: IO GLFW.Window
initializeWindow = do
  ok <- GLFW.init
  if not ok
    then fail "GLFW initialization failed"
    else do
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      GLFW.windowHint $ GLFW.WindowHint'Samples (Just 4)

      mwin <- GLFW.createWindow C.windowWidth C.windowHeight "game window" Nothing Nothing
      case mwin of
        Nothing -> fail "Failed to create GLFW window"
        Just win -> do
          GLFW.makeContextCurrent (Just win)
          GLFW.swapInterval 1
          pure win

setupOpenGL :: GLFW.Window -> IORef Float -> IO ()
setupOpenGL win aspectRef = do
  (w0, h0) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w0) (fromIntegral h0))
  GLFW.setFramebufferSizeCallback
    win
    ( Just $ \_ w h -> do
        GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        writeIORef aspectRef (if h == 0 then 1 else fromIntegral w / fromIntegral h)
    )
  GL.blend $= GL.Disabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.depthFunc $= Just GL.Lequal
  GL.clearDepth $= 1
  GL.frontFace $= GL.CCW
  GL.cullFace $= Just GL.Back
  GL.multisample $= GL.Enabled

setupTerrainShader :: SpecialIndices -> GL.TextureObject -> IO (ProgramU TerrainUs)
setupTerrainShader spec atlasTex = do
  terrainProgU <- loadTerrainProgramWith spec
  withProgram terrainProgU $ do
    bindTerrainStatics terrainProgU 0 2 3 0.0

  withTextureUnit 0 $ GL.textureBinding GL.Texture2DArray $= Just atlasTex
  void $ loadTextureAtUnit 2 "resource_pack/assets/minecraft/textures/colormap/grass.png"
  void $ loadTextureAtUnit 3 "resource_pack/assets/minecraft/textures/colormap/foliage.png"
  pure terrainProgU

setupSkyShader :: IO (ProgramU SkyUs, GL.VertexArrayObject, GL.BufferObject)
setupSkyShader = do
  skyProgU <- loadSkyProgram
  withProgram skyProgU $ do
    bindSkyStatics skyProgU (V3 0.45 0.70 0.95) (V3 0.60 0.78 0.92)

  let skyVerts :: [Float]
      skyVerts =
        [ -1,
          -1,
          1,
          -1,
          1,
          1,
          -1,
          -1,
          1,
          1,
          -1,
          1
        ]
  (skyVAO, skyVBO) <- createVAOWithVertices skyVerts (2 :: GL.NumComponents)

  pure (skyProgU, skyVAO, skyVBO)

setupOutlineShader :: IO (ProgramU OutlineUs, GL.VertexArrayObject, GL.BufferObject)
setupOutlineShader = do
  outlineProgU <- loadOutlineProgram

  vao <- GL.genObjectName
  vbo <- GL.genObjectName

  withProgram outlineProgU $ do
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vbo

    -- Preallocate enough for 3 visible faces: 3 * 8 verts * 3 floats
    let maxFloats = 72 :: Int
    GL.bufferData GL.ArrayBuffer $= (fromIntegral (maxFloats * 4), nullPtr, GL.DynamicDraw)

    GL.vertexAttribPointer (GL.AttribLocation 0)
      $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (3 * 4) (plusPtr nullPtr 0))
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.bindVertexArrayObject $= Nothing

  pure (outlineProgU, vao, vbo)

setupUIShader :: IO (ProgramU UiUs, GL.TextureObject, GL.VertexArrayObject, GL.BufferObject)
setupUIShader = do
  uiProgU <- loadUIProgram
  uiTex <- loadTextureAtUnit 1 "resource_pack/assets/minecraft/textures/gui/sprites/hud/crosshair.png"
  withProgram uiProgU $ do
    bindUiStatics uiProgU 1

  let crosshairSize = 0.04 :: Float
      vertices :: [Float]
      vertices =
        [ -crosshairSize,
          -crosshairSize,
          0.0,
          0.0,
          crosshairSize,
          -crosshairSize,
          1.0,
          0.0,
          crosshairSize,
          crosshairSize,
          1.0,
          1.0,
          -crosshairSize,
          crosshairSize,
          0.0,
          1.0
        ]

  (uiVAO, uiVBO) <- createVAOWithVertices vertices (4 :: GL.NumComponents)
  GL.bindVertexArrayObject $= Just uiVAO
  setupPositionUVAttributes
  GL.bindVertexArrayObject $= Nothing

  pure (uiProgU, uiTex, uiVAO, uiVBO)

initializePlayerAndCamera :: GLFW.Window -> IO (IORef Camera, IORef Player, IORef (Bool, Bool), IORef Double, IORef (Int, Double), IORef Bool, IORef (Map.Map GLFW.Key Bool))
initializePlayerAndCamera win = do
  camRef <- newIORef $ setPosAndAngles (V3 32 32 (50 + playerEyeHeight)) (-20, -45) defaultCam
  let startPlayer = Player (V3 32 32 50) (V3 0 0 0)
  playerRef <- newIORef startPlayer

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
  GLFW.setCursorPosCallback win (Just $ cursorPosCallback camRef)

  t0 <- getCurrentTime
  timeRef <- newIORef t0
  fpsRef <- newIORef (0 :: Int, t0 :: Double)
  clickStateRef <- newIORef (False, False)
  outlineEnabledRef <- newIORef True
  keyPrevRef <- newIORef Map.empty

  pure (camRef, playerRef, clickStateRef, timeRef, fpsRef, outlineEnabledRef, keyPrevRef)

updateProjViewAllM :: (MonadEnv m, MonadIO m) => m ()
updateProjViewAllM = do
  Env
    { envCamRef,
      envAspectRef,
      envRender = RenderState {rsTerrainP, rsOutlineP}
    } <-
    askEnv
  cam <- liftIO $ readIORef envCamRef
  aspect <- liftIO $ readIORef envAspectRef
  let projM = perspective (realToFrac (pi / 3 :: Double)) aspect 0.1 500.0
      Camera camPos front up _ _ = cam
      viewM = lookAt camPos (camPos + front) up

  withProgram rsTerrainP $ do
    setMat4 @"uProj" rsTerrainP projM
    setMat4 @"uView" rsTerrainP viewM

  withProgram rsOutlineP $ do
    setMat4 @"uProj" rsTerrainP projM
    setMat4 @"uView" rsTerrainP viewM

runGame :: IO ()
runGame = do
  win <- initializeWindow
  aspect0 <- getWindowAspectRatio win
  aspectRef <- newIORef aspect0
  setupOpenGL win aspectRef

  Atlas {atTexture = atlasTex, atLayerOf = layerOf, atOverlayOf = overlayOf, atSpecial = spec} <- buildBlockAtlas
  let atlasIndex = AtlasIndex {aiLayerOf = layerOf, aiOverlayOf = overlayOf}
  terrainProgU <- setupTerrainShader spec atlasTex
  (skyProgU, skyVAO, skyVBO) <- setupSkyShader
  (outlineProgU, outlineVAO, outlineVBO) <- setupOutlineShader
  (uiProgU, uiTex, uiVAO, uiVBO) <- setupUIShader

  let workerCount = 3
      texOfF b d = realToFrac (layerOf b d) :: Float
      overlayOfF b d = realToFrac <$> overlayOf b d :: Maybe Float
  cw <- startChunkWorkers workerCount generatedTerrian texOfF overlayOfF
  let cmCfg = mkWorldConfig cw
  cmRef <- newIORef initialWorldState
  (camRef, playerRef, clickStateRef, timeRef, fpsRef, outlineEnabledRef, keyPrevRef) <- initializePlayerAndCamera win

  let env =
        Env
          { envWin = win,
            envCamRef = camRef,
            envPlayerRef = playerRef,
            envClickStateRef = clickStateRef,
            envTimeRef = timeRef,
            envFpsRef = fpsRef,
            envOutlineRef = outlineEnabledRef,
            envKeyPrevRef = keyPrevRef,
            envCMRef = cmRef,
            envCMConfig = cmCfg,
            envAspectRef = aspectRef,
            envRender =
              RenderState
                { rsTerrainP = terrainProgU,
                  rsSkyP = skyProgU,
                  rsSkyVAO = skyVAO,
                  rsSkyVBO = skyVBO,
                  rsOutlineP = outlineProgU,
                  rsOutlineVAO = outlineVAO,
                  rsOutlineVBO = outlineVBO,
                  rsUIP = uiProgU,
                  rsUITex = uiTex,
                  rsUIVAO = uiVAO,
                  rsUIVBO = uiVBO,
                  rsAtlasTex = atlasTex,
                  rsAtlasIndex = atlasIndex
                }
          }

  let drawFrameG :: GameM ()
      drawFrameG = do
        processInput
        Env {envPlayerRef} <- askEnv
        Player ppos _ <- liftIO $ readIORef envPlayerRef
        liftIO clearFrame
        drawSkyM
        updateProjViewAllM
        currentTime <- liftIO getCurrentTime
        Env {envRender = RenderState {rsTerrainP}} <- askEnv
        withProgram rsTerrainP $ do
          setFloat @"uTime" rsTerrainP (realToFrac currentTime)

        updatePlayerPosition ppos
        updateChunks

        chunksDraw <- loadedChunks

        drawWorldOpaque chunksDraw
        drawWorldOverlays chunksDraw
        drawWorldLeaves chunksDraw
        drawWorldWater chunksDraw
        drawCrosshairUIM

        Env {envOutlineRef} <- askEnv
        outlineEnabled <- liftIO $ readIORef envOutlineRef
        when outlineEnabled drawBlockOutlineM

        liftIO $ do
          GLFW.swapBuffers win
          GLFW.pollEvents
        updateFpsTitleM

      drawFrame :: AppM ()
      drawFrame = runGameM drawFrameG
      loopM :: AppM ()
      loopM = do
        drawFrame
        shouldClose <- liftIO $ GLFW.windowShouldClose win
        esc <- liftIO $ GLFW.getKey win GLFW.Key'Escape
        unless (shouldClose || esc == GLFW.KeyState'Pressed) loopM

  runAppM env loopM
  GLFW.terminate

main :: IO ()
main = runGame

clearFrame :: IO ()
clearFrame = do
  GL.clearColor $= GL.Color4 0.15 0.18 0.22 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

-- Temporarily enable blend for the IO action
withBlend :: (MonadIO m) => m a -> m ()
withBlend a =
  GL.blend $= GL.Enabled
    >> GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    >> a
    >> GL.blend $= GL.Disabled

withoutFaceCull :: (MonadIO m) => m a -> m ()
withoutFaceCull a = GL.cullFace $= Nothing >> a >> GL.cullFace $= Just GL.Back

drawWorldOpaque :: (MonadEnv m, MonadIO m) => ChunkMap -> m ()
drawWorldOpaque chunks = do
  Env {envRender = RenderState {rsTerrainP}} <- askEnv

  withProgram rsTerrainP $ do
    mapM_
      ( \h -> do
          let GpuMesh vao _ count = mOpaque (chMeshes h)
          GL.bindVertexArrayObject $= Just vao
          GL.drawArrays GL.Triangles 0 (fromIntegral count)
      )
      (M.elems chunks)

drawWorldWater :: (MonadIO m, MonadEnv m) => ChunkMap -> m ()
drawWorldWater chunks = do
  Env {envRender = RenderState {rsTerrainP}} <- askEnv

  withProgram rsTerrainP $ withBlend $ do
    mapM_
      ( \h -> do
          let GpuMesh vao _ count = mWater (chMeshes h)
          GL.bindVertexArrayObject $= Just vao
          GL.drawArrays GL.Triangles 0 (fromIntegral count)
      )
      (M.elems chunks)

drawWorldLeaves :: (MonadEnv m, MonadIO m) => ChunkMap -> m ()
drawWorldLeaves chunks = do
  -- Alpha cutoff needs to be set here for transparent textures like leaves draw
  -- while still showing transparent textures in the same mesh behind them
  Env {envRender = RenderState {rsTerrainP}} <- askEnv

  withProgram rsTerrainP $ withoutFaceCull $ do
    setFloat @"uAlphaCutoff" rsTerrainP 0.5
    mapM_
      ( \h -> do
          let GpuMesh vao _ count = mLeaves (chMeshes h)
          GL.bindVertexArrayObject $= Just vao
          GL.drawArrays GL.Triangles 0 (fromIntegral count)
      )
      (M.elems chunks)
    setFloat @"uAlphaCutoff" rsTerrainP 0.0

drawWorldOverlays :: (MonadEnv m, MonadIO m) => ChunkMap -> m ()
drawWorldOverlays chunks = do
  Env {envRender = RenderState {rsTerrainP}} <- askEnv

  withProgram rsTerrainP $ do
    setFloat @"uAlphaCutoff" rsTerrainP 0.5
    mapM_
      ( \h -> do
          let GpuMesh vao _ count = mGrassOverlay (chMeshes h)
          GL.bindVertexArrayObject $= Just vao
          GL.drawArrays GL.Triangles 0 (fromIntegral count)
      )
      (M.elems chunks)
    setFloat @"uAlphaCutoff" rsTerrainP 0.5

drawCrosshairUIM :: (MonadEnv m, MonadIO m) => m ()
drawCrosshairUIM = withBlend $ do
  Env {envAspectRef, envRender = RenderState {rsUIP, rsUITex, rsUIVAO}} <- askEnv
  withProgram rsUIP $ withTextureUnit 1 $ do
    GL.textureBinding GL.Texture2D $= Just rsUITex
    aspect <- readIORef envAspectRef
    setFloat @"uAspect" rsUIP aspect

    GL.bindVertexArrayObject $= Just rsUIVAO
    GL.drawArrays GL.TriangleFan 0 4

    GL.bindVertexArrayObject $= Nothing

generateBlockOutlineVerticesM :: (MonadWorld m) => V3 Int -> V3 Float -> m [Float]
generateBlockOutlineVerticesM blk camPos = do
  let blkF@(V3 fx fy fz) = fromIntegral <$> blk

  let blockCenter = blkF + V3 0.5 0.5 0.5
      toCam = normalize (camPos - blockCenter)
      faceFacingCamera normal = toCam `dot` (realToFrac <$> normal) > 0

      p000 = V3 fx fy fz
      p100 = V3 (fx + 1) fy fz
      p110 = V3 (fx + 1) (fy + 1) fz
      p010 = V3 fx (fy + 1) fz
      p001 = V3 fx fy (fz + 1)
      p101 = V3 (fx + 1) fy (fz + 1)
      p111 = V3 (fx + 1) (fy + 1) (fz + 1)
      p011 = V3 fx (fy + 1) (fz + 1)

      loopEdges :: [V3 Float] -> [Float]
      loopEdges [a, b, c, d] =
        let pairs = [(a, b), (b, c), (c, d), (d, a)]
         in concatMap (\(V3 ax ay az, V3 bx by bz) -> [ax, ay, az, bx, by, bz]) pairs
      loopEdges _ = []

      bottomN = V3 0 0 (-1)
      topN = V3 0 0 1
      leftN = V3 (-1) 0 0
      rightN = V3 1 0 0
      frontN = V3 0 (-1) 0
      backN = V3 0 1 0

  bBottom <- blockAt (bottomN + blk)
  bTop <- blockAt (topN + blk)
  bLeft <- blockAt (leftN + blk)
  bRight <- blockAt (rightN + blk)
  bFront <- blockAt (frontN + blk)
  bBack <- blockAt (backN + blk)

  let solid b = b /= Air && blockOpaque b
      showBottom = not (solid bBottom) && faceFacingCamera bottomN
      showTop = not (solid bTop) && faceFacingCamera topN
      showLeft = not (solid bLeft) && faceFacingCamera leftN
      showRight = not (solid bRight) && faceFacingCamera rightN
      showFront = not (solid bFront) && faceFacingCamera frontN
      showBack = not (solid bBack) && faceFacingCamera backN

      bottomEdges = if showBottom then loopEdges [p100, p000, p010, p110] else []
      topEdges = if showTop then loopEdges [p001, p101, p111, p011] else []
      leftEdges = if showLeft then loopEdges [p000, p001, p011, p010] else []
      rightEdges = if showRight then loopEdges [p100, p101, p111, p110] else []
      frontEdges = if showFront then loopEdges [p000, p001, p101, p100] else []
      backEdges = if showBack then loopEdges [p010, p011, p111, p110] else []

  pure (bottomEdges ++ topEdges ++ leftEdges ++ rightEdges ++ frontEdges ++ backEdges)

drawBlockOutlineM :: (MonadEnv m, MonadWorld m, MonadIO m) => m ()
drawBlockOutlineM = do
  Env
    { envCamRef,
      envRender = RenderState {rsOutlineP, rsOutlineVAO, rsOutlineVBO}
    } <-
    askEnv
  Camera camPos camFront _ _ _ <- liftIO $ readIORef envCamRef
  whenJustM
    (raycastBlock camPos camFront C.interactionDistance)
    ( \(hitPos, _) -> do
        vertices <- generateBlockOutlineVerticesM hitPos camPos
        withProgram rsOutlineP $ do
          GL.bindVertexArrayObject $= Just rsOutlineVAO
          GL.bindBuffer GL.ArrayBuffer $= Just rsOutlineVBO
          withArray vertices $ \ptr -> do
            let bytes = (fromIntegral (length vertices * 4) :: GL.GLsizeiptr)
            GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (0 :: GL.GLintptr) bytes ptr
          GL.polygonMode $= (GL.Line, GL.Line)
          GL.lineWidth $= 2.0
          GL.drawArrays GL.Lines 0 (fromIntegral (length vertices `div` 3))
          GL.polygonMode $= (GL.Fill, GL.Fill)
          GL.bindVertexArrayObject $= Nothing
    )

updateFpsTitleM :: (MonadEnv m, MonadIO m) => m ()
updateFpsTitleM = do
  Env {envWin, envFpsRef, envPlayerRef} <- askEnv
  tNow <- liftIO getCurrentTime
  (fc, tStart) <- liftIO $ readIORef envFpsRef
  player <- liftIO $ readIORef envPlayerRef
  let Player (V3 px py pz) _ = player
      V3 ix iy iz = floor <$> V3 px py pz :: V3 Int
      fc' = fc + 1
      elapsed = realToFrac (tNow - tStart) :: Float
      posStr = "(" ++ show ix ++ ", " ++ show iy ++ ", " ++ show iz ++ ")"
  if elapsed >= 0.5 && tStart > 0
    then liftIO $ do
      let fps = fromIntegral fc' / elapsed
      GLFW.setWindowTitle envWin ("lambdacubed - " ++ posStr ++ " - " ++ show (round fps :: Int) ++ " FPS")
      writeIORef envFpsRef (0, tNow)
    else liftIO $ writeIORef envFpsRef (fc', tStart)

drawSkyM :: (MonadEnv m, MonadIO m) => m ()
drawSkyM = do
  Env {envRender = RenderState {rsSkyP, rsSkyVAO, rsSkyVBO}} <- askEnv
  withProgram rsSkyP $ do
    GL.depthFunc $= Nothing
    GL.bindVertexArrayObject $= Just rsSkyVAO
    GL.bindBuffer GL.ArrayBuffer $= Just rsSkyVBO
    GL.drawArrays GL.Triangles 0 6
    GL.depthFunc $= Just GL.Lequal
