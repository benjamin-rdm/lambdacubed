module Main where

import App.Config qualified as C
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Game.WorldManager
import Game.Physics
import Game.World
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Rendering.Camera
import Rendering.Shader.UI (loadUIProgram)
import Rendering.Shader.Terrain (loadTerrainProgram)
import Rendering.Shader.Sky (loadSkyProgram)
import Rendering.Shader.Outline (loadOutlineProgram)
import Rendering.Texture (createBlockTextureArray, loadTextureFromPng, setupTextureMode)
import Utils.Math (toGLMatrix)

data RenderState = RenderState
  { rsTerrainProg :: !GL.Program,
    rsUView :: !GL.UniformLocation,
    rsUProj :: !GL.UniformLocation,
    rsUFogColor :: !GL.UniformLocation,
    rsUFogStart :: !GL.UniformLocation,
    rsUFogEnd :: !GL.UniformLocation,
    rsUTime :: !GL.UniformLocation,
    rsSkyProg :: !GL.Program,
    rsSkyVAO :: !GL.VertexArrayObject,
    rsSkyVBO :: !GL.BufferObject,
    rsOutlineProg :: !GL.Program,
    rsOutlineVAO :: !GL.VertexArrayObject,
    rsOutlineVBO :: !GL.BufferObject,
    rsUOutlineView :: !GL.UniformLocation,
    rsUOutlineProj :: !GL.UniformLocation,
    rsUIProg :: !GL.Program,
    rsUITex :: !GL.TextureObject,
    rsUIVAO :: !GL.VertexArrayObject,
    rsUIVBO :: !GL.BufferObject,
    rsUUiTex :: !GL.UniformLocation,
    rsUUiAspect :: !GL.UniformLocation
  }

data Env = Env
  { envWin :: !GLFW.Window,
    envCamRef :: !(IORef Camera),
    envPlayerRef :: !(IORef Player),
    envClickStateRef :: !(IORef (Bool, Bool)),
    envTimeRef :: !(IORef Double),
    envFpsRef :: !(IORef (Int, Double)),
    envOutlineRef :: !(IORef Bool),
    envKeyPrevRef :: !(IORef (Map.Map GLFW.Key Bool)),
    envCMRef :: !(IORef ChunkManagerState),
    envAspectRef :: !(IORef Float),
    envRender :: !RenderState
  }

newtype AppM a = AppM {unAppM :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runAppM :: Env -> AppM a -> IO a
runAppM env act = runReaderT (unAppM act) env

askEnv :: AppM Env
askEnv = AppM ask

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

isKeyJustReleased :: GLFW.Window -> IORef (Map.Map GLFW.Key Bool) -> GLFW.Key -> IO Bool
isKeyJustReleased win ref key = do
  curr <- isKeyPressed win key
  prevMap <- readIORef ref
  let prev = Map.findWithDefault False key prevMap
  writeIORef ref (Map.insert key curr prevMap)
  pure (not curr && prev)

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

updatePlayerAndCamera :: AppM ()
updatePlayerAndCamera = do
  Env {envCamRef, envPlayerRef, envCMRef} <- askEnv
  cam <- liftIO $ readIORef envCamRef
  let Camera _ front _ _ _ = cam
  dt <- do Env {envTimeRef} <- askEnv; liftIO $ updateTiming envTimeRef
  input <- do Env {envWin} <- askEnv; liftIO $ getKeyboardInput envWin
  cm <- liftIO $ readIORef envCMRef
  let blockAtW = blockQueryFromChunkMap (cmsLoadedChunks cm)
  liftIO $ modifyIORef' envPlayerRef $ stepPlayer blockAtW front input dt
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

handleBlockBreak :: V3 Int -> AppM ()
handleBlockBreak hit = do
  Env {envCMRef} <- askEnv
  cm <- liftIO $ readIORef envCMRef
  -- Update block and rebuild mesh immediately via ChunkManager
  (ok, cm') <- liftIO $ runChunkManager defaultChunkManagerConfig cm (setBlockAtWorld hit Air)
  when ok $ liftIO $ writeIORef envCMRef cm'

handleBlockPlace :: V3 Int -> V3 Int -> AppM ()
handleBlockPlace hit normal = do
  Env {envCMRef} <- askEnv
  cm <- liftIO $ readIORef envCMRef
  let chunks = cmsLoadedChunks cm
      placePos = hit + normal
  when (blockQueryFromChunkMap chunks placePos == Air) $ do
    (ok, cm') <- liftIO $ runChunkManager defaultChunkManagerConfig cm (setBlockAtWorld placePos Stone)
    when ok $ liftIO $ writeIORef envCMRef cm'

handleMouseClicks :: AppM ()
handleMouseClicks = do
  Env {envWin, envCamRef, envClickStateRef, envCMRef} <- askEnv
  (clickL, clickR) <- liftIO $ updateMouseState envWin envClickStateRef
  when (clickL || clickR) $ do
    Camera camPos camFront _ _ _ <- liftIO $ readIORef envCamRef
    cm <- liftIO $ readIORef envCMRef
    let chunks = cmsLoadedChunks cm
    case raycastBlockData chunks camPos camFront C.interactionDistance of
      Nothing -> pure ()
      Just (hit, normal) ->
        if clickL
          then handleBlockBreak hit
          else handleBlockPlace hit normal

processInput :: AppM ()
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

  GL.depthFunc $= Just GL.Lequal
  GL.clearDepth $= 1
  GL.frontFace $= GL.CCW
  GL.cullFace $= Just GL.Back
  GL.multisample $= GL.Enabled

setupTerrainShader :: IO (GL.Program, (GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation, GL.UniformLocation))
setupTerrainShader = do
  terrainProg <- loadTerrainProgram
  GL.currentProgram $= Just terrainProg
  [uView, uProj, uFogColor, uFogStart, uFogEnd, uTime, uAtlas, uAlphaCutoff, uGrassColormap, uFoliageColormap] <-
    mapM (GL.get . GL.uniformLocation terrainProg)
      ["uView", "uProj", "uFogColor", "uFogStart", "uFogEnd", "uTime", "uAtlas", "uAlphaCutoff", "uGrassColormap", "uFoliageColormap"]

  atlas <- createBlockTextureArray
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2DArray $= Just atlas
  GL.uniform uAtlas $= GL.TextureUnit 0

  grassCM <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 2
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just grassCM
  setupTextureMode GL.Texture2D
  loadTextureFromPng "resource_pack/assets/minecraft/textures/colormap/grass.png"
  GL.generateMipmap GL.Texture2D $= GL.Enabled
  GL.uniform uGrassColormap $= GL.TextureUnit 2

  foliageCM <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 3
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just foliageCM
  setupTextureMode GL.Texture2D
  loadTextureFromPng "resource_pack/assets/minecraft/textures/colormap/foliage.png"
  GL.generateMipmap GL.Texture2D $= GL.Enabled
  GL.uniform uFoliageColormap $= GL.TextureUnit 3

  -- We set alpha cutoff to 0.0 as the default behavior
  GL.uniform uAlphaCutoff $= (0.0 :: GL.GLfloat)
  pure (terrainProg, (uView, uProj, uFogColor, uFogStart, uFogEnd, uAtlas, uTime, uAlphaCutoff, uGrassColormap, uFoliageColormap))

setupSkyShader :: IO (GL.Program, GL.VertexArrayObject, GL.BufferObject)
setupSkyShader = do
  skyProg <- loadSkyProgram
  GL.currentProgram $= Just skyProg
  [uSkyTop, uSkyHorizon] <- mapM (GL.get . GL.uniformLocation skyProg) ["uTopColor", "uHorizonColor"]

  let skyTop = GL.Color3 0.45 0.70 (0.95 :: Float)
      skyHorizon = GL.Color3 0.60 0.78 (0.92 :: Float)
  GL.uniform uSkyTop $= skyTop
  GL.uniform uSkyHorizon $= skyHorizon

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

  pure (skyProg, skyVAO, skyVBO)

setupOutlineShader :: IO (GL.Program, GL.VertexArrayObject, GL.BufferObject, GL.UniformLocation, GL.UniformLocation)
setupOutlineShader = do
  outlineProg <- loadOutlineProgram
  GL.currentProgram $= Just outlineProg
  [uOutlineView, uOutlineProj] <- mapM (GL.get . GL.uniformLocation outlineProg) ["uView", "uProj"]

  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  -- Preallocate enough for 3 visible faces: 3 * 8 verts * 3 floats
  let maxFloats = 72 :: Int
  GL.bufferData GL.ArrayBuffer $= (fromIntegral (maxFloats * 4), nullPtr, GL.DynamicDraw)

  GL.vertexAttribPointer (GL.AttribLocation 0)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (3 * 4) (plusPtr nullPtr 0))
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.bindVertexArrayObject $= Nothing

  pure (outlineProg, vao, vbo, uOutlineView, uOutlineProj)

setupUIShader :: IO (GL.Program, GL.TextureObject, GL.VertexArrayObject, GL.BufferObject, GL.UniformLocation, GL.UniformLocation)
setupUIShader = do
  uiProg <- loadUIProgram
  GL.currentProgram $= Just uiProg
  [uUiTex, uAspect] <- mapM (GL.get . GL.uniformLocation uiProg) ["uUiTex", "uAspect"]

  uiTex <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 1
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just uiTex
  setupTextureMode GL.Texture2D
  loadTextureFromPng "resource_pack/assets/minecraft/textures/gui/sprites/hud/crosshair.png"
  GL.generateMipmap GL.Texture2D $= GL.Enabled

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
  GL.activeTexture $= GL.TextureUnit 0

  pure (uiProg, uiTex, uiVAO, uiVBO, uUiTex, uAspect)

initializeGameState :: Float -> Float -> GL.Color3 Float -> GL.UniformLocation -> GL.UniformLocation -> GL.UniformLocation -> IO (IORef ChunkManagerState)
initializeGameState fogStartVal fogEndVal fogColor uFogStart uFogEnd uFogColor = do
  cmRef <- newIORef initialChunkManagerState

  GL.uniform uFogStart $= fogStartVal
  GL.uniform uFogEnd $= fogEndVal
  GL.uniform uFogColor $= fogColor

  let startCoord = chunkCoordOf (V3 32 32 50)
      desired = [V2 (cx + dx) (cy + dy) | let V2 cx cy = startCoord, dx <- [-3 .. 3], dy <- [-3 .. 3]]
  cmSeeded <- execChunkManager defaultChunkManagerConfig initialChunkManagerState $ do
    mapM_ loadChunk desired
  writeIORef cmRef cmSeeded

  pure cmRef

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

updateProjViewAllM :: AppM ()
updateProjViewAllM = do
  Env
    { envCamRef,
      envAspectRef,
      envRender = RenderState {rsUProj, rsUView, rsUOutlineProj, rsUOutlineView, rsTerrainProg, rsOutlineProg}
    } <-
    askEnv
  cam <- liftIO $ readIORef envCamRef
  aspect <- liftIO $ readIORef envAspectRef
  let projM = perspective (realToFrac (pi / 3 :: Double)) aspect 0.1 500.0
      Camera camPos front up _ _ = cam
      viewM = lookAt camPos (camPos + front) up
  pMat <- liftIO $ toGLMatrix projM
  vMat <- liftIO $ toGLMatrix viewM
  liftIO $ do
    GL.currentProgram $= Just rsTerrainProg
    GL.uniform rsUProj $= pMat
    GL.uniform rsUView $= vMat
    GL.currentProgram $= Just rsOutlineProg
    GL.uniform rsUOutlineProj $= pMat
    GL.uniform rsUOutlineView $= vMat
    GL.currentProgram $= Just rsTerrainProg

main :: IO ()
main = do
  win <- initializeWindow
  aspect0 <- getWindowAspectRatio win
  aspectRef <- newIORef aspect0
  setupOpenGL win aspectRef

  (terrainProg, uniforms) <- setupTerrainShader
  (skyProg, skyVAO, skyVBO) <- setupSkyShader
  (outlineProg, outlineVAO, outlineVBO, uOutlineView, uOutlineProj) <- setupOutlineShader
  (uiProg, uiTex, uiVAO, uiVBO, uUiTex, uUiAspect) <- setupUIShader
  GL.currentProgram $= Just terrainProg

  let (uView, uProj, uFogColor, uFogStart, uFogEnd, _, uTime, _, _, _) = uniforms
  cmRef <- initializeGameState C.fogStart C.fogEnd C.fogColor uFogStart uFogEnd uFogColor
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
            envAspectRef = aspectRef,
            envRender =
              RenderState
                { rsTerrainProg = terrainProg,
                  rsUView = uView,
                  rsUProj = uProj,
                  rsUFogColor = uFogColor,
                  rsUFogStart = uFogStart,
                  rsUFogEnd = uFogEnd,
                  rsUTime = uTime,
                  rsSkyProg = skyProg,
                  rsSkyVAO = skyVAO,
                  rsSkyVBO = skyVBO,
                  rsOutlineProg = outlineProg,
                  rsOutlineVAO = outlineVAO,
                  rsOutlineVBO = outlineVBO,
                  rsUOutlineView = uOutlineView,
                  rsUOutlineProj = uOutlineProj,
                  rsUIProg = uiProg,
                  rsUITex = uiTex,
                  rsUIVAO = uiVAO,
                  rsUIVBO = uiVBO,
                  rsUUiTex = uUiTex,
                  rsUUiAspect = uUiAspect
                }
          }

  let drawFrame :: AppM ()
      drawFrame = do
        Env {envPlayerRef} <- askEnv
        Player ppos _ <- liftIO $ readIORef envPlayerRef
        processInput
        liftIO clearFrame
        drawSkyM
        updateProjViewAllM
        currentTime <- liftIO getCurrentTime
        Env {envRender = RenderState {rsUTime}} <- askEnv
        liftIO $ GL.uniform rsUTime $= (realToFrac currentTime :: GL.GLfloat)
        Env {envCMRef} <- askEnv
        cmState0 <- liftIO $ readIORef envCMRef
        cmState1 <- liftIO $ execChunkManager defaultChunkManagerConfig cmState0 $ do
          updatePlayerPosition ppos
          updateChunks
        liftIO $ writeIORef envCMRef cmState1
        let chunksDraw = cmsLoadedChunks cmState1
        liftIO $ drawWorldOpaque chunksDraw
        liftIO $ drawWorldGrassOverlay chunksDraw
        liftIO $ drawWorldLeaves chunksDraw
        liftIO $ drawWorldWater chunksDraw
        Env {envOutlineRef} <- askEnv
        outlineEnabled <- liftIO $ readIORef envOutlineRef
        when outlineEnabled drawBlockOutlineM
        drawCrosshairUIM
        liftIO $ do
          GLFW.swapBuffers win
          GLFW.pollEvents
        updateFpsTitleM

      loopM :: AppM ()
      loopM = do
        drawFrame
        shouldClose <- liftIO $ GLFW.windowShouldClose win
        esc <- liftIO $ GLFW.getKey win GLFW.Key'Escape
        unless (shouldClose || esc == GLFW.KeyState'Pressed) loopM

  runAppM env loopM
  GLFW.terminate

clearFrame :: IO ()
clearFrame = do
  GL.clearColor $= GL.Color4 0.15 0.18 0.22 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

drawWorldOpaque :: ChunkMap -> IO ()
drawWorldOpaque chunks =
  mapM_
    ( \h -> do
        GL.bindVertexArrayObject $= Just (cmVAO (chOpaque h))
        GL.drawArrays GL.Triangles 0 (fromIntegral (cmCount (chOpaque h)))
    )
    (M.elems chunks)

drawWorldWater :: ChunkMap -> IO ()
drawWorldWater chunks = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  mapM_
    ( \h -> do
      GL.bindVertexArrayObject $= Just (cmVAO (chWater h))
      GL.drawArrays GL.Triangles 0 (fromIntegral (cmCount (chWater h)))
    )
    (M.elems chunks)
  GL.blend $= GL.Disabled

drawWorldLeaves :: ChunkMap -> IO ()
drawWorldLeaves chunks = do
  -- Alpha cutoff needs to be set here for transparent textures like leaves draw
  -- while still showing transparent textures in the same mesh behind them
  -- Still do not quite understand the entire logic, but my OpenGL knowledge 
  -- mostly comes from ChatGPT, so that might explain
  GL.blend $= GL.Disabled
  GL.cullFace $= Nothing
  mprog1 <- GL.get GL.currentProgram
  case mprog1 of
    Just prog -> do
      u <- GL.get $ GL.uniformLocation prog "uAlphaCutoff"
      GL.uniform u $= (0.5 :: GL.GLfloat)
    Nothing -> pure ()
  mapM_
    ( \h -> do
        GL.bindVertexArrayObject $= Just (cmVAO (chLeaves h))
        GL.drawArrays GL.Triangles 0 (fromIntegral (cmCount (chLeaves h)))
    )
    (M.elems chunks)
  mprog2 <- GL.get GL.currentProgram
  case mprog2 of
    Just prog -> do
      u <- GL.get $ GL.uniformLocation prog "uAlphaCutoff"
      GL.uniform u $= (0.0 :: GL.GLfloat)
    Nothing -> pure ()
  GL.cullFace $= Just GL.Back
  GL.blend $= GL.Disabled

drawWorldGrassOverlay :: ChunkMap -> IO ()
drawWorldGrassOverlay chunks = do
  GL.blend $= GL.Disabled
  mprog1 <- GL.get GL.currentProgram
  case mprog1 of
    Just prog -> do
      u <- GL.get $ GL.uniformLocation prog "uAlphaCutoff"
      GL.uniform u $= (0.5 :: GL.GLfloat)
    Nothing -> pure ()
  mapM_
    ( \h -> do
        GL.bindVertexArrayObject $= Just (cmVAO (chGrassOverlay h))
        GL.drawArrays GL.Triangles 0 (fromIntegral (cmCount (chGrassOverlay h)))
    )
    (M.elems chunks)
  mprog2 <- GL.get GL.currentProgram
  case mprog2 of
    Just prog -> do
      u <- GL.get $ GL.uniformLocation prog "uAlphaCutoff"
      GL.uniform u $= (0.0 :: GL.GLfloat)
    Nothing -> pure ()

drawCrosshairUIM :: AppM ()
drawCrosshairUIM = do
  Env {envAspectRef, envRender = RenderState {rsUIProg, rsUITex, rsUIVAO, rsUUiTex, rsUUiAspect, rsTerrainProg}} <- askEnv
  liftIO $ do
    GL.depthFunc $= Nothing
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    GL.currentProgram $= Just rsUIProg
    GL.activeTexture $= GL.TextureUnit 1
    GL.textureBinding GL.Texture2D $= Just rsUITex
    GL.uniform rsUUiTex $= GL.TextureUnit 1

    aspect <- readIORef envAspectRef
    GL.uniform rsUUiAspect $= (realToFrac aspect :: GL.GLfloat)

    GL.bindVertexArrayObject $= Just rsUIVAO
    GL.drawArrays GL.TriangleFan 0 4

    GL.bindVertexArrayObject $= Nothing
    GL.currentProgram $= Just rsTerrainProg
    GL.activeTexture $= GL.TextureUnit 0
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Disabled

generateBlockOutlineVertices :: ChunkMap -> V3 Int -> V3 Float -> [Float]
generateBlockOutlineVertices chunkMap blk@(V3 x y z) camPos =
  let blkF@(V3 fx fy fz) = fromIntegral <$> blk

      isBlockSolid pos = let b = blockQueryFromChunkMap chunkMap pos in b /= Air && blockOpaque b

      blockCenter = blkF + V3 0.5 0.5 0.5
      toCam = normalize (camPos - blockCenter)
      faceFacingCamera normal = toCam `dot` normal > 0

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

      showBottom = not (isBlockSolid (V3 x y (z - 1))) && faceFacingCamera bottomN
      showTop = not (isBlockSolid (V3 x y (z + 1))) && faceFacingCamera topN
      showLeft = not (isBlockSolid (V3 (x - 1) y z)) && faceFacingCamera leftN
      showRight = not (isBlockSolid (V3 (x + 1) y z)) && faceFacingCamera rightN
      showFront = not (isBlockSolid (V3 x (y - 1) z)) && faceFacingCamera frontN
      showBack = not (isBlockSolid (V3 x (y + 1) z)) && faceFacingCamera backN

      bottomEdges = if showBottom then loopEdges [p100, p000, p010, p110] else []
      topEdges = if showTop then loopEdges [p001, p101, p111, p011] else []
      leftEdges = if showLeft then loopEdges [p000, p001, p011, p010] else []
      rightEdges = if showRight then loopEdges [p100, p101, p111, p110] else []
      frontEdges = if showFront then loopEdges [p000, p001, p101, p100] else []
      backEdges = if showBack then loopEdges [p010, p011, p111, p110] else []
   in bottomEdges ++ topEdges ++ leftEdges ++ rightEdges ++ frontEdges ++ backEdges

drawBlockOutlineM :: AppM ()
drawBlockOutlineM = do
  Env
    { envCamRef,
      envCMRef,
      envRender = RenderState {rsOutlineProg, rsOutlineVAO, rsOutlineVBO, rsTerrainProg}
    } <-
    askEnv
  Camera camPos camFront _ _ _ <- liftIO $ readIORef envCamRef
  cm <- liftIO $ readIORef envCMRef
  let chunks = cmsLoadedChunks cm
  case raycastBlockData chunks camPos camFront C.interactionDistance of
    Nothing -> pure ()
    Just (hitPos, _) -> liftIO $ do
      let vertices = generateBlockOutlineVertices chunks hitPos camPos
      GL.currentProgram $= Just rsOutlineProg
      GL.bindVertexArrayObject $= Just rsOutlineVAO
      GL.bindBuffer GL.ArrayBuffer $= Just rsOutlineVBO
      withArray vertices $ \ptr -> do
        let bytes = (fromIntegral (length vertices * 4) :: GL.GLsizeiptr)
        GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (0 :: GL.GLintptr) bytes ptr
      GL.depthFunc $= Nothing
      GL.polygonMode $= (GL.Line, GL.Line)
      GL.lineWidth $= 2.0
      GL.drawArrays GL.Lines 0 (fromIntegral (length vertices `div` 3))
      GL.polygonMode $= (GL.Fill, GL.Fill)
      GL.depthFunc $= Just GL.Lequal
      GL.currentProgram $= Just rsTerrainProg
      GL.bindVertexArrayObject $= Nothing

updateFpsTitleM :: AppM ()
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

drawSkyM :: AppM ()
drawSkyM = do
  Env {envRender = RenderState {rsSkyProg, rsSkyVAO, rsSkyVBO, rsTerrainProg}} <- askEnv
  liftIO $ do
    GL.depthFunc $= Nothing
    GL.currentProgram $= Just rsSkyProg
    GL.bindVertexArrayObject $= Just rsSkyVAO
    GL.bindBuffer GL.ArrayBuffer $= Just rsSkyVBO
    GL.drawArrays GL.Triangles 0 6
    GL.bindVertexArrayObject $= Nothing
    GL.currentProgram $= Just rsTerrainProg
    GL.depthFunc $= Just GL.Lequal
