{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rendering.Buffer
  ( Buffer (..),
    createDynamicBufferFor,
    floatsPerVertex,
    deleteBuffer,
    bufferSubDataForVertices,
    drawBuffer,
    drawBufferAs,
    drawBufferCount,
    bufferWithVerticesFor,
    Vertex (..)
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_, Foldable(..), toList, foldlM)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (nullPtr, plusPtr)
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal)
import GHC.TypeNats (type (+))
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST (Datatype)
import Rendering.Shader.Typed (Len)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM

data Buffer (is :: [(Symbol, Datatype)]) = Buffer
  { bufVAO :: !GL.VertexArrayObject,
    bufVBO :: !GL.BufferObject,
    bufCount :: !Int
  }

class Vertex a where
  toFloats :: a -> VS.Vector Float

instance Foldable t => Vertex (t Float) where
  toFloats :: Foldable t => t Float -> VS.Vector Float
  toFloats = VS.fromList . toList

bufferWithVerticesFor :: forall is f v. (KnownVertexLayout is, KnownNat (TotalComponents is), Foldable f, Vertex v) => f v -> IO (Buffer is)
bufferWithVerticesFor vertices = do
  let vertexCount = length vertices
  buf <- createDynamicBufferFor @is vertexCount GL.StaticDraw
  count <- bufferSubDataForVertices @is buf vertices
  pure buf {bufCount = count}

type family TotalComponents (is :: [(Symbol, Datatype)]) :: Nat where
  TotalComponents '[] = 0
  TotalComponents ('(name, t) ': rest) = Len t + TotalComponents rest

class KnownVertexLayout (is :: [(Symbol, Datatype)]) where
  vertexComponents :: Proxy is -> [Int]

instance KnownVertexLayout '[] where
  vertexComponents :: Proxy '[] -> [Int]
  vertexComponents _ = []

instance (KnownNat (Len t), KnownVertexLayout rest) => KnownVertexLayout ('(name, t) ': rest) where
  vertexComponents :: (KnownNat (Len t), KnownVertexLayout rest) => Proxy ('(name, t) : rest) -> [Int]
  vertexComponents _ = fromInteger (natVal (Proxy @(Len t))) : vertexComponents (Proxy @rest)

floatsPerVertex :: forall is. KnownNat (TotalComponents is) => Int
floatsPerVertex = fromInteger (natVal (Proxy @(TotalComponents is)))

setupVertexLayout :: forall is. KnownVertexLayout is => Proxy is -> IO ()
setupVertexLayout proxy = do
  let floatByteLength = 4
      comps = vertexComponents proxy
      stride :: GL.GLsizei
      stride = fromIntegral (sum comps * floatByteLength)
      offsets = init (scanl (+) 0 comps)
  for_ (zip3 [0 :: Int ..] comps offsets) $ \(loc, comp, offset) -> do
    let ptr = plusPtr nullPtr (offset * floatByteLength)
    GL.vertexAttribPointer (GL.AttribLocation (fromIntegral loc))
      $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral comp) GL.Float stride ptr)
    GL.vertexAttribArray (GL.AttribLocation (fromIntegral loc)) $= GL.Enabled

createDynamicBufferFor :: forall is. (KnownVertexLayout is, KnownNat (TotalComponents is)) => Int -> GL.BufferUsage -> IO (Buffer is)
createDynamicBufferFor vertexCapacity usage = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  let strideFloats = floatsPerVertex @is
      bytes = fromIntegral (vertexCapacity * strideFloats * 4)
  GL.bufferData GL.ArrayBuffer $= (bytes, nullPtr, usage)
  setupVertexLayout (Proxy @is)
  GL.bindVertexArrayObject $= Nothing
  pure (Buffer vao vbo 0)

deleteBuffer :: Buffer is -> IO ()
deleteBuffer (Buffer vao vbo _) = do
  GL.deleteObjectName vbo
  GL.deleteObjectName vao

bufferSubDataFloatsVector :: (MonadIO m) => Buffer is -> VS.Vector Float -> m ()
bufferSubDataFloatsVector bm floatsVec = liftIO $ do
  GL.bindBuffer GL.ArrayBuffer $= Just (bufVBO bm)
  if VS.null floatsVec
    then pure ()
    else
      VS.unsafeWith floatsVec $ \ptr -> do
        let bytes = fromIntegral (VS.length floatsVec * 4) :: GL.GLsizeiptr
        GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (0 :: GL.GLintptr) bytes ptr

bufferSubDataForVertices ::
  forall is m f v.
  ( MonadIO m,
    KnownNat (TotalComponents is),
    Foldable f,
    Vertex v
  ) =>
  Buffer is -> f v -> m Int
bufferSubDataForVertices buf vertices = do
  let floatsPerVert = floatsPerVertex @is
      vertexCount = length vertices
      totalFloats = vertexCount * floatsPerVert
  vertexData <- liftIO $ do
    mv <- VSM.new totalFloats
    finalOffset <- foldlM (writeVertex mv) 0 vertices
    when (finalOffset /= totalFloats) $ -- We want to guarantee this on type level in the future
      error "bufferSubDataForVertices: wrote unexpected number of floats"
    VS.unsafeFreeze mv
  bufferSubDataFloatsVector buf vertexData
  pure vertexCount
  where
    writeVertex mv offset vertex = do
      let vertexVec = toFloats vertex
          len = VS.length vertexVec
      VS.copy (VSM.slice offset len mv) vertexVec
      pure (offset + len)

drawBufferAs :: (MonadIO m) => GL.PrimitiveMode -> Buffer is -> m ()
drawBufferAs mode bm = liftIO $ do
  GL.bindVertexArrayObject $= Just (bufVAO bm)
  GL.drawArrays mode 0 (fromIntegral (bufCount bm))
  GL.bindVertexArrayObject $= Nothing

drawBufferCount :: (MonadIO m) => GL.PrimitiveMode -> Int -> Buffer is -> m ()
drawBufferCount mode count bm = liftIO $ do
  GL.bindVertexArrayObject $= Just (bufVAO bm)
  GL.drawArrays mode 0 (fromIntegral count)
  GL.bindVertexArrayObject $= Nothing

drawBuffer :: (MonadIO m) => Buffer is -> m ()
drawBuffer = drawBufferAs GL.Triangles
