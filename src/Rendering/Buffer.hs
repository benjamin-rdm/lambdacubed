module Rendering.Buffer
  ( Buffer (..),
    createBufferWithVertices,
    createDynamicBuffer,
    deleteBuffer,
    bufferSubDataFloats,
    drawBuffer,
    drawBufferAs,
    drawBufferCount,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL

data Buffer = Buffer
  { bufVAO :: !GL.VertexArrayObject,
    bufVBO :: !GL.BufferObject,
    bufCount :: !Int
  }

createBufferWithVertices :: [Float] -> GL.NumComponents -> IO Buffer
createBufferWithVertices vertices componentCount = do
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

  pure (Buffer vao vbo (length vertices `div` fromIntegral componentCount))

createDynamicBuffer :: Int -> GL.BufferUsage -> IO Buffer
createDynamicBuffer floatCapacity usage = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  GL.bufferData GL.ArrayBuffer $= (fromIntegral (floatCapacity * 4), nullPtr, usage)
  GL.bindVertexArrayObject $= Nothing
  pure (Buffer vao vbo 0)

deleteBuffer :: Buffer -> IO ()
deleteBuffer (Buffer vao vbo _) = do
  GL.deleteObjectName vbo
  GL.deleteObjectName vao

bufferSubDataFloats :: (MonadIO m) => Buffer -> [Float] -> m ()
bufferSubDataFloats bm floats = liftIO $ do
  GL.bindBuffer GL.ArrayBuffer $= Just (bufVBO bm)
  withArray floats $ \ptr -> do
    let bytes = fromIntegral (length floats * 4) :: GL.GLsizeiptr
    GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (0 :: GL.GLintptr) bytes ptr

drawBufferAs :: (MonadIO m) => GL.PrimitiveMode -> Buffer -> m ()
drawBufferAs mode bm = liftIO $ do
  GL.bindVertexArrayObject $= Just (bufVAO bm)
  GL.drawArrays mode 0 (fromIntegral (bufCount bm))

drawBufferCount :: (MonadIO m) => GL.PrimitiveMode -> Int -> Buffer -> m ()
drawBufferCount mode count bm = liftIO $ do
  GL.bindVertexArrayObject $= Just (bufVAO bm)
  GL.drawArrays mode 0 (fromIntegral count)

drawBuffer :: (MonadIO m) => Buffer -> m ()
drawBuffer = drawBufferAs GL.Triangles
