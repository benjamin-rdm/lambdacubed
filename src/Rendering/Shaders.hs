module Rendering.Shaders
  ( loadProgramFromFiles,
    compileShader,
    linkShaderProgram,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL

compileShader :: GL.ShaderType -> BS8.ByteString -> IO GL.Shader
compileShader shaderType source = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $= source
  GL.compileShader shader
  status <- GL.get (GL.compileStatus shader)
  if status
    then pure shader
    else do
      shaderLog <- GL.get (GL.shaderInfoLog shader)
      fail $ "Shader compilation failed: " ++ shaderLog

linkShaderProgram :: [GL.Shader] -> IO GL.Program
linkShaderProgram shaders = do
  program <- GL.createProgram
  mapM_ (GL.attachShader program) shaders
  GL.linkProgram program
  status <- GL.get (GL.linkStatus program)
  if status
    then pure program
    else do
      programLog <- GL.get (GL.programInfoLog program)
      fail $ "Program linking failed: " ++ programLog

loadShaderSource :: FilePath -> IO BS8.ByteString
loadShaderSource = BS8.readFile

loadProgramFromFiles :: FilePath -> FilePath -> IO GL.Program
loadProgramFromFiles vertPath fragPath = do
  vsrc <- loadShaderSource vertPath
  fsrc <- loadShaderSource fragPath
  vs <- compileShader GL.VertexShader vsrc
  fs <- compileShader GL.FragmentShader fsrc
  linkShaderProgram [vs, fs]
