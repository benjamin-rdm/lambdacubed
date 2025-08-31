module Rendering.Shader.Utils
  ( loadProgramFromSources,
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

loadProgramFromSources :: BS8.ByteString -> BS8.ByteString -> IO GL.Program
loadProgramFromSources vertSrc fragSrc = do
  putStrLn "GLSL Vertex Shader"
  BS8.putStrLn vertSrc
  putStrLn "GLSL Fragment Shader"
  BS8.putStrLn fragSrc
  vs <- compileShader GL.VertexShader vertSrc
  fs <- compileShader GL.FragmentShader fragSrc
  linkShaderProgram [vs, fs]
