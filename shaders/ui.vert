#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aUV;
uniform float uAspect;
out vec2 vUV;
void main(){
  vUV = aUV;
  float invAspect = (uAspect > 0.0) ? (1.0 / uAspect) : 1.0;
  vec2 pos = vec2(aPos.x * invAspect, aPos.y);
  gl_Position = vec4(pos, 0.0, 1.0);
}
