#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aTC;
uniform mat4 uView;
uniform mat4 uProj;
out vec3 vTC;
out float vFogDist;
void main(){
  vTC = aTC;
  vec4 posVS = uView * vec4(aPos, 1.0);
  vFogDist = length(posVS.xyz);
  gl_Position = uProj * posVS;
}
