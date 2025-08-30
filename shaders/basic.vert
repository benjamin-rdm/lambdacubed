#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aTC;
layout (location = 2) in vec2 aClimate; // (temperature, humidity) as v2
uniform mat4 uView;
uniform mat4 uProj;
out vec3 vTC;
out vec2 vClimate;
out float vFogDist;
void main(){
  vTC = aTC;
  vClimate = aClimate;
  vec4 posVS = uView * vec4(aPos, 1.0);
  vFogDist = length(posVS.xyz);
  gl_Position = uProj * posVS;
}
