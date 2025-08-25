#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aTC;

uniform mat4 uView;
uniform mat4 uProj;

out vec3 vTC;

void main(){
  vTC = aTC;
  gl_Position = uProj * uView * vec4(aPos, 1.0);
}
