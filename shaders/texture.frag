#version 330 core

in vec3 vTC;

uniform sampler2DArray uAtlas;

out vec4 FragColor;

void main(){
  FragColor = texture(uAtlas, vTC);
}
