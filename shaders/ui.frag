#version 330 core
in vec2 vUV;
uniform sampler2D uUiTex;
out vec4 FragColor;
void main(){
  vec4 c = texture(uUiTex, vUV);
  if (c.a <= 0.0) discard;
  FragColor = c;
}
