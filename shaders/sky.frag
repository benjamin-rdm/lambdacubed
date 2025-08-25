#version 330 core
in float vY;
uniform vec3 uTopColor;
uniform vec3 uHorizonColor;
out vec4 FragColor;
void main(){
  float t = clamp(vY * 0.5 + 0.5, 0.0, 1.0);
  vec3 c = mix(uHorizonColor, uTopColor, t);
  FragColor = vec4(c, 1.0);
}

