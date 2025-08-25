#version 330 core
in vec3 vTC;
in float vFogDist;
uniform sampler2DArray uAtlas;
uniform vec3 uFogColor;
uniform float uFogStart;
uniform float uFogEnd;
uniform float uTime;
out vec4 FragColor;
void main(){
  vec3 texCoord = vTC;
  
  // Animate water texture (layers 5-20 are water frames)
  if(texCoord.z == 5.0) {
    float animSpeed = 4.0; // 4 FPS
    int frameCount = 16;
    int currentFrame = int(mod(uTime * animSpeed, float(frameCount)));
    texCoord.z = 5.0 + float(currentFrame);
  }
  
  vec4 base = texture(uAtlas, texCoord);
  float fog = clamp((vFogDist - uFogStart) / (uFogEnd - uFogStart), 0.0, 1.0);
  vec3 rgb = mix(base.rgb, uFogColor, fog);
  FragColor = vec4(rgb, base.a);
}

