#version 330 core
in vec3 vTC;
in vec2 vClimate;
in float vFogDist;
uniform sampler2DArray uAtlas;
uniform sampler2D uGrassColormap;
uniform sampler2D uFoliageColormap;
uniform float uAlphaCutoff;
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

  if (texCoord.z == 0.0) {
    float temperature = clamp(vClimate.x, 0.0, 1.0);
    float humidity = clamp(vClimate.y, 0.0, 1.0);
    float u = 1.0 - temperature;
    float v = 1.0 - (humidity * temperature);
    vec3 grassTint = texture(uGrassColormap, vec2(u, v)).rgb;
    base.rgb *= grassTint;
  }

  if (texCoord.z == 23.0) {
    float temperature = clamp(vClimate.x, 0.0, 1.0);
    float humidity = clamp(vClimate.y, 0.0, 1.0);
    float u = 1.0 - temperature;
    float v = 1.0 - (humidity * temperature);
    vec3 foliageTint = texture(uFoliageColormap, vec2(u, v)).rgb;
    base.rgb *= foliageTint;
  }

  if (texCoord.z == 4.0) {
    vec4 overlay = texture(uAtlas, texCoord);
    
    float grassIntensity = overlay.r;
    
    if (grassIntensity < 0.1) discard;
    
    // Apply grass colormap tinting
    float temperature = clamp(vClimate.x, 0.0, 1.0);
    float humidity = clamp(vClimate.y, 0.0, 1.0);
    float u = 1.0 - temperature;
    float v = 1.0 - (humidity * temperature);
    vec3 grassTint = texture(uGrassColormap, vec2(u, v)).rgb;
    
    base = vec4(grassTint * grassIntensity, 1.0);
  }

  if (base.a < uAlphaCutoff) discard;
  float fog = clamp((vFogDist - uFogStart) / (uFogEnd - uFogStart), 0.0, 1.0);
  vec3 rgb = mix(base.rgb, uFogColor, fog);
  FragColor = vec4(rgb, base.a);
}
