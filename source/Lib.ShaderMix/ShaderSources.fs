module internal ShaderSources
  let vertexShader =
    """#version 300 es

precision highp float;

in vec4 a_position;
in vec2 a_texcoord;

out vec2 v_texcoord;

void main() {
  gl_Position = a_position;
  v_texcoord = a_texcoord;
}
"""

  let fragmentShaderSourcePrelude =
    """#version 300 es

precision highp float;

uniform float iTime;
uniform vec2 iResolution;
uniform sampler2D iChannel0;  
uniform sampler2D iChannel1;  
uniform sampler2D iChannel2;  
uniform sampler2D iChannel3;  

in vec2 v_texcoord;

out vec4 f_fragColor;

void mainImage(out vec4 fragColor, in vec2 fragCoord);

void main() {
  mainImage(f_fragColor, gl_FragCoord.xy);
}

"""

  let fragmentShaderSimplePresenter = """
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  vec2 q = fragCoord/iResolution.xy;
  q.y = 1.-q.y;
  fragColor = texture(iChannel0, q);
}
"""