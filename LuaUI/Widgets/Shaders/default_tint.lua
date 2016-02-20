
return
{
  vertex = [[
    uniform vec3 cameraPos;
    #if (USE_SHADOWS == 1)
      uniform mat4 shadowMatrix;
      uniform vec4 shadowParams;
    #endif

      varying vec4 vertexWorldPos;
      varying vec3 cameraDir;
      varying float fogFactor;

    #ifdef use_normalmapping
      varying mat3 tbnMatrix;
    #else
      varying vec3 normalv;
    #endif

    uniform int numModelDynLights;


    void main(void)
    {
    #ifdef use_normalmapping
      vec3 tangent   = gl_MultiTexCoord5.xyz;
      vec3 bitangent = gl_MultiTexCoord6.xyz;
      tbnMatrix      = gl_NormalMatrix * mat3(tangent, bitangent, gl_Normal);
    #else
      normalv = gl_NormalMatrix * gl_Normal;
    #endif

      gl_ClipVertex  = gl_ModelViewMatrix * gl_Vertex; // M (!)
      gl_Position    = gl_ProjectionMatrix * gl_ClipVertex;

      vertexWorldPos = gl_ClipVertex;
      cameraDir      = vertexWorldPos.xyz - cameraPos;

    #if (USE_SHADOWS == 1)
      gl_TexCoord[1] = shadowMatrix * vertexWorldPos;
      gl_TexCoord[1].st = gl_TexCoord[1].st * (inversesqrt( abs(gl_TexCoord[1].st) + shadowParams.z) + shadowParams.w) + shadowParams.xy;
    #endif

      gl_TexCoord[0].st = gl_MultiTexCoord0.st;

      #if (DEFERRED_MODE == 0)
      float fogCoord = length(cameraDir.xyz);
      fogFactor = (gl_Fog.end - fogCoord) * gl_Fog.scale; //gl_Fog.scale := 1.0 / (gl_Fog.end - gl_Fog.start)
      fogFactor = clamp(fogFactor, 0.0, 1.0);
      #endif
    }
  ]],
  fragment= [[
  //#define use_normalmapping
  //#define flip_normalmap
  //#define use_shadows
  //#define flipAlpha
  //#define tex1Alpha

    uniform sampler2D textureS3o1;
    uniform sampler2D textureS3o2;
    uniform samplerCube specularTex;
    uniform samplerCube reflectTex;

    uniform vec3 sunDir;
    uniform vec3 sunDiffuse;
    uniform vec3 sunAmbient;

  #ifdef use_shadows
    uniform sampler2DShadow shadowTex;
    uniform float shadowDensity;
  #endif

    uniform vec4 teamColor;
    uniform vec3 tint;
    uniform float alphaPass;

    varying vec4 vertexWorldPos;
    varying vec3 cameraDir;
    varying float fogFactor;

  #ifdef use_normalmapping
    uniform sampler2D normalMap;
    varying mat3 tbnMatrix;
  #else
    varying vec3 normalv;
  #endif

  float GetShadowCoeff(vec4 shadowCoors)
  {
     #ifdef use_shadows
     float coeff = shadow2DProj(shadowTex, shadowCoors).r;

     coeff  = (1.0 - coeff);
     coeff *= shadowDensity;
     return (1.0 - coeff);
     #else
     return 1.0;
     #endif
  }

  void main(void)
  {
  #ifdef use_normalmapping
     vec2 tc = gl_TexCoord[0].st;
     #ifdef flip_normalmap
        tc.t = 1.0 - tc.t;
     #endif
     vec3 nvTS  = normalize((texture2D(normalMap, tc).xyz - 0.5) * 2.0);
     vec3 normal = tbnMatrix * nvTS;
  #else
     vec3 normal = normalize(normalv);
  #endif

     vec3 light = max(dot(normal, sunDir), 0.0) * sunDiffuse + sunAmbient;

     vec4 diffuse     = texture2D(textureS3o1, gl_TexCoord[0].st);
     vec4 extraColor  = texture2D(textureS3o2, gl_TexCoord[0].st);

     vec3 reflectDir = reflect(cameraDir, normal);
     vec3 specular   = textureCube(specularTex, reflectDir).rgb * extraColor.g * 4.0;
     vec3 reflection = textureCube(reflectTex,  reflectDir).rgb;

     float shadow = GetShadowCoeff(gl_TexCoord[1] + vec4(0.0, 0.0, -0.00005, 0.0));

     // no highlights if in shadow; decrease light to ambient level
     specular *= shadow;
     light = mix(sunAmbient, light, shadow);


     reflection  = mix(light, reflection, extraColor.g); // reflection
     reflection += extraColor.rrr; // self-illum

     gl_FragColor     = diffuse;
     gl_FragColor.rgb = mix(gl_FragColor.rgb, teamColor.rgb, gl_FragColor.a); // teamcolor
     gl_FragColor.rgb = gl_FragColor.rgb * reflection + specular;
     gl_FragColor.rgb *= tint;

     //gl_FragColor.rgb = mix(gl_Fog.color.rgb, gl_FragColor.rgb, fogFactor); // fog
     gl_FragColor.a   = extraColor.a * teamColor.a;
  }

  ]],
  uniformInt = {
      textureS3o1 = 0,
      textureS3o2 = 1,
      shadowTex   = 2,
      specularTex = 3,
      reflectTex  = 4,
      normalMap   = 5,
      --detailMap   = 6,
  },
  uniform = {
      sunPos = {gl.GetSun("pos")},
      sunAmbient = {gl.GetSun("ambient", "unit")},
      sunDiffuse = {gl.GetSun("diffuse", "unit")},
      shadowDensity = {gl.GetSun("shadowDensity" ,"unit")},
      shadowParams  = {gl.GetShadowMapParams()},
  },
  uniformMatrix = {
      shadowMatrix = {gl.GetMatrixData("shadow")},
  }
}
