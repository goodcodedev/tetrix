let vertSource = () => {|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat3 layout;
        varying vec3 vPos;
        varying vec3 vScreenPos;
        varying vec3 vNormal;
        void main() {
            // Z currently not affected by layout
            vec3 pos2 = vec3((vec3(position.xy, 1.0) * layout).xy, position.z);
            vPos = position;
            vScreenPos = pos2;
            vNormal = normal;
            gl_Position = vec4(pos2, 1.0);
        }
    |};

let fragSource = light => {
  let lightDecls = Light.ProgramLight.getFragVarDecls(light);
  let lightSrc = Light.ProgramLight.getLightFunction(light);
  {|
        precision mediump float;
        varying vec3 vPos;
        varying vec3 vScreenPos;
        varying vec3 vNormal;
        |}
  ++ lightDecls
  ++ {|

        |}
  ++ lightSrc
  ++ {|

        void main() {
            vec3 color = lighting(vPos, vScreenPos, vNormal);
            gl_FragColor = vec4(color, 0.2);
        }
    |};
};

let make =
    (
      vo,
      ~size=Scene.Dimensions(Scene.Scale(1.0), Scene.Scale(1.0)),
      ~light,
      ()
    ) => {
  let vs = vertSource();
  let fs = fragSource(light);
  Scene.makeNode(
    ~cls="node3d",
    ~size,
    ~uniforms=Light.ProgramLight.getUniforms(light),
    ~transparent=true,
    ~vertShader=Gpu.Shader.make(vs),
    ~fragShader=Gpu.Shader.make(fs),
    ~vo,
    ()
  );
};
