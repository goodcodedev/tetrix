let vertSource = () => {
    {|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat3 layout;
        varying vec3 vPos;
        varying vec3 vNormal;
        void main() {
            // Z currently not affected by layout
            vec3 pos2 = vec3((vec3(position.xy, 1.0) * layout).xy, position.z);
            vPos = position;
            vNormal = normal;
            gl_Position = vec4(pos2, 1.0);
        }
    |}
};

let fragSource = (light) => {
    let lightDecls = Light.ProgramLight.getFragVarDecls(light);
    let lightSrc = Light.ProgramLight.getLightFunction(light);
    {|
        precision mediump float;
        varying vec3 vPos;
        varying vec3 vNormal;
        |} ++ lightDecls ++ {|
        
        |} ++ lightSrc ++ {|

        void main() {
            vec3 color = vec3(1.0, 1.0, 1.0) * lighting(vPos);
            gl_FragColor = vec4(color, 0.2);
        }
    |}
};

let make = (
    vo,
    ~updateOn,
    ~size=Scene.Dimensions(Scene.Scale(1.0), Scene.Scale(1.0)),
    ~light,
    ()
) => {
    let vs = vertSource();
    let fs = fragSource(light);
    Scene.makeNode(
        "node3d",
        ~updateOn,
        ~size,
        ~transparent=true,
        ~vertShader=Gpu.Shader.make(vs),
        ~fragShader=Gpu.Shader.make(fs),
        ~vo,
        ()
    );
};