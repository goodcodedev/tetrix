let vertSource = {|
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
|};

let fragSource = {|
    precision mediump float;
    varying vec3 vPos;
    varying vec3 vNormal;
    void main() {
        gl_FragColor = vec4(vNormal, 1.0);
    }
|};

let make = (vo, ~updateOn, ()) => {
    Scene.makeNode(
        "node3d",
        ~updateOn,
        ~vertShader=Gpu.Shader.make(vertSource),
        ~fragShader=Gpu.Shader.make(fragSource),
        ~vo,
        ()
    );
};