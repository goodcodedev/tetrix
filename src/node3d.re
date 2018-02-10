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

    float lighting(vec3 p) {
        vec3 diffuseDir = vec3(0.4, 0.3, 0.3);
        float NdotD = max(dot(diffuseDir, vNormal), 0.0);
        vec3 pointPos = vec3(-2.8, 0.3, 2.0);
        vec3 pointVec = pointPos - p;
        vec3 pointDir = normalize(pointVec);
        float NdotP = max(dot(pointDir, vNormal), 0.0);
        float c = NdotD * 0.8 + NdotP * 0.6;
        return c;
    }

    void main() {
        vec3 color = vec3(1.0, 1.0, 1.0) * lighting(vPos);
        gl_FragColor = vec4(color, 0.2);
    }
|};

let make = (
    vo,
    ~updateOn,
    ~size=Scene.Dimensions(Scene.Scale(1.0), Scene.Scale(1.0)),
    ()
) => {
    Scene.makeNode(
        "node3d",
        ~updateOn,
        ~size,
        ~transparent=true,
        ~vertShader=Gpu.Shader.make(vertSource),
        ~fragShader=Gpu.Shader.make(fragSource),
        ~vo,
        ()
    );
};