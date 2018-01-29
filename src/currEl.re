let currElVertex = {|
    precision mediump float;
    attribute vec2 position;
    uniform vec2 translation;
    varying vec2 vPosition;
    uniform mat3 layout;
    void main() {
        vPosition = position + translation;
        vec3 transformed = vec3(vPosition, 1.0) * layout;
        gl_Position = vec4(transformed.xy, 0.0, 1.0);
    }
|};
let currElFragment = {|
    precision mediump float;
    varying vec2 vPosition;

    uniform vec3 elColor;
    uniform sampler2D sdfTiles;

    void main() {
        // To texture coords
        vec2 sdfPos = (vPosition + 1.0) * 0.5;
        vec3 sdfColor = texture2D(sdfTiles, sdfPos).xyz;
        float sdfCoef = abs(0.5 - sdfColor.x) * 0.9;
        float tileCoef = 1.0 - sdfCoef;
        vec3 color = elColor * tileCoef + sdfColor * sdfCoef;
        gl_FragColor = vec4(color, 1.0);
    }
|};

open Gpu;

let makeNode = (sdfTiles, color, elPos, vertices, indices) => {
    Scene.makeNode(
        "currEl",
        ~updateOn=[UpdateFlags.ElPosChanged],
        ~vertShader=Shader.make(currElVertex),
        ~fragShader=Shader.make(currElFragment),
        ~vertices,
        ~indices,
        ~uniforms=[
            ("elColor", color),
            ("translation", elPos),
            ("layout", Scene.UMat3f.id())
        ],
        ~textures=[
            ("sdfTiles", sdfTiles)
        ],
        ()
    )
};