let currElVertex = {|
    precision mediump float;
    attribute vec2 position;
    uniform vec2 translation;
    uniform mat3 layout;
    uniform mat3 sdfTilesMat;
    varying vec2 vPosition;
    varying vec2 vTexPos;
    void main() {
        vPosition = position + translation;
        vec3 transformed = vec3(vPosition, 1.0) * layout;
        vTexPos = (vec3(vPosition, 1.0) * sdfTilesMat).xy;
        gl_Position = vec4(transformed.xy, 0.0, 1.0);
    }
|};
let currElFragment = {|
    precision mediump float;
    varying vec2 vPosition;
    varying vec2 vTexPos;

    uniform vec3 elColor;
    uniform sampler2D sdfTiles;

    void main() {
        // To texture coords
        vec3 sdfColor = texture2D(sdfTiles, vTexPos).xyz;
        float sdfCoef = abs(0.5 - sdfColor.x) * 0.9;
        float tileCoef = 1.0 - sdfCoef;
        vec3 color = elColor * tileCoef + sdfColor * sdfCoef;
        gl_FragColor = vec4(color, 1.0);
    }
|};

open Gpu;

let makeNode = (color, elPos, vo, sdfTiles) => {
    Scene.makeNode(
        "currEl",
        ~updateOn=[UpdateFlags.ElPosChanged],
        ~vertShader=Shader.make(currElVertex),
        ~fragShader=Shader.make(currElFragment),
        ~vo,
        ~uniforms=[
            ("elColor", color),
            ("translation", elPos)
        ],
        ~textures=[
            ("sdfTiles", Scene.NodeTex.node(sdfTiles))
        ],
        ()
    )
};