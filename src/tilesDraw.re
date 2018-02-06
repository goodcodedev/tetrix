let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 sdfTilesMat;
    varying vec2 sdfPos;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        vec3 transformed = vec3(position, 1.0) * layout;
        sdfPos = (vec3(position, 1.0) * sdfTilesMat).xy;
        gl_Position = vec4(transformed.xy, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    varying vec2 vPosition;
    varying vec2 sdfPos;

    uniform sampler2D tiles;
    uniform sampler2D sdfTiles;

    const int numCols = 12;
    const int numRows = 26;

    void main() {
        vec2 tilePos = vec2((vPosition.x + 1.0) * 0.5, (vPosition.y - 1.0) * -0.5);
        float tile = texture2D(tiles, tilePos).x;
        int colorIdx = int(tile * 255.0);
        vec3 color =
            (colorIdx == 1) ? vec3(0.5, 0.9, 1.0)
            : (colorIdx == 2) ? vec3(0.45, 0.5, 0.95)
            : (colorIdx == 3) ? vec3(0.95, 0.85, 0.3)
            : (colorIdx == 4) ? vec3(0.95, 1.0, 0.5)
            : (colorIdx == 5) ? vec3(0.55, 0.95, 0.45)
            : (colorIdx == 6) ? vec3(0.7, 0.4, 0.85)
            : (colorIdx == 7) ? vec3(0.9, 0.4, 0.35)
            : vec3(0.8, 0.8, 0.9);
        vec3 sdfColor = texture2D(sdfTiles, sdfPos).xyz;
        float sdfCoef = abs(0.5 - sdfColor.x);
        float tileCoef = 1.0 - sdfCoef;
        //color = color * 0.5 + ((colorIdx == 0) ? vec3(0.0, 0.0, 0.0) : (sdfColor * 0.5));
        gl_FragColor = (colorIdx == 0) ? vec4(0.0, 0.0, 0.0, 0.0) : vec4(color * tileCoef + sdfColor * sdfCoef, 1.0);
    }
|};

open Gpu;

let makeNode = (tilesTex, sdfTiles) => {
    open Scene;
    Scene.makeNode(
        "tilesDraw",
        ~updateOn=[UpdateFlags.TilesChanged, UpdateFlags.ElPosChanged],
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ~uniforms=[],
        ~transparent=true,
        ~partialDraw=true,
        ~textures=[
            ("tiles", NodeTex.tex(tilesTex)),
            ("sdfTiles", NodeTex.node(sdfTiles))
        ],
        ()
    )
};