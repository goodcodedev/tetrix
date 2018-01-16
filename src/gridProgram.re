let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        gl_Position = vec4(position, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    uniform vec3 lineColor;
    uniform vec2 elPos;
    uniform vec3 elColor;
    uniform sampler2D tiles;

    varying vec2 vPosition;

    const float numCols = 14.0;
    const float numRows = 28.0;
    const float xsize = 1.0 / 280.0 * 1.0;
    const float ysize = 1.0 / 560.0 * 1.0;
    const float tileWidth = 1.0 / numCols;
    const float tileHeight = 1.0 / numRows;

    float shadowCoef(vec2 tilePos) {
        float tileXOffset = mod(tilePos.x, tileWidth);
        float tileYOffset = mod(tilePos.y, tileHeight);
        // Sample neighbour tiles
        vec4 tileL = texture2D(tiles, tilePos + vec2(-tileWidth, 0.0));
        vec4 tileTL = texture2D(tiles, tilePos + vec2(-tileWidth, -tileHeight));
        vec4 tileT = texture2D(tiles, tilePos + vec2(0.0, -tileHeight));
        vec4 tileTR = texture2D(tiles, tilePos + vec2(tileWidth, -tileHeight));
        vec4 tileR = texture2D(tiles, tilePos + vec2(tileWidth, 0.0));
        vec4 tileBR = texture2D(tiles, tilePos + vec2(tileWidth, tileHeight));
        vec4 tileB = texture2D(tiles, tilePos + vec2(0.0, tileHeight));
        vec4 tileBL = texture2D(tiles, tilePos + vec2(-tileWidth, tileHeight));
        // Find closest distance. 1.0 signifies no value
        float tileLDist = tileL.x > 0.0 ? tileXOffset : 1.0;
        float tileTLDist = tileTL.x > 0.0 ? length(vec2(tileXOffset, tileYOffset)) : 1.0;
        float tileTDist = tileT.x > 0.0 ? tileYOffset : 1.0;
        float tileTRDist = tileTR.x > 0.0 ? length(vec2(tileWidth - tileXOffset, tileYOffset)) : 1.0;
        float tileRDist = tileR.x > 0.0 ? tileWidth - tileXOffset : 1.0;
        float tileBRDist = tileBR.x > 0.0 ? length(vec2(tileWidth - tileXOffset, tileHeight - tileYOffset)) : 1.0;
        float tileBDist = tileB.x > 0.0 ? tileHeight - tileYOffset : 1.0;
        float tileBLDist = tileBL.x > 0.0 ? length(vec2(tileXOffset, tileHeight - tileYOffset)) : 1.0;
        float tileDistance = min(
            tileLDist,
            min(tileTLDist,
                min(tileTDist,
                    min(tileTRDist,
                        min(tileRDist,
                            min(tileBRDist,
                                min(tileBDist, tileBLDist)
                            )
                        )
                    )
                )
            )
        );
        float shadowCoef = (tileDistance < 1.0) ? (tileWidth - tileDistance) / tileWidth : 0.0;
        return shadowCoef / 16.0;
    }

    void main() {
        vec2 aspect = vec2(numCols / numRows, 1.0);
        // Normalized coord to 0.0 - 1.0
        vec2 coord = (vPosition + 1.0) * 0.5;
        float xblank = (mod(coord.x + xsize / 1.0, 1.0 / numCols) > xsize) ? 1.0 : 0.0;
        float yblank = (mod(coord.y + ysize / 1.0, 1.0 / numRows) > ysize) ? 1.0 : 0.0;
        float alpha = 1.0 - xblank * yblank;
        vec3 bg = vec3(0.04, 0.1, 0.2);
        vec2 elVec = vPosition - (elPos + vec2(-0.03, 0.1));
        vec2 elVecNorm = normalize(elVec);
        // Roughly light a triangle below element
        float dir = dot(elVecNorm, vec2(0.0, -1.0));
        float lengthCoef = max(1.5 - length(elVec), 0.0);
        float light = smoothstep(-0.3, 0.5, dir) * lengthCoef * 0.05;
        float colorLight = max(0.35 - length(elVec * aspect), 0.0) * 0.3;
        // Add some shadow from neighbour tiles
        // Texture coord system.
        vec2 tilePos = vec2(coord.x, (coord.y * -1.0) + 1.0);
        float shadow = shadowCoef(tilePos);
        // Let shadow fall below line
        vec3 color = (1.0 - alpha) * bg + lineColor * alpha;
        color = mix(color, vec3(1.0, 1.0, 1.0), shadow);
        color = color + elColor * colorLight;
        gl_FragColor = vec4(color + light, 1.0);
    }
|};

open Gpu;
let createProgram = () => {
    Program.make(
        Shader.make(vertexSource),
        Shader.make(fragmentSource),
        [|
            Uniform.make("lineColor", GlType.Vec3f),
            Uniform.make("elPos", GlType.Vec2f),
            Uniform.make("elColor", GlType.Vec3f),
        |]
    )
};

let createDrawState = (canvas : Canvas.t, tilesTexture) => {
    DrawState.init(
        canvas.context,
        createProgram(),
        [|
            Uniform.UniformVec3f([|0.0, 0.0, 0.0|]),
            Uniform.UniformVec2f([|0.0, 0.0|]),
            Uniform.UniformVec3f([|0.0, 0.0, 0.0|])
        |],
        VertexBuffer.makeQuad(),
        IndexBuffer.makeQuad(),
        [|
            ProgramTexture.make("tiles", tilesTexture)
        |]
    )
};

let draw = (ds, canvas) => {
    DrawState.draw(ds, canvas);
};