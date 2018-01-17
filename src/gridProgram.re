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
    uniform sampler2D tileShadows;

    varying vec2 vPosition;

    const float numCols = 14.0;
    const float numRows = 28.0;
    const float xsize = 1.0 / 280.0 * 1.0;
    const float ysize = 1.0 / 560.0 * 1.0;
    const float tileWidth = 1.0 / numCols;
    const float tileHeight = 1.0 / numRows;

    float shadowCoef(vec2 tilePos) {
        // Translate point to "shadow perspective"
        float tileXOffset = mod(tilePos.x, tileWidth);
        float tileYOffset = mod(tilePos.y, tileHeight);
        float xaspect = numCols / numRows;
        float LDist = tileXOffset * xaspect;
        float RDist = (tileWidth - tileXOffset) * xaspect;
        vec4 tile = texture2D(tiles, tilePos);
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
        float coefBase = 1.0 + tileHeight;
        float defaultVal = 1.0;
        float len = (tile.x > 0.0) ? 0.0 : defaultVal;
        float lenL = tileL.x > 0.0 ? LDist : defaultVal;
        float lenTL = tileTL.x > 0.0 ? length(vec2(LDist, tileYOffset)) : defaultVal;
        float lenT = tileT.x > 0.0 ? tileYOffset : defaultVal;
        float lenTR = tileTR.x > 0.0 ? length(vec2(RDist, tileYOffset)) : defaultVal;
        float lenR = tileR.x > 0.0 ? RDist : defaultVal;
        float lenBR = tileBR.x > 0.0 ? length(vec2(RDist, tileHeight - tileYOffset)) : defaultVal;
        float lenB = tileB.x > 0.0 ? (tileHeight - tileYOffset) : defaultVal;
        float lenBL = tileBL.x > 0.0 ? length(vec2(LDist, tileHeight - tileYOffset)) : defaultVal;
        float from = tileHeight / 3.0;
        float to = tileHeight / 3.0;
        // Let adjecent influence
        // Sides
        //float sideLen = ()
        return max(1.0 - len,
                max(1.0 - smoothstep(from, to, lenL),
                max(1.0 - smoothstep(from, to, lenTL),
                max(1.0 - smoothstep(from, to, lenT),
                max(1.0 - smoothstep(from, to, lenTR),
                max(1.0 - smoothstep(from, to, lenR),
                max(1.0 - smoothstep(from, to, lenBR),
                max(1.0 - smoothstep(from, to, lenB),
                1.0 - smoothstep(from, to, lenBL)))))))));
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
        // Perspective coord
        vec2 persp = vPosition + vec2(
            vPosition.x * 0.05,
            vPosition.y * 0.05 * aspect.x
        );
        // Texture coord system.
        //vec2 tilePos = vec2((persp.x + 1.0) * 0.5, (persp.y * -0.5) + 0.5);
        //float shadow = shadowCoef(tilePos);
        float shadow = texture2D(tileShadows, coord).x;
        // Let shadow fall below line
        vec3 color = (1.0 - alpha) * bg + lineColor * alpha;
        color = mix(color, vec3(1.0, 1.0, 1.0), shadow * 0.2);
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

let createDrawState = (canvas : Canvas.t, tilesTexture, shadowTexture) => {
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
            ProgramTexture.make("tiles", tilesTexture),
            ProgramTexture.make("tileShadows", shadowTexture)
        |]
    )
};

let draw = (ds, canvas) => {
    DrawState.draw(ds, canvas);
};