let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 tileShadowsMat;
    uniform mat3 beamsMat;
    uniform mat3 dropMat;
    varying vec2 vPosition;
    varying vec2 tileShadowsPos;
    varying vec2 beamsPos;
    varying vec2 dropPos;
    
    varying vec2 coord;
    void main() {
        vPosition = position;
        coord = (position + 1.0) * 0.5;
        vec2 transformed = (vec3(position, 1.0) * layout).xy;
        tileShadowsPos = (vec3(position, 1.0) * tileShadowsMat).xy;
        beamsPos = (vec3(position, 1.0) * beamsMat).xy;
        dropPos = (vec3(position, 1.0) * dropMat).xy;
        gl_Position = vec4(transformed.xy, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    uniform vec3 bg;
    uniform vec3 lineColor;
    uniform vec2 elPos;
    uniform vec3 elColor;
    uniform vec3 dropColor;
    uniform vec2 pixelSize;
    uniform sampler2D tiles;
    uniform sampler2D tileShadows;
    uniform sampler2D beams;
    uniform sampler2D drop;
    uniform vec4 centerRadius;

    varying vec2 vPosition;
    varying vec2 tileShadowsPos;
    varying vec2 beamsPos;
    varying vec2 dropPos;

    // Normalized to 0.0 - 1.0
    varying vec2 coord;

    const float numCols = 12.0;
    const float numRows = 26.0;
    const vec2 aspect = vec2(numCols / numRows, 1.0);
    const float colSize = 2.0 / numCols;
    const float rowSize = 2.0 / numRows;
    float xsize = 2.0 / pixelSize.x;
    float ysize = 2.0 / pixelSize.y;
    const float tileWidth = 1.0 / numCols;
    const float tileHeight = 1.0 / numRows;

    void main() {
        float xblank = (mod(vPosition.x, colSize) > xsize) ? 1.0 : 0.0;
        float x3blank = (mod(vPosition.x - colSize*3.0, colSize * 6.0) > xsize * 2.0) ? 1.0 : 0.0;
        float yblank = (mod(vPosition.y + ysize, rowSize) > ysize) ? 1.0 : 0.0;
        float lineCoef = (1.0 - xblank * yblank * x3blank) * 0.9 + (1.0 - x3blank) * 0.1;

        // todo: Calculate in vertex shader
        vec2 elVec = vPosition - centerRadius.xy;
        vec2 elVecNorm = normalize(elVec);
        float elVecLength = length(elVec);

        float lengthCoef = max(1.5 - elVecLength, 0.0);

        // Roughly light a triangle below element
        float triangleDir = dot(elVecNorm, vec2(0.0, -1.0));
        float triangleLight = smoothstep(-0.3, 0.5, triangleDir) * lengthCoef * 0.05;

        // Aura light
        float skew = (vPosition.y - centerRadius.y + 0.15) / 0.8;
        skew = 1.0;
        float auraLight = max(0.15 - length(elVec * centerRadius.wz) * skew, 0.0) * 0.05;

        // Shadow
        float shadow = texture2D(tileShadows, tileShadowsPos).x;
        vec3 color = mix(bg, vec3(0.0, 0.0, 0.0), shadow * 0.15);

        // Line
        color = mix(color, lineColor, lineCoef);

        // Beam
        vec3 beam = texture2D(beams, beamsPos).xyz;
        color = mix(color, beam, (beam.x == 0.0) ? 0.0 : 0.05);

        // Dropbeam
        float dropBeam = texture2D(drop, dropPos).x;
        color = mix(color, dropColor, dropBeam * 0.2);
        color = color + elColor * auraLight;
        gl_FragColor = vec4(color + triangleLight, 1.0);
    }
|};

open Gpu;
let createProgram = (boardCoords : Coords.boardCoords) => {
    open Data;
    Program.make(
        Shader.make(vertexSource),
        Shader.make(fragmentSource),
        [|
            Uniform.make("lineColor", UniformVec3f(ref(Vec3.zeros()))),
            Uniform.make("elPos", UniformVec2f(ref(Vec2.zeros()))),
            Uniform.make("elColor", UniformVec3f(ref(Vec3.zeros()))),
            Uniform.make("screen", UniformVec2f(ref(Vec2.make(boardCoords.pixelWidth, boardCoords.pixelHeight)))),
            Uniform.make("mat", UniformMat3f(ref(Mat3.fromArray(boardCoords.mat))))
        |]
    )
};

let createDrawState = (canvas : Canvas.t, boardCoords : Coords.boardCoords, tilesTexture, shadowTexture, beamTexture) => {
    DrawState.init(
        canvas.context,
        createProgram(boardCoords),
        VertexBuffer.makeQuad(()),
        Some(IndexBuffer.makeQuad()),
        [|
            ProgramTexture.make("tiles", tilesTexture),
            ProgramTexture.make("tileShadows", shadowTexture),
            ProgramTexture.make("beams", beamTexture)
        |]
    )
};

let draw = (ds, canvas) => {
    DrawState.draw(ds, canvas);
};

let makeNode = (
    boardColor,
    lineColor,
    tilesTex,
    tileShadows,
    beamNode,
    dropNode,
    dropColor,
    sdfTiles,
    elState : SceneState.elState,
    centerRadius
) => {
    open Scene;
    /*let bg = UVec3f.vals(0.08, 0.12, 0.22);*/
    makeNode(
        ~key="grid",
        /*
        ~margin=Scene.MarginRBLT(
            Scale(0.01),
            Scale(0.002),
            Scale(0.01),
            Scale(0.005)
        ),
        */
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ~uniforms=[
            ("bg", boardColor),
            ("lineColor", lineColor),
            ("elPos", elState.pos),
            ("elColor", elState.color),
            ("dropColor", dropColor),
            ("centerRadius", centerRadius)
        ],
        ~pixelSizeUniform=true,
        ~textures=[
            ("tiles", tilesTex),
            ("tileShadows", SceneTex.node(tileShadows)),
            ("beams", SceneTex.node(beamNode)),
            ("drop", SceneTex.node(dropNode))
        ],
        ~deps=[
            sdfTiles,
            beamNode,
            tileShadows,
            dropNode
        ],
        ()
    )
};