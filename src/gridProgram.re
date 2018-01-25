let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 mat;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        vec3 transformed = vec3(position, 1.0) * mat;
        gl_Position = vec4(transformed.xy, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    uniform vec3 lineColor;
    uniform vec2 elPos;
    uniform vec3 elColor;
    uniform vec2 screen;
    uniform sampler2D tiles;
    uniform sampler2D tileShadows;
    uniform sampler2D beams;

    varying vec2 vPosition;

    const float numCols = 12.0;
    const float numRows = 26.0;
    float xsize = 1.0 / screen.x;
    float ysize = 1.0 / screen.y;
    const float tileWidth = 1.0 / numCols;
    const float tileHeight = 1.0 / numRows;

    void main() {
        vec2 aspect = vec2(numCols / numRows, 1.0);
        // Normalized coord to 0.0 - 1.0
        vec2 coord = (vPosition + 1.0) * 0.5;
        float xblank = (mod(coord.x + xsize / 1.0, 1.0 / numCols) > xsize) ? 1.0 : 0.0;
        float yblank = (mod(coord.y + ysize / 1.0, 1.0 / numRows) > ysize) ? 1.0 : 0.0;
        float alpha = 1.0 - xblank * yblank;
        vec3 bg = vec3(0.08, 0.12, 0.22);
        vec2 elVec = vPosition - (elPos + vec2(-0.03, 0.1));
        vec2 elVecNorm = normalize(elVec);
        // Roughly light a triangle below element
        float dir = dot(elVecNorm, vec2(0.0, -1.0));
        float elVecLength = length(elVec);
        float lengthCoef = max(1.5 - elVecLength, 0.0);
        float light = smoothstep(-0.3, 0.5, dir) * lengthCoef * 0.05;
        float colorLight = max(0.35 - length(elVec * aspect), 0.0) * 0.3;
        // Add some shadow from neighbour tiles
        // Perspective coord
        vec2 persp = vPosition + vec2(
            vPosition.x * 0.05,
            vPosition.y * 0.05 * aspect.x
        );
        // Shadow
        float shadow = texture2D(tileShadows, coord).x;
        vec3 color = mix(bg, vec3(0.0, 0.0, 0.0), (1.0 - shadow) * 0.6);
        // Line
        color = mix(color, lineColor, alpha);
        // Beam
        vec3 beam = texture2D(beams, coord).xyz;
        float beamCoef = 0.04 - (1.0 - smoothstep(0.2, 0.4, elVecLength)) * 0.04;
        color = mix(color, beam, (beam.x == 0.0) ? 0.0 : beamCoef);
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
            Uniform.make("screen", GlType.Vec2f),
            Uniform.make("mat", GlType.Mat3f)
        |]
    )
};

let createDrawState = (canvas : Canvas.t, boardCoords : Coords.boardCoords, tilesTexture, shadowTexture, beamTexture) => {
    DrawState.init(
        canvas.context,
        createProgram(),
        [|
            Uniform.UniformVec3f([|0.0, 0.0, 0.0|]),
            Uniform.UniformVec2f([|0.0, 0.0|]),
            Uniform.UniformVec3f([|0.0, 0.0, 0.0|]),
            Uniform.UniformVec2f([|boardCoords.pixelWidth, boardCoords.pixelHeight|]),
            Uniform.UniformMat3f(boardCoords.mat)
        |],
        VertexBuffer.makeQuad(()),
        IndexBuffer.makeQuad(),
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


let makeItem = (canvas : Canvas.t) => {
    let context = canvas.context;
    let vertexQuad = VertexBuffer.makeQuad(());
    let indexQuad = IndexBuffer.makeQuad();
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [||]
        ),
        [||],
        vertexQuad,
        indexQuad,
        [||]
    );
    Scene.makeItem(
        (state, flags) => DrawState.draw(drawState, canvas),
        UpdateFlags.([Frame]),
        []
    )
};