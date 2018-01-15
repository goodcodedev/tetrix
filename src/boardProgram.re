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
    varying vec2 vPosition;

    uniform sampler2D tiles;
    uniform sampler2D sdfTiles;

    const int numCols = 14;
    const int numRows = 28;

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
        vec2 sdfPos = vec2(tilePos.x, (tilePos.y * -1.0) + 1.0);
        vec3 sdfColor = texture2D(sdfTiles, sdfPos).xyz;
        float sdfCoef = abs(0.5 - sdfColor.x);
        float tileCoef = 1.0 - sdfCoef;
        //color = color * 0.5 + ((colorIdx == 0) ? vec3(0.0, 0.0, 0.0) : (sdfColor * 0.5));
        color = (colorIdx == 0) ? color : color * tileCoef + sdfColor * sdfCoef;
        gl_FragColor = vec4(color, 1.0);
    }
|};

let currElFragment = {|
    precision mediump float;
    varying vec2 vPosition;

    uniform vec3 elColor;
    uniform sampler2D sdfTiles;

    void main() {
        vec2 tilePos = vec2((vPosition.x + 1.0) * 0.5, (vPosition.y - 1.0) * -0.5);
        vec2 sdfPos = vec2(tilePos.x, (tilePos.y * -1.0) + 1.0);
        vec3 sdfColor = texture2D(sdfTiles, sdfPos).xyz;
        vec3 color = elColor * 0.5 + sdfColor * 0.5;
        gl_FragColor = vec4(color, 1.0);
        gl_FragColor = vec4(elColor, 1.0);
    }
|};

type t = {
    tiles: array(int),
    mutable updateTiles: bool,
    mutable currElTiles: array(float),
    mutable updateCurrEl: bool,
    mutable currElColor: array(float),
    mutable updateCurrElColor: bool,
    drawState: Gpu.DrawState.t,
    currElDraw: Gpu.DrawState.t,
    canvas: Gpu.Canvas.t,
    frameBuffer: Gpu.FrameBuffer.t
};

open Gpu;

let currElVertices = VertexBuffer.make(
    [|-0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0.5, -0.5|],
    [|VertexAttrib.make("position", Vec2f)|],
    DynamicDraw
);
let currElIndexes = IndexBuffer.make(IndexBuffer.makeQuadsData(1), DynamicDraw);

let createCanvas = (tiles) => {
    let canvas = Canvas.init(280, 560);
    let sdfTilesTex = Texture.make(512, 512, Some(Array.make(512*512*4, 0)), Texture.RGBA);
    let vertexQuad = VertexBuffer.makeQuad();
    let indexQuad = IndexBuffer.makeQuad();
    let fbuffer = FrameBuffer.make(512, 512);
    let fbufferInit = FrameBuffer.init(fbuffer, canvas.context);
    /* Draw to framebuffer */
    let sdfTilesDrawState = DrawState.init(
        canvas.context,
        SdfTiles.createProgram(),
        [||],
        vertexQuad,
        indexQuad,
        [||]
    );
    FrameBuffer.bindTexture(fbufferInit, canvas.context, sdfTilesTex);
    Canvas.setFramebuffer(canvas, fbufferInit);
    DrawState.draw(sdfTilesDrawState, canvas);
    Canvas.clearFramebuffer(canvas);
    /* Board program drawState */
    let drawState = DrawState.init(
        canvas.context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [||]
        ),
        [||],
        vertexQuad,
        indexQuad,
        [|
            ProgramTexture.make(
                "tiles",
                Texture.make(14, 28, Some(tiles), Texture.Luminance)
            ),
            ProgramTexture.make("sdfTiles", sdfTilesTex)
        |]
    );
    let currElDraw = DrawState.init(
        canvas.context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(currElFragment),
            [|Uniform.make("elColor", GlType.Vec3f)|]
        ),
        [|Uniform.UniformVec3f([|1.0, 0.0, 1.0|])|],
        currElVertices,
        currElIndexes,
        [|
            ProgramTexture.make("sdfTiles", sdfTilesTex)
        |]
    );
    DrawState.draw(drawState, canvas);
    {
        tiles: tiles,
        currElTiles: [||],
        updateCurrEl: false,
        currElColor: [||],
        updateCurrElColor: false,
        drawState: drawState,
        currElDraw: currElDraw,
        canvas: canvas,
        updateTiles: false,
        frameBuffer: fbuffer
    }
};

let draw = (bp) => {
    if (bp.updateTiles) {
        bp.drawState.textures[0].texture.update = true;
        bp.updateTiles = false;
    };
    if (bp.updateCurrElColor) {
        bp.currElDraw.uniforms[0] = Uniform.UniformVec3f(bp.currElColor);
    };
    if (bp.updateCurrEl) {
        bp.currElDraw.vertexBuffer.data = bp.currElTiles;
        bp.currElDraw.vertexBuffer.update = true;
        switch (bp.currElDraw.indexBuffer) {
        | Some(indexBuffer) => {
            /* Four per quad, 2 per element */
            indexBuffer.data = IndexBuffer.makeQuadsData(Array.length(bp.currElTiles) / 4 / 2);
            indexBuffer.update = true;
        }
        | None => ()
        }
    };
    Canvas.clear(bp.canvas, 0.0, 0.0, 0.0);
    DrawState.draw(bp.drawState, bp.canvas);
    DrawState.draw(bp.currElDraw, bp.canvas);
};