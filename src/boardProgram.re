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
        gl_FragColor = vec4(color, 1.0);
    }
|};

type t = {
    tiles: array(int),
    mutable updateTiles: bool,
    drawState: Gpu.DrawState.t,
    canvas: Gpu.Canvas.t
};

open Gpu;

let createCanvas = (tiles) => {
    let canvas = Canvas.init(280, 560);
    let drawState = DrawState.init(
        canvas.context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [||]
        ),
        VertexBuffer.makeQuad(),
        IndexBuffer.makeQuad(),
        [|DataTexture.make(14, 28, "tiles", Some(tiles))|]
    );
    DrawState.draw(drawState, canvas);
    {
        tiles: tiles,
        drawState: drawState,
        canvas: canvas,
        updateTiles: false
    }
};

let draw = (bp) => {
    if (bp.updateTiles) {
        bp.drawState.textures[0].update = true;
        bp.updateTiles = false;
    };
    DrawState.draw(bp.drawState, bp.canvas);
};