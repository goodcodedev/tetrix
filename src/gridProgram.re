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
    varying vec2 vPosition;

    const float numCols = 14.0;
    const float numRows = 28.0;
    const float xsize = 1.0 / 280.0 * 2.0;
    const float ysize = 1.0 / 560.0 * 2.0;

    void main() {
        // Normalized coord to 0.0 - 1.0
        vec2 coord = (vPosition + 1.0) * 0.5;
        float xblank = (mod(coord.x + xsize / 2.0, 1.0 / numCols) > xsize) ? 1.0 : 0.0;
        float yblank = (mod(coord.y + ysize / 2.0, 1.0 / numRows) > ysize) ? 1.0 : 0.0;
        float alpha = 1.0 - xblank * yblank;
        gl_FragColor = vec4(lineColor, alpha);
    }
|};

open Gpu;
let createProgram = () => {
    Program.make(
        Shader.make(vertexSource),
        Shader.make(fragmentSource),
        [|Uniform.make("lineColor", GlType.Vec3f)|]
    )
};

let createDrawState = (canvas : Canvas.t) => {
    DrawState.init(
        canvas.context,
        createProgram(),
        [|Uniform.UniformVec3f([|0.0, 0.0, 0.0|])|],
        VertexBuffer.makeQuad(),
        IndexBuffer.makeQuad(),
        [||]
    )
};

let draw = (ds, canvas) => {
    DrawState.draw(ds, canvas);
};