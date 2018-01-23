let vertexSource = {|
    precision mediump float;
    attribute vec2 position;

    uniform mat3 model;

    varying vec2 vPosition;
    void main() {
        vec2 pos = vec3(vec3(position, 1.0) * model).xy;
        vPosition = pos;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};

let fragmentSource = SdfShader.createFragmentSource({|
    point += 0.5;
    float boxWidth = 0.5;
    float boxHeight = 0.5;
    float boxDepth = 0.2;
    float rounding = 0.1;
    float box = length(max(abs(point) - vec3(boxWidth, boxHeight, boxDepth), vec3(0.0, 0.0, 0.0))) - rounding;
    return box;
|});

type t = {
    drawState : Gpu.DrawState.t,
    canvas: Gpu.Canvas.t
};

open Gpu;

let createProgram = () => {
    Program.make(
        Shader.make(vertexSource),
        Shader.make(fragmentSource),
        [|
            Uniform.make("model", GlType.Mat3f)
        |]
    )
};

let make = (canvas : Canvas.t, model) => {
    let drawState = DrawState.init(
        canvas.context,
        createProgram(),
        [|
            Uniform.UniformMat3f(model)
        |],
        VertexBuffer.makeQuad(()),
        IndexBuffer.makeQuad(),
        [||]
    );
    {
        drawState,
        canvas
    }
};

let draw = (self) => {
    DrawState.draw(self.drawState, self.canvas);
};