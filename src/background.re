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

    void main() {
        float xNorm = (vPosition.x + 1.0) * 0.5;
        float yNorm = (vPosition.y + 1.0) * 0.5;
        vec3 color = vec3(0.0, 0.2 + 0.2 * xNorm * yNorm, 0.3 + 0.3 * xNorm * yNorm);
        gl_FragColor = vec4(color, 1.0);
    }
|};

open Gpu;

type t = {
    canvas: Canvas.t,
    drawState: Gpu.DrawState.t
};
let init = (canvas : Gpu.Canvas.t) => {
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
    {
        canvas,
        drawState
    }
};

let draw = (self) => {
    DrawState.draw(self.drawState, self.canvas);
};

let makeNode = () => {
    Scene.makeNode(
        "background",
        ~updateOn=UpdateFlags.([Frame]),
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ()
    )
};