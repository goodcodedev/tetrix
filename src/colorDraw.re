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

    uniform vec4 color;

    void main() {
        gl_FragColor = color;
    }
|};

open Gpu;

type t = {
    canvas: Canvas.t,
    drawState: Gpu.DrawState.t,
    fbuffer: Gpu.FrameBuffer.inited
};
/* Draws a color given quad coords and a color */
let init = (canvas : Gpu.Canvas.t) => {
    let context = canvas.context;
    let fbuffer = FrameBuffer.init(FrameBuffer.make(512, 512), canvas.context);
    let vertexQuad = VertexBuffer.makeQuad();
    let indexQuad = IndexBuffer.makeQuad();
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [|Uniform.make("color", GlType.Vec4f)|]
        ),
        [|Uniform.UniformVec4f([|1.0, 1.0, 1.0|])|],
        vertexQuad,
        indexQuad,
        [||]
    );
    {
        canvas,
        drawState,
        fbuffer
    }
};

let drawToTexture = (self, texture, color, ~clearColor=?, ()) => {
    FrameBuffer.bindTexture(self.fbuffer, self.canvas.context, texture);
    Canvas.setFramebuffer(self.canvas, self.fbuffer);
    switch (clearColor) {
    | Some(color) =>
        Canvas.clear(self.canvas, color[0], color[1], color[2]);
    | None => ()
    };
    self.drawState.uniforms[0] = Uniform.UniformVec4f(color);
    DrawState.draw(self.drawState, self.canvas);
    Canvas.clearFramebuffer(self.canvas);
};

let draw = (self, color) => {
    self.drawState.uniforms[0] = Uniform.UniformVec4f(color);
    DrawState.draw(self.drawState, self.canvas);
};

/* Vertices assumed to be quads. Renders to framebuffer,
   so need only be called when updated */
let updateVertices = (self, vertices) => {
    VertexBuffer.setData(self.drawState.vertexBuffer, vertices);
    switch (self.drawState.indexBuffer) {
    | Some(indexBuffer) => {
        IndexBuffer.setData(
            indexBuffer,
            IndexBuffer.makeQuadsData(Array.length(vertices) / 8)
        );
    }
    | None => failwith("Indexbuffer expected");
    };
};
