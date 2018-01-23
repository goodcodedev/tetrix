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
let init = (canvas : Gpu.Canvas.t, boardCoords: Coords.boardCoords) => {
    let context = canvas.context;
    let fbuffer = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let vertexQuad = VertexBuffer.makeQuad(());
    let indexQuad = IndexBuffer.makeQuad();
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [|
                Uniform.make("color", GlType.Vec4f),
                Uniform.make("mat", GlType.Mat3f)
            |]
        ),
        [|
            Uniform.UniformVec4f([|1.0, 1.0, 1.0|]),
            Uniform.UniformMat3f(boardCoords.mat),
        |],
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
