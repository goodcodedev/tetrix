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

    uniform vec3 color;

    void main() {
        gl_FragColor = vec4(color, 1.0);
    }
|};

open Gpu;

type t = {
    canvas: Canvas.t,
    beams: Texture.t,
    drawState: Gpu.DrawState.t,
    fbuffer: Gpu.FrameBuffer.inited
};
/* Draws a "beams" to a framebuffer given coords and a color */
let init = (canvas : Gpu.Canvas.t) => {
    let context = canvas.context;
    let beams = Texture.make(1024, 1024, Some(Array.make(1024*1024*4, 0)), Texture.RGBA);
    let fbuffer = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let vertexQuad = VertexBuffer.makeQuad(());
    let indexQuad = IndexBuffer.makeQuad();
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [|Uniform.make("color", GlType.Vec3f)|]
        ),
        [|Uniform.UniformVec3f([|1.0, 1.0, 1.0|])|],
        vertexQuad,
        indexQuad,
        [||]
    );
    {
        canvas: canvas,
        drawState: drawState,
        beams: beams,
        fbuffer: fbuffer
    }
};

let draw = (self, canvas) => {
    FrameBuffer.bindTexture(self.fbuffer, self.canvas.context, self.beams);
    Canvas.setFramebuffer(self.canvas, self.fbuffer);
    Canvas.clear(canvas, 0.0, 0.0, 0.0);
    DrawState.draw(self.drawState, self.canvas);
    Canvas.clearFramebuffer(self.canvas);
};

/* Vertices assumed to be quads. Renders to framebuffer,
   so need only be called when updated */
let updateVertices = (self, vertices, canvas) => {
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
    draw(self, canvas);
};

