let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
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
    let beams = Texture.make(IntDataTexture(Array.make(1024*1024*4, 0), 1024, 1024), Texture.RGBA, Texture.NearestFilter);
    let fbuffer = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let vertexQuad = VertexBuffer.makeQuad(());
    let indexQuad = IndexBuffer.makeQuad();
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [|Uniform.make("color", UniformVec3f(ref(Data.Vec3.make(1.0, 1.0, 1.0))))|]
        ),
        vertexQuad,
        Some(indexQuad),
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
    Canvas.clear(canvas, 0.0, 0.0, 0.0, 1.0);
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

let makeNode = (elColor, vo) => {
    Scene.makeNode(
        ~key="beams",
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ~uniforms=[
            ("color", elColor)
        ],
        ~drawTo=Scene.TextureRGB,
        ~clearOnDraw=true,
        ~vo,
        ()
    )
};