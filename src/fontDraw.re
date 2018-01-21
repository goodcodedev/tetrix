let vertexSource = {|#version 300 es
    precision mediump float;
    attribute vec2 position;
    attribute vec2 uv;
    varying vec2 vUv;
    
    //uniform mat3 model;
    
    void main() {
        vUv = uv;
        //vec2 pos = vec3(model * vec3(position, 1.0)).xy;
        vec2 pos = position;
        gl_Position = vec4((pos - vec2(40.0, 0.0)) / 50., 0.0, 1.0);
    }
|};

let fragmentSource = {|#version 300 es
    #ifdef GL_OES_standard_derivatives
    #extension GL_OES_standard_derivatives : enable
    #endif
    precision mediump float;
    uniform sampler2D map;
    varying vec2 vUv;

    float median(float r, float g, float b) {
        return max(min(r, g), min(max(r, g), b));
    }

    void main() {
        float opacity = 0.5;
        vec3 color = vec3(0.2, 0.5, 0.8);
        vec3 sample = 1.0 - texture2D(map, vUv).rgb;
        float sigDist = median(sample.r, sample.g, sample.b) - 0.5;
        float alpha = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);
        gl_FragColor = vec4(color.xyz, alpha * opacity);
    }
|};

open Gpu;

type t = {
    canvas: Canvas.t,
    drawState: Gpu.DrawState.t,
    fbuffer: Gpu.FrameBuffer.inited
};


/* Draws a color given quad coords and a color */
let init = (canvas : Gpu.Canvas.t, vertices, image) => {
    let context = canvas.context;
    let fbuffer = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let vertexBuffer = VertexBuffer.make(
        vertices,
        [|
            VertexAttrib.make("position", GlType.Vec2f),
            VertexAttrib.make("uv", GlType.Vec2f)
        |],
        StaticDraw
    );
    let indexBuffer = IndexBuffer.make(
        IndexBuffer.makeQuadsData(Array.length(vertices) / 16),
        StaticDraw
    );
    let imageTexture = Texture.make(Texture.ImageTexture(image), Texture.RGBA);
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [||]
        ),
        [||],
        vertexBuffer,
        indexBuffer,
        [|
            ProgramTexture.make("map", imageTexture)
        |]
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

let draw = (self) => {
    DrawState.draw(self.drawState, self.canvas);
    [%debugger];
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

let loadFont = (font, canvas) => {
    FontFiles.request(font, (fontFiles) => {
        let font = SdfFont.BMFont.parse(fontFiles.bin);
        let layout = SdfFont.TextLayout.make(
            "abc",
            font,
            500,
            ()
        );
        let glyphs = SdfFont.TextLayout.update(layout);
        let vd = SdfFont.TextLayout.vertexData(layout, glyphs);
        Js.log(vd);
        let fontDraw = init(canvas, vd, fontFiles.image);
        draw(fontDraw);
    });
};