let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    attribute vec2 uv;
    varying vec2 vUv;
    
    uniform mat3 model;
    
    void main() {
        vUv = uv;
        vec2 pos = vec3(model * vec3(position, 1.0)).xy;
        gl_Position = vec4((pos - vec2(20,0)) / 100., 0.0, 1.0);
    }
|};

let fragmentSource = {|
    #ifdef GL_OES_standard_derivatives
    #extension GL_OES_standard_derivatives : enable
    #endif
    
    precision mediump float;
    
    uniform sampler2D map;
    
    varying vec2 vUv;
    
    float median(float r, float g, float b) {
        return max(min(r,g), min(max(r,g),b));
    }
    
    void main() {
        vec3 sdfVal = 1.0 - texture2D(map, vUv).rgb;
        float sigDist = median(sdfVal.r, sdfVal.g, sdfVal.b) - 0.5;
        //float alpha = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);
        float coef = 1.0 - sigDist;
        float alpha = smoothstep(0.0, 0.22, sigDist);
        gl_FragColor = vec4(0.0, 0.0, 0.0, alpha);
    }
|};

open Gpu;

type t = {
    canvas: Canvas.t,
    drawState: Gpu.DrawState.t,
    fbuffer: Gpu.FrameBuffer.inited
};

let loadFont = (font) => {
    FontFiles.request(font, (fontFiles) => {
        [%debugger];
        let parsed = SdfFont.BMFont.parse(fontFiles.bin);
    });
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
