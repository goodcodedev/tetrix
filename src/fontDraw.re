let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    attribute vec2 uv;

    uniform mat3 model;
    uniform mat3 layout;
    uniform vec2 pixelSize;

    varying vec2 vUv;
    varying float smoothFactor;
    
    void main() {
        vUv = uv;
        // x scale. I got the value of this by outputting smoothfactor and sampling color
        // Divided by some factors that looked good to see what I needed
        // to multiply with.
        // Took rise over run with smoothFactor as run and factor as rise,
        // subtracted x - x*slope to get + constant.
        smoothFactor = model[0][0] * pixelSize.x;
        smoothFactor = (smoothFactor * -1.6211904762 + 15.4452380953);
        vec2 pos = vec3(vec3(position, 1.0) * model * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};
/* https://github.com/libgdx/libgdx/wiki/Distance-field-fonts */
let fragmentSource = {|
    #ifdef GL_OES_standard_derivatives
    #extension GL_OES_standard_derivatives : enable
    #endif
    precision mediump float;
    uniform sampler2D map;
    uniform vec3 color;
    uniform float opacity;
    varying vec2 vUv;
    varying float smoothFactor;

    float aastep(float value) {
      #ifdef GL_OES_standard_derivatives
        float afwidth = length(vec2(dFdx(value), dFdy(value))) * 0.70710678118654757;
      #else
        float afwidth = (1.0 / 32.0) * (1.4142135623730951 / (2.0 * gl_FragCoord.w));
      #endif
      //afwidth = afwidth * 22.0;
      afwidth = afwidth * smoothFactor;
      // 1.0 - 5.0
      // 0.3 - 22.0
      return smoothstep(0.5 - afwidth, 0.5 + afwidth, value);
    }

    void main() {
        float discardLimit = 0.0001;
        vec4 texColor = 1.0 - texture2D(map, vUv);
        float alpha = aastep(texColor.x);
        //color = texColor.xyz;
        float colorCoef = 1.0 - alpha;
        vec3 c = color * alpha;
        gl_FragColor = vec4(c, opacity * alpha);
        //gl_FragColor = vec4(smoothFactor, 0.0, 0.0, opacity * alpha);
        if (alpha < discardLimit) {
            discard;
        }
    }
|};
let msdfVertexSource = String.trim({|#version 300 es
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
|});

let msdfFragmentSource = String.trim({|#version 300 es
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
|});

open Gpu;

type t = {
    canvas: Canvas.t,
    drawState: Gpu.DrawState.t,
    fbuffer: Gpu.FrameBuffer.inited,
    fbTexture: Gpu.Texture.t,
    model: Data.Mat3.t,
    mutable hasTexture: bool,
    bgDraw: Gpu.DrawState.t,
};


/* Draws a color given quad coords and a color */
let init = (canvas : Gpu.Canvas.t, vertices, image, model, bgDraw) => {
    let context = canvas.context;
    let fbuffer = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let fbTexture = Texture.make(Texture.EmptyTexture, Texture.RGBA, Texture.LinearFilter);
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
    let imageTexture = Texture.make(Texture.ImageTexture(image), Texture.RGBA, Texture.LinearFilter);
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [|
                Uniform.make("model", UniformMat3f(ref(model)))
            |]
        ),
        vertexBuffer,
        Some(indexBuffer),
        [|
            ProgramTexture.make("map", imageTexture)
        |]
    );
    {
        canvas,
        drawState,
        fbuffer,
        fbTexture,
        model,
        hasTexture: false,
        bgDraw
    }
};

/*
    It would be nice to pack several renders into the same texture,
    and associate positions with individual text draw instances.
*/
let drawToTexture = (self) => {
    FrameBuffer.bindTexture(self.fbuffer, self.canvas.context, self.fbTexture);
    Canvas.setFramebuffer(self.canvas, self.fbuffer);
    Canvas.clear(self.canvas, 0.0, 0.0, 0.0);
    DrawState.draw(self.drawState, self.canvas);
    Canvas.clearFramebuffer(self.canvas);
    self.hasTexture = true;
};

let getTexture = (self) => {
    if (!self.hasTexture) {
        drawToTexture(self);
    };
    self.fbTexture
};

let draw = (self) => {
    let context = self.canvas.context;
    Gl.enable(~context, Constants.blend);
    Gl.blendFunc(~context, Constants.src_alpha, Constants.one_minus_src_alpha);
    DrawState.draw(self.drawState, self.canvas);
    Gl.disable(~context, Constants.blend);
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

let loadFont = (font, canvas, bgDraw) => {
    FontFiles.request(font, "sheet0", (fontFiles) => {
        let font = SdfFont.BMFont.parse(fontFiles.bin);
        let layout = SdfFont.TextLayout.make(
            "Vimtris",
            font,
            500,
            ()
        );
        let (_, glyphs) = SdfFont.TextLayout.update(layout);
        let vd = SdfFont.TextLayout.vertexData(layout, glyphs);
        let scale = Data.Mat3.scale(1.0  /. 80.0, 1.0  /. -80.0);
        let vpTrans = Data.Mat3.trans(-1.0, 0.0);
        let model = Data.Mat3.matmul(scale, vpTrans);
        let fontDraw = init(canvas, vd, fontFiles.image, model, bgDraw);
        draw(fontDraw);
    });
};

let makeNode = (
    text,
    font,
    ~key="fontDraw",
    ~height=0.2,
    ~numLines=1,
    ~align=SdfFont.TextLayout.AlignLeft,
    ~color=Color.fromFloats(1.0, 1.0, 1.0),
    ~opacity=1.0,
    ~hidden=false,
    ()
) => {
    let fontTexture = Texture.make(Texture.EmptyTexture, Texture.RGBA, Texture.LinearFilter);
    let vertexBuffer = VertexBuffer.make(
        [||],
        [|
            VertexAttrib.make("position", GlType.Vec2f),
            VertexAttrib.make("uv", GlType.Vec2f)
        |],
        StaticDraw
    );
    let indexBuffer = IndexBuffer.make([||], StaticDraw);
    let uModel = Scene.UMat3f.id();
    let aspect = 1.0 /. (height *. float_of_int(numLines));
    let vo = Scene.SceneVO.make(vertexBuffer, Some(indexBuffer));
    let node = Scene.makeNode(
        key,
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ~textures=[("map", Scene.SceneTex.tex(fontTexture))],
        ~vo,
        ~uniforms=[
            ("model", uModel),
            ("color", Scene.UVec3f.vec(Color.toVec3(color))),
            ("opacity", Scene.UFloat.make(opacity))
        ],
        ~pixelSizeUniform=true,
        ~transparent=true,
        ~loading=true,
        ~size=Scene.Aspect(aspect),
        ~hidden,
        ()
    );
    
    FontFiles.request(font, "sheet0", (fontFiles) => {
        let font = SdfFont.BMFont.parse(fontFiles.bin);
        let scale = 2.0 /. float_of_int(font.common.lineHeight) *. height;
        let width = 2.0 /. scale;
        /* todo: move to nodes update function */
        let layout = SdfFont.TextLayout.make(
            text,
            font,
            int_of_float(width),
            ~align,
            ()
        );
        let (maxLineWidth, glyphs) = SdfFont.TextLayout.update(layout);
        let vertices = SdfFont.TextLayout.vertexData(layout, glyphs);
        VertexBuffer.setDataT(vertexBuffer, vertices);
        IndexBuffer.setDataT(indexBuffer, IndexBuffer.makeQuadsData(Array.length(vertices) / 16));
        Texture.setDataT(fontTexture, Texture.ImageTexture(fontFiles.image));
        /* Update model matrix value */
        let xTrans = switch (align) {
        | SdfFont.TextLayout.AlignLeft => -1.0
        | SdfFont.TextLayout.AlignCenter => (float_of_int(maxLineWidth) *. scale) *. -0.5;
        | SdfFont.TextLayout.AlignRight => (1.0 -. (float_of_int(maxLineWidth) *. scale))
        };
        let modelMat = Data.Mat3.matmul(
            Data.Mat3.trans(xTrans, 1.0 -. height),
            Data.Mat3.scale(scale, scale *. aspect)
        );
        Uniform.setMat3f(uModel.uniform, modelMat);
        node.loading = false;
    });
    node
};