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

    uniform sampler2D tiles;

    const float numCols = 12.0;
    const float numRows = 26.0;

    void main() {
        // Perspective coord
        vec2 aspect = vec2(numCols / numRows, 1.0);
        vec2 persp = vPosition + vec2(
            vPosition.x * 0.1,
            0.0
        );
        vec2 tilePos = vec2((persp.x + 1.0) * 0.5, (persp.y - 1.0) * -0.5);
        tilePos.y = tilePos.y - 0.03;
        float tile = texture2D(tiles, tilePos).x;
        vec4 tileColor = (tile > 0.0) ?  vec4(1.0, 1.0, 1.0, 1.0) : vec4(0.0, 0.0, 0.0, 1.0);
        gl_FragColor = (tilePos.x < 0.0 || tilePos.x > 1.0 || tilePos.y < 0.0 || tilePos.y > 1.0) ?
                        vec4(0.0, 0.0, 0.0, 1.0) : tileColor;
    }
|};

let blurVertex = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 unblurredMat;
    varying vec2 vPosition;
    varying vec2 unblurredPos;
    void main() {
        vPosition = position;
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        unblurredPos = (vec3(position, 1.0) * unblurredMat).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};
let blurFragment = {|
    precision mediump float;
    varying vec2 vPosition;
    varying vec2 unblurredPos;

    uniform float pDistance;
    uniform vec2 pixelSize;

    uniform sampler2D unblurred;

    float blurred(vec2 point, float pHeight, float pWidth) {
        // Something like circular box blur
        // should approximate bokeh effect
        // https://en.wikipedia.org/wiki/Gaussian_blur
        // [ 0   .8  1   .8  0  ]
        // [ .8  1   1   1   .8 ]
        // [ 1   1  -1-  1   1  ]
        // [ .8  1   1   1   .8 ]
        // [ 0   .8  1   .8  0  ]
        return (
            texture2D(unblurred, point + vec2(-pWidth, -pHeight*2.0)).x * 0.8 +
            texture2D(unblurred, point + vec2(0.0, -pHeight*2.0)).x +
            texture2D(unblurred, point + vec2(pWidth, -pHeight*2.0)).x * 0.8 +

            texture2D(unblurred, point + vec2(-pWidth*2.0, -pHeight)).x * 0.8 +
            texture2D(unblurred, point + vec2(-pWidth, -pHeight)).x +
            texture2D(unblurred, point + vec2(0.0, -pHeight)).x +
            texture2D(unblurred, point + vec2(pWidth, -pHeight)).x +
            texture2D(unblurred, point + vec2(pWidth*2.0, -pHeight)).x * 0.8 +

            texture2D(unblurred, point + vec2(-pWidth*2.0, 0.0)).x +
            texture2D(unblurred, point + vec2(-pWidth, 0.0)).x +
            texture2D(unblurred, point + vec2(0.0, 0.0)).x +
            texture2D(unblurred, point + vec2(pWidth, 0.0)).x +
            texture2D(unblurred, point + vec2(pWidth*2.0, 0.0)).x +

            texture2D(unblurred, point + vec2(-pWidth*2.0, pHeight)).x * 0.8 +
            texture2D(unblurred, point + vec2(-pWidth, pHeight)).x +
            texture2D(unblurred, point + vec2(0.0, pHeight)).x +
            texture2D(unblurred, point + vec2(pWidth, pHeight)).x +
            texture2D(unblurred, point + vec2(pWidth*2.0, pHeight)).x * 0.8 +

            texture2D(unblurred, point + vec2(-pWidth, pHeight*2.0)).x * 0.8 +
            texture2D(unblurred, point + vec2(0.0, pHeight*2.0)).x +
            texture2D(unblurred, point + vec2(pWidth, pHeight*2.0)).x * 0.8
        ) / (13.0 + 0.8 * 8.0);
    }

    void main() {
        float aspect = pixelSize.x / pixelSize.y;
        float pHeight = 0.002 * pDistance;
        float pWidth = 0.002 * aspect * pDistance;
        float b = blurred(unblurredPos, pHeight, pWidth);
        gl_FragColor = vec4(b, b, b, 1.0);
        //gl_FragColor = vec4(texture2D(unblurred, unblurredPos).x, 1.0, 0.0, 1.0);
    }
|};

open Gpu;

type t = {
    canvas: Canvas.t,
    unblurred: Texture.t,
    drawState: Gpu.DrawState.t,
    blurDraw1: Gpu.DrawState.t,
    blurDraw2: Gpu.DrawState.t,
    blurTex1: Gpu.Texture.t,
    blurTex2: Gpu.Texture.t,
    fbuffer1: Gpu.FrameBuffer.inited,
    fbuffer2: Gpu.FrameBuffer.inited,
    fbuffer3: Gpu.FrameBuffer.inited
};

let init = (canvas : Gpu.Canvas.t, boardCoords : Coords.boardCoords, tilesTex) => {
    let context = canvas.context;
    /* First draw unblurred */
    let unblurred = Texture.make(IntDataTexture(Array.make(1024*1024*4, 0), 1024, 1024), Texture.RGBA, Texture.LinearFilter);
    let fbuffer1 = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let vertexQuad = VertexBuffer.makeQuad(());
    let indexQuad = IndexBuffer.makeQuad();
    /* Draw to framebuffer */
    let drawState = DrawState.init(
        context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [||]
        ),
        vertexQuad,
        Some(indexQuad),
        [|
            ProgramTexture.make(
                "tiles",
                tilesTex
            )
        |]
    );
    /* Blur draw */
    let blurTex1 = Texture.make(IntDataTexture(Array.make(1024*1024*4, 0), 1024, 1024), Texture.RGBA, Texture.LinearFilter);
    let fbuffer2 = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let blurTex2 = Texture.make(IntDataTexture(Array.make(1024*1024*4, 0), 1024, 1024), Texture.RGBA, Texture.LinearFilter);
    let fbuffer3 = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    let blurVertex = Shader.make(blurVertex);
    let blurFragment = Shader.make(blurFragment);
    let blurProgram = Program.make(
        blurVertex,
        blurFragment,
        [|
            Uniform.make("pDistance", UniformFloat(ref(10.0))),
            Uniform.make("screen", UniformVec2f(ref(Data.Vec2.make(
                boardCoords.pixelWidth,
                boardCoords.pixelHeight
            )))),
        |]
    );
    let blurDraw1 = DrawState.init(
        context,
        blurProgram,
        vertexQuad,
        Some(indexQuad),
        [|
            ProgramTexture.make(
                "unblurred",
                unblurred
            )
        |]
    );
    let blurDraw2 = DrawState.init(
        context,
        blurProgram,
        vertexQuad,
        Some(indexQuad),
        [|
            ProgramTexture.make(
                "unblurred",
                blurTex1
            )
        |]
    );
    {
        canvas: canvas,
        drawState: drawState,
        blurDraw1: blurDraw1,
        blurDraw2: blurDraw2,
        blurTex1: blurTex1,
        blurTex2: blurTex2,
        fbuffer1: fbuffer1,
        fbuffer2: fbuffer2,
        fbuffer3: fbuffer3,
        unblurred: unblurred
    }
};

let draw = (ts) => {
    /* Draw tiles as black */
    FrameBuffer.bindTexture(ts.fbuffer1, ts.canvas.context, ts.unblurred);
    Canvas.setFramebuffer(ts.canvas, ts.fbuffer1);
    DrawState.draw(ts.drawState, ts.canvas);
    Canvas.clearFramebuffer(ts.canvas);
    /* Draw first pass blur */
    FrameBuffer.bindTexture(ts.fbuffer2, ts.canvas.context, ts.blurTex1);
    Canvas.setFramebuffer(ts.canvas, ts.fbuffer2);
    Uniform.setFloat(ts.drawState.program.uniforms[0].uniform, 10.0);
    DrawState.draw(ts.blurDraw1, ts.canvas);
    Canvas.clearFramebuffer(ts.canvas);
    /* Draw second pass blur */
    FrameBuffer.bindTexture(ts.fbuffer3, ts.canvas.context, ts.blurTex2);
    Canvas.setFramebuffer(ts.canvas, ts.fbuffer3);
    Uniform.setFloat(ts.blurDraw2.program.uniforms[0].uniform, 1.0);
    DrawState.draw(ts.blurDraw2, ts.canvas);
    Canvas.clearFramebuffer(ts.canvas);
};

let makeNode = (tilesTex) => {
    /* First draw unblurred */
    let unblurred = Scene.makeNode(
        "unblurred",
        ~updateOn=[UpdateFlags.TilesChanged,UpdateFlags.Resize],
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ~textures=[
            ("tiles", tilesTex)
        ],
        ~drawTo=Scene.TextureRGBA,
        ()
    );
    /* First blur */
    let blur1 = Scene.makeNode(
        "blur1",
        ~updateOn=[UpdateFlags.TilesChanged,UpdateFlags.Resize],
        ~vertShader=Shader.make(blurVertex),
        ~fragShader=Shader.make(blurFragment),
        ~uniforms=[
            ("pDistance", Scene.UFloat.make(10.0))
        ],
        ~pixelSizeUniform=true,
        ~textures=[
            ("unblurred", Scene.SceneTex.node(unblurred))
        ],
        ~drawTo=Scene.TextureRGBA,
        ()
    );
    /* Second blur */
    Scene.makeNode(
        "blur2",
        ~updateOn=[UpdateFlags.TilesChanged,UpdateFlags.Resize],
        ~vertShader=Shader.make(blurVertex),
        ~fragShader=Shader.make(blurFragment),
        ~uniforms=[
            ("pDistance", Scene.UFloat.make(1.0))
        ],
        ~pixelSizeUniform=true,
        ~textures=[
            ("unblurred", Scene.SceneTex.node(blur1))
        ],
        ~drawTo=Scene.TextureRGB,
        ~deps=[
            unblurred,
            blur1
        ],
        ()
    );
};