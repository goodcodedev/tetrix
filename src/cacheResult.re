let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 texMat;
    varying vec2 texUV;
    void main() {
        texUV = (vec3(position, 1.0) * texMat).xy;
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    varying vec2 texUV;
    uniform sampler2D tex;

    void main() {
        vec4 texColor = texture2D(tex, texUV);
        gl_FragColor = vec4(texColor);
    }
|};

open Gpu;

let makeNode = (
    node,
    ~transparent=?,
    ~partialDraw=?,
    ~size=?,
    ()
) => {
    let (drawTo, clearOnDraw) = switch (transparent, partialDraw) {
    | (Some(true), _) => (Scene.TextureRGBA, true)
    | (_, Some(true)) => (Scene.TextureRGBA, true)
    | (_, _) => (Scene.TextureRGB, false)
    };
    let texNode = Scene.makeNode(
        ~cls="cacheResultTex",
        ~children=[node],
        ~drawTo,
        ~selfDraw=false,
        ~clearOnDraw,
        ()
    );
    Scene.makeNode(
        ~cls="cachedResult",
        ~key="cachedResult",
        ~vertShader=Shader.make(vertexSource),
        ~fragShader=Shader.make(fragmentSource),
        ~transparent=?transparent,
        ~partialDraw=?partialDraw,
        ~size=?size,
        ~textures=[
            ("tex", Scene.SceneTex.node(texNode))
        ],
        ~deps=[texNode],
        ()
    )
};