let lightBaseVert = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 model;
    void main() {
        vec2 pos = (vec3(position, 1.0) * model * layout).xy;
        gl_Position = vec4(pos.xy, 0.0, 1.0);
    }
|};

let lightBaseFrag = {|
    precision mediump float;

    void main() {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
|};

let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 lightMat;
    varying vec2 lightUV;
    uniform mat3 elMat;
    varying vec2 elUV;
    void main() {
        lightUV = (vec3(position, 1.0) * lightMat).xy;
        elUV = (vec3(position, 1.0) * elMat).xy;
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    varying vec2 lightUV;
    varying vec2 elUV;
    uniform sampler2D light;
    uniform sampler2D el;
    uniform vec3 color;

    void main() {
      vec4 elColor = texture2D(el, elUV);
      float light = texture2D(light, lightUV).x;
      gl_FragColor = mix(vec4(color, light * 0.4), elColor, step(0.01, elColor.a));
    }
|};

let makeNode = (elState: SceneState.elState, lighting) => {
    let cols = 2.0;
    let rows = 1.5;
    let margin = Scene.MarginXY(Scale(0.25), Scale(0.05));
    let toTex = Gpu.Texture.makeEmptyRgb();
    let tempTex = Gpu.Texture.makeEmptyRgb();
    let size = Scene.Aspect(cols /. rows);
    let lightBaseNode = Scene.makeNode(
        ~cls="lightBase",
        ~vertShader=Gpu.Shader.make(lightBaseVert),
        ~fragShader=Gpu.Shader.make(lightBaseFrag),
        ~uniforms=[
            ("model", elState.pos)
        ],
        ~vo=elState.vo,
        ~partialDraw=true,
        ~drawTo=Scene.TextureItem(toTex),
        ~clearOnDraw=true,
        ()
    );
    let lightNode = Blur2.makeNode(
        lightBaseNode,
        toTex,
        tempTex,
        4.0,
        4.0
    );
    let elNode = SdfTiles.makeNode
    (
        cols,
        rows,
        lighting,
        ~vo=elState.vo,
        ~color=SdfNode.SdfDynColor(elState.color),
        ~model=elState.pos,
        ~tileSpace=0.25,
        ~drawTo=Scene.TextureRGBA,
        ()
    );
    /*DrawTex.makeNode(lightNode, ())*/
    Scene.makeNode(
        ~vertShader=Gpu.Shader.make(vertexSource),
        ~fragShader=Gpu.Shader.make(fragmentSource),
        ~transparent=true,
        ~size,
        ~margin,
        ~uniforms=[("color", elState.color)],
        ~deps=[lightNode, elNode],
        ~textures=[
            ("light", Scene.SceneTex.node(lightNode)),
            ("el", Scene.SceneTex.node(elNode)),
        ],
        ()
    )
};
    /*
     Scene.makeNode(
         ~cls="element",
         ~size=Aspect(4.0 /. 3.0),
         ~partialDraw=true,
         ~margin=MarginXY(Scale(0.25), Scale(0.022)),
         ~vertShader=Shader.make(currElVertex),
         ~fragShader=Shader.make(currElFragment),
         ~vo=elState.vo,
         ~uniforms=[
             ("elColor", elState.color),
             ("translation", elState.pos)
         ],
         ()
     )*/
