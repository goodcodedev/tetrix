let currElVertex = {|
    precision mediump float;
    attribute vec2 position;
    uniform vec2 translation;
    uniform mat3 layout;
    varying vec2 vPosition;
    void main() {
        // Doubled translation here for better precision
        // Calibrated to not extend outside -1 to 1,
        // it uses same code as board elements, just
        // doing quick fix atleast for now.
        vPosition = (position + translation / 2.0) * 0.75;
        vec3 transformed = vec3(vPosition, 1.0) * layout;
        gl_Position = vec4(transformed.xy, 0.0, 1.0);
    }
|};
let currElFragment = {|
    precision mediump float;
    varying vec2 vPosition;

    uniform vec3 elColor;

    void main() {
        vec3 color = elColor * 0.8 + vec3(0.0, 0.0, 0.0) * 0.2;
        gl_FragColor = vec4(color, 1.0);
    }
|};

let makeNode = (elState : SceneState.elState, lighting) => {
    SdfTiles.makeNode(
        2.0,
        1.5,
        lighting,
        ~vo=elState.vo,
        ~color=SdfNode.SdfDynColor(elState.color),
        ~model=elState.pos,
        ~margin=MarginXY(Scale(0.25), Scale(0.05)),
        ~tileSpace=0.22,
        ()
    )
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
};