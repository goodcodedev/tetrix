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
        vec3 color = elColor * 0.7 + vec3(0.0, 0.0, 0.0) * 0.3;
        gl_FragColor = vec4(color, 1.0);
    }
|};

open Gpu;

let makeNode = (elState : SceneState.elState) => {
    Scene.makeNode(
        "element",
        ~updateOn=[UpdateFlags.ElChanged],
        ~size=Aspect(4.0 /. 3.0),
        ~partialDraw=true,
        ~margin=MarginXY(Scale(0.21), Scale(0.022)),
        ~vertShader=Shader.make(currElVertex),
        ~fragShader=Shader.make(currElFragment),
        ~vo=elState.vo,
        ~uniforms=[
            ("elColor", elState.color),
            ("translation", elState.pos)
        ],
        ()
    )
};