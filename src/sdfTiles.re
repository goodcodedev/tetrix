let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        gl_Position = vec4(position, 0.0, 1.0);
    }
|};

let fragmentSource = SdfShader.createFragmentSource({|
    point.x = mod(point.x, 1.0 / 12.0) - 1.0 / 24.0;
    point.y = mod(point.y, 1.0 / 26.0) - 1.0 / 52.0;
    float boxWidth = 1.0 / 24.0;
    float boxHeight = 1.0 / 52.0;
    float boxDepth = 0.1;
    float box = length(max(abs(point) - vec3(boxWidth, boxHeight, boxDepth), vec3(0.0, 0.0, 0.0)));
    // Octahedron towards z
    float rot = 20.0;
    mat3 xrot = mat3(
        1.0, 0.0, 0.0,
        0.0, cos(rot), -sin(rot),
        0.0, sin(rot), cos(rot)
    );
    //point = point * xrot;
    float d = 0.0;
    // Dont know too much of what I'm doing here..
    // sdpyramid from https://www.shadertoy.com/view/Xds3zN with some modifications
    vec3 octa = vec3(0.5 * boxHeight / boxWidth, 0.5, 0.24);
    d = max( d, abs( dot(point, vec3( -octa.x, 0, octa.z )) ));
    d = max( d, abs( dot(point, vec3(  octa.x, 0, octa.z )) ));
    d = max( d, abs( dot(point, vec3(  0, -octa.y, octa.z )) ));
    d = max( d, abs( dot(point, vec3(  0, octa.y, octa.z )) ));
    float o = d - octa.z / 27.0;
    // Intersection
    //return o;
    return max(o, box);
|});

type t = {
    drawState: Gpu.DrawState.t,
    canvas: Gpu.Canvas.t
};

open Gpu;

let createProgram = () => {
    Program.make(
        Shader.make(vertexSource),
        Shader.make(fragmentSource),
        [||]
    )
};

let createDrawState = (canvas : Canvas.t) => {
    DrawState.init(
        canvas.context,
        createProgram(),
        [||],
        VertexBuffer.makeQuad(()),
        IndexBuffer.makeQuad(),
        [||]
    )
};
let createCanvas = () => {
    let canvas = Canvas.init(240, 580);
    let drawState = createDrawState(canvas);
    DrawState.draw(drawState, canvas);
    {
        drawState: drawState,
        canvas: canvas
    }
};

let draw = (bp) => {
    DrawState.draw(bp.drawState, bp.canvas);
};