let maxIterations = 99999;
type complexNumber = {
    real: float,
    imaginary: float
};
let multComplex = (c1, c2) => {
    {
        real: (c1.real *. c2.real) -. (c1.imaginary *. c2.imaginary),
        imaginary: (c1.real *. c2.imaginary) +. (c1.imaginary *. c2.real)
    }
};
let addComplex = (c1, c2) => {
    {
        real: c1.real +. c2.real,
        imaginary: c1.imaginary +. c2.imaginary
    }
};
let absComplex = (c) => {
    Js_math.sqrt(c.real *. c.real +. c.imaginary *. c.imaginary)
};
let calcIterations = (x, y) => {
    let z = {
        real: 0.,
        imaginary: 0.
    };
    let c = {
        real: x,
        imaginary: y
    };
    let rec iterate = (z, c, iterations) => {
        let z = addComplex(multComplex(z, z), c);
        if (absComplex(z) > 2.0) {
            iterations
        } else {
            iterate(z, c, iterations + 1)
        };
    };
    iterate(z, c, 0)
};

module Constants = RGLConstants;
module Gl = Reasongl.Gl;
let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        gl_Position = vec4(position, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    varying vec2 vPosition;
    const int max_iterations = 80;

    float range_over(float range, int iterations) {
        return float(range - min(range, float(iterations))) / range;
    }

    void main() {
        vec2 z = vec2(0.0, 0.0);
        float z_r_temp = 0.0;
        vec2 c = vec2(vPosition.x * 1.4, vPosition.y * 1.2) * 0.15 + 0.4;
        int iterations = 0;
        for (int i = 0; i <= max_iterations; i++) {
            // z = z*z + c
            z_r_temp = (z.x*z.x - z.y*z.y) + c.x;
            z = vec2(z_r_temp, ((z.x*z.y) * 2.0) + c.y);
            if (length(z) > 2.0) {
                iterations = i;
                break;
            }
        }
        if (iterations == 0) {
            iterations = max_iterations;
        }
        float l_extra = length(z) - 2.0;
        vec3 color = vec3(
            range_over(4.0 + l_extra, iterations),
            range_over(12.0 + l_extra, iterations),
            range_over(80.0 + l_extra, iterations)
        );
        gl_FragColor = vec4(color, 1.0);
    }
|};

let compileShader = (context, shaderType, source) => {
    let shaderHandle = Gl.createShader(~context, shaderType);
    Gl.shaderSource(~context, ~shader=shaderHandle, ~source);
    Gl.compileShader(~context, shaderHandle);
    let compiledCorrectly = Gl.getShaderParameter(~context, ~shader=shaderHandle, ~paramName=Gl.Compile_status) == 1;
    if (!compiledCorrectly) {
        print_endline("Shader compilation failed: " ++ Gl.getShaderInfoLog(~context, shaderHandle));
        None
    } else {
        Some(shaderHandle)
    };
};

let createProgram = (context, vertexSource, fragmentSource) => {
    let vShader = compileShader(context, Constants.vertex_shader, vertexSource);
    let fShader = compileShader(context, Constants.fragment_shader, fragmentSource);
    switch ((vShader, fShader)) {
    | (Some(vShader), Some(fShader)) => {
        let program = Gl.createProgram(~context);
        Gl.attachShader(~context, ~program, ~shader=vShader);
        Gl.deleteShader(~context, vShader);
        Gl.attachShader(~context, ~program, ~shader=fShader);
        Gl.deleteShader(~context, fShader);
        Gl.linkProgram(~context, program);
        let linkedCorrectly = Gl.getProgramParameter(~context, ~program, ~paramName=Gl.Link_status) == 1;
        if (linkedCorrectly) {
            Some(program)
        } else {
            print_endline("GlProgram linking failed" ++ Gl.getProgramInfoLog(~context, program));
            None
        };
    }
    | _ => {
        None
    }
    }
};

open Gpu;

let createCanvas = () => {
    let canvas = Canvas.init(400, 300);
    let context = canvas.context;
    let program = Program.init(Program.make(
        Shader.make(vertexSource),
        Shader.make(fragmentSource),
        [||]
    ), context);
    switch (program) {
    | Some(program) => {
        let vertices = VertexBuffer.makeQuad();
        let vBuffer = VertexBuffer.init(
            vertices,
            context,
            program.programRef
        );
        let indexes = IndexBuffer.makeQuad();
        let indexInit = IndexBuffer.init(indexes, context);
        Canvas.drawIndexes(canvas, program, vBuffer, indexInit);
    }
    | None => failwith("Program creation failed");
    }
};

let createCanvas2 = () => {
    let window = Gl.Window.init(~argv=[||]);
    let width = 400;
    let height = 300;
    Gl.Window.setWindowSize(~window, ~width, ~height);
    let context = Gl.Window.getContext(window);
    Gl.viewport(~context, ~x=0, ~y=0, ~width, ~height);
    let program = createProgram(context, vertexSource, fragmentSource);
    switch (program) {
    | None => failwith("Program creation failed");
    | Some(program) => {
        Gl.useProgram(~context, program);
        let vertexBuffer = Gl.createBuffer(~context);
        let elementBuffer = Gl.createBuffer(~context);
        let vertexPosition = Gl.getAttribLocation(~context, ~program, ~name="position");
        Gl.enableVertexAttribArray(~context, ~attribute=vertexPosition);
        /* Create quad setup */
        let vertexArray = Gl.Bigarray.of_array(Gl.Bigarray.Float32, [|
            -1., -1.,
            -1., 1.,
            1., 1.,
            1., -1.
        |]);
        let elementArray = Gl.Bigarray.of_array(Gl.Bigarray.Uint16, [|
            0, 1, 2, 0, 2, 3
        |]);
        Gl.bindBuffer(~context, ~target=Constants.array_buffer, ~buffer=vertexBuffer);
        Gl.bufferData(
            ~context,
            ~target=Constants.array_buffer,
            ~data=vertexArray,
            ~usage=Constants.static_draw
        );
        Gl.bindBuffer(~context, ~target=Constants.element_array_buffer, ~buffer=elementBuffer);
        Gl.bufferData(
            ~context,
            ~target=Constants.element_array_buffer,
            ~data=elementArray,
            ~usage=Constants.static_draw
        );
        Gl.clearColor(~context, ~r=0., ~g=0., ~b=0., ~a=1.);
        Gl.clear(~context, ~mask=Constants.color_buffer_bit);
        Gl.bindBuffer(~context, ~target=Constants.array_buffer, ~buffer=vertexBuffer);
        Gl.vertexAttribPointer(
            ~context,
            ~attribute=vertexPosition,
            ~size=2 * 4,
            ~type_=Constants.float_,
            ~normalize=false,
            ~stride=0,
            ~offset=0
        );
        Gl.enableVertexAttribArray(~context, ~attribute=vertexPosition);
        Gl.bindBuffer(~context, ~target=Constants.element_array_buffer, ~buffer=elementBuffer);
        Gl.drawElements(
            ~context,
            ~mode=Constants.triangles,
            ~count=6,
            ~type_=Constants.unsigned_short,
            ~offset=0
        );
    }
    }
};
