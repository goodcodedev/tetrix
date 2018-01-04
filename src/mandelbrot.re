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

let createImage = (x, y) => {

};

module Constants = RGLConstants;
module Gl = Reasongl.Gl;
let vertexSource = {|
    attribute vec2 position;
    varying vec2 vPosition;
    void main() {
        vPosition = position;
        gl_Position = vec4(position, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    varying vec2 vPosition;
    const int max_iterations = 99999;
    void main() {
        float z_r = 0.0;
        float z_r2 = 0.0;
        float z_i = 0.0;
        float c_r = vPosition.x;
        float c_i = vPosition.y;
        int iterations = 0;
        for (int i = 0; i <= max_iterations; i++) {
            // z = z*z + c
            z_r2 = (z_r*z_r - z_i*z_i) + c_r;
            z_i = ((z_r*z_i) * 2.0) + c_i;
            z_r = z_r2;
            // If abs(z) > 2.0
            if (sqrt(z_i*z_i + z_r*z_r) > 2.0) {
                iterations = i;
                break;
            }
        }
        if (iterations == 0) {
            iterations = max_iterations;
        }
        float c = 1.0 - float(iterations) / float(max_iterations);
        gl_FragColor = vec4(0.0, c / 2.0, c, 1.0);
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
            print_endline("GlProgram linking failed");
            None
        };
    }
    | _ => {
        None
    }
    }
};

let createCanvas = () => {
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
            ~size=2,
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

let drawFrame = () => {

};