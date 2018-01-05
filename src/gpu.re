module Constants = RGLConstants;
module Gl = Reasongl.Gl;

type uniform = {
    name: string
};

type attribute = {
    name: string,
    attribLocation: Gl.attributeT,
    size: int
};

type vertexBuffer = {
    bufferRef: Gl.bufferT,
    usage: int,
    data: array(float)
};

type elementBuffer = {
    bufferRef: Gl.bufferT,
    usage: int,
    data: array(int)
};

type program = {
    programRef: Gl.programT,
    uniforms: list(uniform),
    attributes: list(attribute),
    vertexBuffer: vertexBuffer,
    elementBuffer: option(elementBuffer)
};

/* Indexes uniforms by uniform index in
   arrays of each possible type */
type programState = {
    floats: array(float),
    ints: array(int)
};
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

let makeProgram = (
    context,
    vertexSource,
    fragmentSource,
    attributes,
    uniforms,
    vertexBuffer,
    elementBuffer) => {
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
            Some({
                programRef: program,
                uniforms,
                attributes,
                vertexBuffer,
                elementBuffer
            })
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

let initProgram = (context, program) => {
    Gl.useProgram(~context, program.programRef);
    let vertexBuffer = Gl.createBuffer(~context);
    Gl.bindBuffer(
        ~context,
        ~target=Constants.array_buffer,
        ~buffer=vertexBuffer
    );
    Gl.bufferData(
        ~context,
        ~target=Constants.array_buffer,
        ~data=Gl.Bigarray.of_array(Gl.Bigarray.Float32, program.vertexBuffer.data),
        ~usage=Constants.static_draw
    );
    switch (program.elementBuffer) {
    | Some(elementBuffer) => {
        let bufferRef = Gl.createBuffer(~context);
        Gl.bindBuffer(
            ~context,
            ~target=Constants.element_array_buffer,
            ~buffer=elementBuffer.bufferRef
        );
        Gl.bufferData(
            ~context,
            ~target=Constants.element_array_buffer,
            ~data=Gl.Bigarray.of_array(Gl.Bigarray.Uint16, elementBuffer.data),
            ~usage=elementBuffer.usage
        );
    }
    | None => ()
    };
};

let initCanvas = (width, height) => {
    let window = Gl.Window.init(~argv=[||]);
    Gl.Window.setWindowSize(~window, ~width, ~height);
    let context = Gl.Window.getContext(window);
    Gl.viewport(~context, ~x=0, ~y=0, ~width, ~height);
    context
};

let drawFrame = (context, program) => {
    Gl.clearColor(~context, ~r=0., ~g=0., ~b=0., ~a=1.);
    Gl.clear(~context, ~mask=Constants.color_buffer_bit);
    List.iter((attrib) => {
        Gl.vertexAttribPointer(
            ~context,
            ~attribute=attrib.attribLocation,
            ~size=attrib.size,
            ~type_=Constants.float_,
            ~normalize=false,
            ~stride=0,
            ~offset=0
        );
        Gl.enableVertexAttribArray(~context, ~attribute=attrib.attribLocation);
    }, program.attributes);
    switch (program.elementBuffer) {
    | Some(elementBuffer) => {
        /* Draw element buffer */
        Gl.bindBuffer(
            ~context,
            ~target=Constants.element_array_buffer,
            ~buffer=elementBuffer.bufferRef
        );
        Gl.drawElements(
            ~context,
            ~mode=Constants.triangles,
            ~count=Array.length(elementBuffer.data),
            ~type_=Constants.unsigned_short,
            ~offset=0
        );
    }
    | None => {
        /* Draw vertex buffer */
        Gl.bindBuffer(
            ~context,
            ~target=Constants.element_array_buffer,
            ~buffer=program.vertexBuffer.bufferRef
        );
        Gl.drawArrays(
            ~context,
            ~mode=Constants.triangles,
            ~count=Array.length(program.vertexBuffer.data),
            ~first=0
        );
    }
    }
};

