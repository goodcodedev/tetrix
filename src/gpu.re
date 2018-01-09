module Constants = RGLConstants;
module Gl = Reasongl.Gl;


module GlType = {
    type t =
      | Float
      | Int
      | Vec2f
      | Vec3f
      | Vec4f
      | Mat3f
      | Mat4f;
    let getSize = (glType) => {
        switch (glType) {
        | Float | Int => 1
        | Vec2f => 2
        | Vec3f => 3
        | Vec4f => 4
        | Mat3f => 9
        | Mat4f => 16
        }
    };
    let getBytes = (glType) => {
        switch (glType) {
        | Float | Vec2f | Vec3f | Vec4f
        | Mat3f | Mat4f => 4
        | Int => 2
        }
    };
    let getTypeConst = (glType) => {
        switch (glType) {
        | Float | Vec2f | Vec3f | Vec4f
        | Mat3f | Mat4f => Constants.float_
        | Int => Constants.unsigned_short
        }
    };
};

type uniformValue =
  | UniformFloat(float)
  | UniformInt(int)
  | UniformVec2f(array(float))
  | UniformVec3f(array(float))
  | UniformVec4f(array(float))
  ;

type bufferUsage =
  | StaticDraw
  | DynamicDraw
  | StreamingDraw;

module Uniform = {
    type t = {
        name: string,
        glType: GlType.t
    };

    let make = (name, glType) => {
        name: name,
        glType: glType
    };
};

module VertexAttrib = {
    type t = {
        name: string,
        glType: GlType.t
    };

    let make = (name, glType) => {
        name: name,
        glType: glType
    };


    let init = (attrib, context, program, size, stride) => {
        let loc = Gl.getAttribLocation(~context, ~program, ~name=attrib.name);
        Gl.vertexAttribPointer(
            ~context,
            ~attribute=loc,
            ~size,
            ~type_=GlType.getTypeConst(attrib.glType),
            ~normalize=false,
            ~stride,
            ~offset=0
        );
        loc
    };
};

module Shader = {
    type t = {
        source: string
    };

    let make = (source) => {
        source: source
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
};

module Program = {
    type t = {
        vertexShader: Shader.t,
        fragmentShader: Shader.t,
        uniforms: array(Uniform.t)
    };

    type inited = {
        programRef: Gl.programT
    };

    let make = (vertexShader, fragmentShader, uniforms) => {
        vertexShader: vertexShader,
        fragmentShader: fragmentShader,
        uniforms: uniforms
    };

    let linkProgram = (context, vertexSource, fragmentSource) => {
        let vShader = Shader.compileShader(context, Constants.vertex_shader, vertexSource);
        let fShader = Shader.compileShader(context, Constants.fragment_shader, fragmentSource);
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

    let init = (program, context) => {
        switch (linkProgram(
            context,
            program.vertexShader.source,
            program.fragmentShader.source
        )) {
            | Some(programRef) => Some({
                programRef: programRef
            })
            | None => None
        }
    };

};

module VertexBuffer = {
    type t = {
        data: array(float),
        attributes: array(VertexAttrib.t),
        usage: bufferUsage
    };
    type inited = {
        bufferRef: Gl.bufferT,
        attribLocs: array(Gl.attributeT),
        count: int
    };
    let make = (data, attributes, usage) => {
        data: data,
        attributes: attributes,
        usage: usage
    };
    let makeQuad = () => {
        data: [|
            -1., -1.,
            -1., 1.,
            1., 1.,
            1., -1.
        |],
        attributes: [|
            VertexAttrib.make("position", Vec2f)
        |],
        usage: StaticDraw
    };

    let init = (buffer, context, program) => {
        let vertexBuffer = Gl.createBuffer(~context);
        Gl.bindBuffer(
            ~context,
            ~target=Constants.array_buffer,
            ~buffer=vertexBuffer
        );
        Gl.bufferData(
            ~context,
            ~target=Constants.array_buffer,
            ~data=Gl.Bigarray.of_array(Gl.Bigarray.Float32, buffer.data),
            ~usage=switch buffer.usage {
            | StaticDraw => Constants.static_draw
            | DynamicDraw => Constants.dynamic_draw
            | StreamingDraw => Constants.stream_draw
            }
        );
        let stride = ref(0);
        let locs = Array.map((attrib) => {
            Js.log(attrib);
            let loc = VertexAttrib.init(attrib, context, program, 2, stride^);
            loc
        }, buffer.attributes);
        {
            bufferRef: vertexBuffer,
            attribLocs: locs,
            count: Array.length(buffer.data)
        }
    };
};

module IndexBuffer = {
    type t = {
        data: array(int),
        usage: bufferUsage
    };
    type inited = {
        elBufferRef: Gl.bufferT,
        count: int
    };
    let make = (data, usage) => {
        data: data,
        usage: usage
    };
    let makeQuad = () => {
        data: [|
            0, 1, 2,
            0, 2, 3
        |],
        usage: StaticDraw
    };
    let init = (buffer, context) => {
        let bufferRef = Gl.createBuffer(~context);
        Gl.bindBuffer(
            ~context,
            ~target=Constants.element_array_buffer,
            ~buffer=bufferRef
        );
        Gl.bufferData(
            ~context,
            ~target=Constants.element_array_buffer,
            ~data=Gl.Bigarray.of_array(Gl.Bigarray.Uint16, buffer.data),
            ~usage=switch buffer.usage {
            | StaticDraw => Constants.static_draw
            | DynamicDraw => Constants.dynamic_draw
            | StreamingDraw => Constants.stream_draw
            }
        );
        {
            elBufferRef: bufferRef,
            count: Array.length(buffer.data)
        }
    };
};

module DrawState = {
    type t = {
        uniforms: array(uniformValue)
    };
};


module Canvas = {
    type t = {
        window: Gl.Window.t,
        context: Gl.contextT,
        mutable currProgram: option(Program.inited),
        mutable currVertexBuffer: option(VertexBuffer.inited),
        mutable currIndexBuffer: option(IndexBuffer.inited)
    };
    let init = (width, height) => {
        let window = Gl.Window.init(~argv=[||]);
        Gl.Window.setWindowSize(~window, ~width, ~height);
        let context = Gl.Window.getContext(window);
        Gl.viewport(~context, ~x=0, ~y=0, ~width, ~height);
        {
            window,
            context,
            currProgram: None,
            currVertexBuffer: None,
            currIndexBuffer: None
        }
    };

    let clear = (canvas, r, g, b) => {
        Gl.clearColor(~context=canvas.context, ~r, ~g, ~b, ~a=1.);
        Gl.clear(~context=canvas.context, ~mask=Constants.color_buffer_bit);
    };

    let drawVertices = (canvas, program, vertexBuffer) => {
        let context = canvas.context;
        switch (canvas.currProgram) {
        | Some(currentProgram) when (currentProgram == program) => ()
        | _ => {
            Gl.useProgram(~context, program.programRef);
            canvas.currProgram = Some(program);
        }
        };
        switch (canvas.currVertexBuffer) {
        | Some(currBuffer) when (currBuffer == vertexBuffer) => ()
        | _ => {
            Gl.bindBuffer(
                ~context,
                ~target=Constants.array_buffer,
                ~buffer=vertexBuffer.bufferRef
            );
            Array.iter((loc) => {
                Gl.enableVertexAttribArray(~context, ~attribute=loc);
            }, vertexBuffer.attribLocs)
        }
        };
        Gl.drawArrays(
            ~context,
            ~mode=Constants.triangles,
            ~count=vertexBuffer.count,
            ~first=0
        );
    };

    let drawIndexes = (canvas, program, vertexBuffer, indexBuffer) => {
        let context = canvas.context;
        switch (canvas.currProgram) {
        | Some(currentProgram) when (currentProgram == program) => ()
        | _ => {
            Gl.useProgram(~context, program.programRef);
            canvas.currProgram = Some(program);
        }
        };
        switch (canvas.currVertexBuffer) {
        | Some(currBuffer) when (currBuffer == vertexBuffer) => ()
        | _ => {
            Gl.bindBuffer(
                ~context,
                ~target=Constants.array_buffer,
                ~buffer=vertexBuffer.bufferRef
            );
            Array.iter((loc) => {
                Gl.enableVertexAttribArray(~context, ~attribute=loc);
            }, vertexBuffer.attribLocs)
        }
        };
        switch (canvas.currIndexBuffer) {
        | Some(currBuffer) when (currBuffer == indexBuffer) => ()
        | _ => {
            Gl.bindBuffer(
                ~context,
                ~target=Constants.element_array_buffer,
                ~buffer=indexBuffer.elBufferRef
            );
        }
        };
        Gl.drawElements(
            ~context,
            ~mode=Constants.triangles,
            ~count=indexBuffer.count,
            ~type_=Constants.unsigned_short,
            ~offset=0
        );
    };
};
