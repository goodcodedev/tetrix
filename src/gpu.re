module Constants = RGLConstants;
module Gl = Reasongl.Gl;

module Color = {
    type t = {
        r: float,
        g: float,
        b: float
    };

    let from255 = (r, g, b) => {
        {
            r: float_of_int(r) /. 255.0,
            g: float_of_int(g) /. 255.0,
            b: float_of_int(b) /. 255.0
        }
    };

    let toArray = (c) => {
        [|c.r, c.g, c.b|]
    }
};

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

type bufferUsage =
  | StaticDraw
  | DynamicDraw
  | StreamingDraw;

module Uniform = {
    type uniformValue =
      | UniformFloat(float)
      | UniformInt(int)
      | UniformVec2f(array(float))
      | UniformVec3f(array(float))
      | UniformVec4f(array(float))
      ;
    type inited = {
        glType: GlType.t,
        loc: Gl.uniformT
    };

    type t = {
        name: string,
        glType: GlType.t,
        mutable inited: option(inited)
    };

    let make = (name, glType) => {
        name: name,
        glType: glType,
        inited: None
    };

    let init = (uniform, context, program) => {
        let inited = {
            glType: uniform.glType,
            loc: Gl.getUniformLocation(~context, ~program, ~name=uniform.name)
        };
        uniform.inited = Some(inited);
        inited
    };

    let setValue = (uniform, context, uniformValue) => {
        switch (uniformValue) {
        | UniformFloat(value) => Gl.uniform1f(~context, ~location=uniform.loc, ~value)
        | UniformInt(value) => Gl.uniform1i(~context, ~location=uniform.loc, ~value)
        | UniformVec2f(value) => Gl.uniform2f(~context, ~location=uniform.loc, ~v1=value[0], ~v2=value[1])
        | UniformVec3f(value) => Gl.uniform3f(~context, ~location=uniform.loc, ~v1=value[0], ~v2=value[1], ~v3=value[2])
        | UniformVec4f(value) => Gl.uniform4f(~context, ~location=uniform.loc, ~v1=value[0], ~v2=value[1], ~v3=value[2], ~v4=value[3])
        };
    };
};

module VertexAttrib = {
    type t = {
        name: string,
        glType: GlType.t
    };
    type inited = {
        loc: Gl.attributeT,
        size: int,
        stride: int,
        type_: int
    };

    let make = (name, glType) => {
        name: name,
        glType: glType
    };


    let init = (attrib, context, program, size, stride) => {
        let loc = Gl.getAttribLocation(~context, ~program, ~name=attrib.name);
        {
            loc: loc,
            size: size,
            stride: stride,
            type_: GlType.getTypeConst(attrib.glType)
        }
    };

    let setPointer = (inited, context) => {
        Gl.vertexAttribPointer(
            ~context,
            ~attribute=inited.loc,
            ~size=inited.size,
            ~type_=inited.type_,
            ~normalize=false,
            ~stride=inited.stride,
            ~offset=0
        );
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
        programRef: Gl.programT,
        uniforms: array(Uniform.inited)
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
            | Some(programRef) => {
                let uniforms = Array.map((uniform) => {
                    Uniform.init(uniform, context, programRef)
                }, program.uniforms);
                Some({
                    programRef: programRef,
                    uniforms: uniforms
                })
            }
            | None => None
        }
    };

};

module VertexBuffer = {
    type inited = {
        bufferRef: Gl.bufferT,
        attribs: array(VertexAttrib.inited),
        mutable data: array(float),
        mutable update: bool,
        mutable count: int,
        usage: bufferUsage
    };
    type t = {
        data: array(float),
        attributes: array(VertexAttrib.t),
        usage: bufferUsage,
        mutable inited: option(inited)
    };
    let make = (data, attributes, usage) => {
        data: data,
        attributes: attributes,
        usage: usage,
        inited: None
    };
    let makeQuad = () => {
        {
            data: [|
                -1., -1.,
                -1., 1.,
                1., 1.,
                1., -1.
            |],
            attributes: [|
                VertexAttrib.make("position", Vec2f)
            |],
            usage: StaticDraw,
            inited: None
        }
    };
    let updateData = (inited : inited, context) => {
        inited.count = Array.length(inited.data);
        Gl.bufferData(
            ~context,
            ~target=Constants.array_buffer,
            ~data=Gl.Bigarray.of_array(Gl.Bigarray.Float32, inited.data),
            ~usage=switch (inited.usage) {
            | StaticDraw => Constants.static_draw
            | DynamicDraw => Constants.dynamic_draw
            | StreamingDraw => Constants.stream_draw
            }
        );
    };

    let init = (buffer, context, program) => {
        switch (buffer.inited) {
        | Some(inited) => inited
        | None => {
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
                ~usage=switch (buffer.usage) {
                | StaticDraw => Constants.static_draw
                | DynamicDraw => Constants.dynamic_draw
                | StreamingDraw => Constants.stream_draw
                }
            );
            let stride = ref(0);
            let locs = Array.map((attrib : VertexAttrib.t) => {
                let size = GlType.getSize(attrib.glType);
                let loc = VertexAttrib.init(attrib, context, program, size, stride^);
                stride := stride^ + (size * GlType.getBytes(attrib.glType));
                loc
            }, buffer.attributes);
            let inited = {
                bufferRef: vertexBuffer,
                attribs: locs,
                data: buffer.data,
                update: false,
                count: Array.length(buffer.data),
                usage: buffer.usage
            };
            buffer.inited = Some(inited);
            inited
        }
        }
    };
};

module IndexBuffer = {
    type inited = {
        elBufferRef: Gl.bufferT,
        mutable data: array(int),
        mutable update: bool,
        mutable count: int,
        usage: bufferUsage
    };
    type t = {
        data: array(int),
        usage: bufferUsage,
        mutable inited: option(inited)
    };
    let make = (data, usage) => {
        data: data,
        usage: usage,
        inited: None
    };
    let makeQuad = () => {
        data: [|
            0, 1, 2,
            0, 2, 3
        |],
        usage: StaticDraw,
        inited: None
    };
    let makeQuadsData = (num) => {
        /* Assuming clockwise orientation */
        let rec quadData = (quadNum) => {
            if (quadNum >= num) {
                []
            } else {
                let offset = 4 * quadNum;
                [[|
                    0 + offset, 1 + offset,
                    2 + offset, 0 + offset,
                    2 + offset, 3 + offset
                |], ...quadData(quadNum + 1)]
            }
        };
        Array.concat(quadData(0))
    };
    let updateData = (inited : inited, context) => {
        inited.count = Array.length(inited.data);
        Gl.bufferData(
            ~context,
            ~target=Constants.element_array_buffer,
            ~data=Gl.Bigarray.of_array(Gl.Bigarray.Uint16, inited.data),
            ~usage=switch (inited.usage) {
            | StaticDraw => Constants.static_draw
            | DynamicDraw => Constants.dynamic_draw
            | StreamingDraw => Constants.stream_draw
            }
        );
    };
    let init = (buffer, context) => {
        switch (buffer.inited) {
        | Some(inited) => inited
        | None => {
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
            let inited = {
                elBufferRef: bufferRef,
                data: buffer.data,
                update: false,
                count: Array.length(buffer.data),
                usage: buffer.usage
            };
            buffer.inited = Some(inited);
            inited
        }
        }
    };
};

module Texture = {

    type format =
      | Luminance
      | RGBA;

    type inited = {
        texRef: Gl.textureT,
        width: int,
        height: int,
        data: array(int),
        mutable update: bool,
        format: int
    };

    type t = {
        width: int,
        height: int,
        data: option(array(int)),
        format: format,
        mutable inited: option(inited)
    };

    let make = (width, height, data, format) => {
        {
            width: width,
            height: height,
            data: data,
            format: format,
            inited: None
        }
    };

    let luminance = 6409;
    
    [@bs.send]
    external _texImage2D :
    (
      ~context: Gl.contextT,
      ~target: int,
      ~level: int,
      ~internalFormat: int,
      ~width: int,
      ~height: int,
      ~border: int,
      ~format: int,
      ~type_: int,
      ~data: Gl.Bigarray.t('a, 'b)
    ) =>
    unit =
    "texImage2D";

    let unpackAlignment = 3317;

    [@bs.send]
    external _pixelStorei : (
        ~context: Gl.contextT,
        int,
        int
    ) => unit = "pixelStorei";

    let init = (texture : t, context) => {
        switch (texture.inited) {
        | Some(inited) => inited
        | None => {
            let texRef = Gl.createTexture(~context);
            Gl.bindTexture(
                ~context,
                ~target=Constants.texture_2d,
                ~texture=texRef
            );
            Gl.texParameteri(
                ~context,
                ~target=Constants.texture_2d,
                ~pname=Constants.texture_wrap_s,
                ~param=Constants.clamp_to_edge
            );
            Gl.texParameteri(
                ~context,
                ~target=Constants.texture_2d,
                ~pname=Constants.texture_wrap_t,
                ~param=Constants.clamp_to_edge
            );
            Gl.texParameteri(
                ~context,
                ~target=Constants.texture_2d,
                ~pname=Constants.texture_min_filter,
                ~param=Constants.nearest
            );
            Gl.texParameteri(
                ~context,
                ~target=Constants.texture_2d,
                ~pname=Constants.texture_mag_filter,
                ~param=Constants.nearest
            );
            let format = switch (texture.format) {
            | RGBA => Constants.rgba
            | Luminance => luminance
            };
            /* Luminance format gives 1 value per pixel repeated for rgba */
            switch (texture.format) {
            | Luminance => {
                _pixelStorei(~context, unpackAlignment, 1);
            }
            | _ => ()
            };
            let data = switch (texture.data) {
            | Some(data) => {
                _texImage2D(
                    ~context,
                    ~target=Constants.texture_2d,
                    ~level=0,
                    ~internalFormat=format,
                    ~width=texture.width,
                    ~height=texture.height,
                    ~border=0,
                    ~format,
                    ~type_=RGLConstants.unsigned_byte,
                    ~data=Gl.Bigarray.of_array(Gl.Bigarray.Uint8, data)
                );
                data
            }
            | None => [||]
            };
            let inited = {
                texRef: texRef,
                width: texture.width,
                height: texture.height,
                data: data,
                update: false,
                format: format
            };
            texture.inited = Some(inited);
            inited
        }
        }
    };
    let initOrBind = (texture : t, context) => {
        switch (texture.inited) {
        | Some(inited) => {
            Gl.bindTexture(
                ~context,
                ~target=Constants.texture_2d,
                ~texture=inited.texRef
            );
            inited
        }
        | None => {
            /* Will also bind */
            init(texture, context)
        }
        }
    };
    let updateData = (inited : inited, context) => {
        Gl.bindTexture(
            ~context,
            ~target=Constants.texture_2d,
            ~texture=inited.texRef
        );
        _texImage2D(
            ~context,
            ~target=Constants.texture_2d,
            ~level=0,
            ~internalFormat=inited.format,
            ~width=inited.width,
            ~height=inited.height,
            ~border=0,
            ~format=inited.format,
            ~type_=RGLConstants.unsigned_byte,
            ~data=Gl.Bigarray.of_array(Gl.Bigarray.Uint8, inited.data)
        );
        inited.update = false;
    };
};

module ProgramTexture {
    type inited = {
        texture: Texture.inited,
        uniformRef: Gl.uniformT
    };

    type t = {
        uniformName: string,
        texture: Texture.t,
        mutable inited: option(inited)
    };

    let make = (uniformName, texture) => {
        {
            uniformName: uniformName,
            texture: texture,
            inited: None
        }
    };
    let init = (programTexture, context, program) => {
        switch (programTexture.inited) {
        | Some(inited) => inited
        | None => {
            let tInit = Texture.init(programTexture.texture, context);
            let uRef = Gl.getUniformLocation(
                ~context,
                ~program=program,
                ~name=programTexture.uniformName
            );
            let inited = {
                texture: tInit,
                uniformRef: uRef
            };
            programTexture.inited = Some(inited);
            inited
        }
        }
    };
};

module FrameBuffer = {
    type t = {
        width: int,
        height: int
    };

    type frameBufferT;
    let frameBufferC = 36160;
    let color_attachment0 = 36064;

    [@bs.send]
    external _createFramebuffer : (~context : Gl.contextT) => frameBufferT = "createFramebuffer";
    [@bs.send]
    external _bindFramebuffer: (~context: Gl.contextT, int, Js.Nullable.t(frameBufferT)) => unit = "bindFramebuffer";
    [@bs.send]
    external _framebufferTexture2d: (~context: Gl.contextT, int, int, int, Gl.textureT, int) => unit = "framebufferTexture2D";

    type inited = {
        width: int,
        height: int,
        frameBufferRef: frameBufferT
    };
    let make = (width, height) => {
        {
            width: width,
            height: height
        }
    };

    let init = (frameBuffer : t, context) => {
        /* Setup framebuffer */
        let frameBufferRef = _createFramebuffer(~context);
        {
            width: frameBuffer.width,
            height: frameBuffer.height,
            frameBufferRef: frameBufferRef
        }
    };

    let bindTexture = (frameBuffer: inited, context, texture) => {
        let texInit = Texture.init(texture, context);
        _bindFramebuffer(~context, frameBufferC, Js.Nullable.return(frameBuffer.frameBufferRef));
        _framebufferTexture2d(~context, frameBufferC, color_attachment0, Constants.texture_2d, texInit.texRef, 0);
    };
};

module Canvas = {
    type keyboardT = {
        mutable keyCode: Reasongl.Gl.Events.keycodeT
    };

    type t = {
        window: Gl.Window.t,
        context: Gl.contextT,
        width: int,
        height: int,
        mutable currProgram: option(Program.inited),
        mutable currVertexBuffer: option(VertexBuffer.inited),
        mutable currIndexBuffer: option(IndexBuffer.inited),
        mutable currTextures: array(ProgramTexture.inited),
        keyboard: keyboardT,
        mutable deltaTime: float
    };
    let init = (width, height) => {
        let window = Gl.Window.init(~argv=[||]);
        Gl.Window.setWindowSize(~window, ~width, ~height);
        let context = Gl.Window.getContext(window);
        Gl.viewport(~context, ~x=0, ~y=0, ~width, ~height);
        {
            window,
            context,
            width,
            height,
            currProgram: None,
            currVertexBuffer: None,
            currIndexBuffer: None,
            currTextures: [||],
            keyboard: {
                keyCode: Gl.Events.Nothing
            },
            deltaTime: 0.0
        }
    };

    let run = (width, height, setup, draw, ~keyPressed=?, ()) => {
        let canvas = init(width, height);
        let userState = ref(setup(canvas));
        /* Start render loop */
        Gl.render(
            ~window = canvas.window,
            ~displayFunc = (f) => {
                canvas.deltaTime = f /. 1000.;
                userState := draw(userState^, canvas);
            },
            ~keyDown = (~keycode, ~repeat) => {
                canvas.keyboard.keyCode = keycode;
                if (!repeat) {
                    switch (keyPressed) {
                    | Some(keyPressed) => {
                        userState := keyPressed(userState^, canvas);
                    }
                    | None => ()
                    }
                };
            },
            ~keyUp = (~keycode) => {
                /* Need this to trigger cleaning of keyes pressed
                   and repeat marked */
                ()
            },
            ()
        );
    };

    let clear = (canvas, r, g, b) => {
        Gl.clearColor(~context=canvas.context, ~r, ~g, ~b, ~a=1.);
        Gl.clear(~context=canvas.context, ~mask=Constants.color_buffer_bit);
    };

    let setFramebuffer = (canvas, frameBuffer : FrameBuffer.inited) => {
        FrameBuffer._bindFramebuffer(~context=canvas.context, FrameBuffer.frameBufferC, Js.Nullable.return(frameBuffer.frameBufferRef));
        Gl.viewport(~context=canvas.context, ~x=0, ~y=0, ~width=frameBuffer.width, ~height=frameBuffer.height);
    };

    let clearFramebuffer = (canvas) => {
        FrameBuffer._bindFramebuffer(~context=canvas.context, FrameBuffer.frameBufferC, Js.Nullable.null);
        Gl.viewport(~context=canvas.context, ~x=0, ~y=0, ~width=canvas.width, ~height=canvas.height);
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
            Array.iter((attrib : VertexAttrib.inited) => {
                VertexAttrib.setPointer(attrib, context);
                Gl.enableVertexAttribArray(~context, ~attribute=attrib.loc);
            }, vertexBuffer.attribs);
        }
        };
        Gl.drawArrays(
            ~context,
            ~mode=Constants.triangles,
            ~count=vertexBuffer.count,
            ~first=0
        );
    };

    let drawIndexes = (canvas, program, uniforms, vertexBuffer, indexBuffer, textures) => {
        let context = canvas.context;
        switch (canvas.currProgram) {
        | Some(currentProgram) when (currentProgram === program) => ()
        | _ => {
            Gl.useProgram(~context, program.programRef);
            canvas.currProgram = Some(program);
        }
        };
        /* Set uniforms */
        if (Array.length(uniforms) != Array.length(program.uniforms)) {
            failwith("Different number of uniforms and uniform values");
        };
        Array.iteri((i, uniform) => {
            Uniform.setValue(uniform, context, uniforms[i]);
        }, program.uniforms);
        /* Vertex buffer */
        switch (canvas.currVertexBuffer) {
        | Some(currBuffer) when (currBuffer === vertexBuffer) => ()
        | _ => {
            Gl.bindBuffer(
                ~context,
                ~target=Constants.array_buffer,
                ~buffer=vertexBuffer.bufferRef
            );
            if (vertexBuffer.update) {
                VertexBuffer.updateData(vertexBuffer, context);
            };
            Array.iter((attrib : VertexAttrib.inited) => {
                VertexAttrib.setPointer(attrib, context);
                Gl.enableVertexAttribArray(~context, ~attribute=attrib.loc);
            }, vertexBuffer.attribs);
            canvas.currVertexBuffer = Some(vertexBuffer);
        }
        };
        /* Index buffer */
        switch (canvas.currIndexBuffer) {
        | Some(currBuffer) when (currBuffer === indexBuffer) => ()
        | _ => {
            Gl.bindBuffer(
                ~context,
                ~target=Constants.element_array_buffer,
                ~buffer=indexBuffer.elBufferRef
            );
            if (indexBuffer.update) {
                IndexBuffer.updateData(indexBuffer, context);
            };
            canvas.currIndexBuffer = Some(indexBuffer);
        }
        };
        /* Textures */
        let tex0 = Constants.texture0;
        let currTexLength = Array.length(canvas.currTextures);
        canvas.currTextures = Array.mapi((i, pInit : ProgramTexture.inited) => {
            let wasBound = if (i > currTexLength - 1 || canvas.currTextures[i] !== pInit) {
                Gl.uniform1i(~context, ~location=pInit.uniformRef, ~value=i);
                Gl.activeTexture(~context, tex0 + i);
                Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture=pInit.texture.texRef);
                true
            } else {
                false
            };
            if (pInit.texture.update) {
                if (!wasBound) {
                    Gl.activeTexture(~context, tex0 + i);
                    Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture=pInit.texture.texRef);
                };
                Texture.updateData(pInit.texture, context);
            };
            pInit
        }, textures);
        /* Draw elements */
        Gl.drawElements(
            ~context,
            ~mode=Constants.triangles,
            ~count=indexBuffer.count,
            ~type_=Constants.unsigned_short,
            ~offset=0
        );
    };
};

module DrawState = {
    type t = {
        uniforms: array(Uniform.uniformValue),
        program: Program.inited,
        vertexBuffer: VertexBuffer.inited,
        indexBuffer: option(IndexBuffer.inited),
        textures: array(ProgramTexture.inited)
    };

    let init = (context, program, uniforms, vertexes, indexes, textures) => {
        let pInited = Program.init(program, context);
        switch (pInited) {
        | Some(program) => {
            let iBuffer = VertexBuffer.init(vertexes, context, program.programRef);
            let iIndexes = IndexBuffer.init(indexes, context);
            let iTextures = Array.map((programTex : ProgramTexture.t) => {
                let pInit = ProgramTexture.init(programTex, context, program.programRef);
                pInit
            }, textures);
            {
                uniforms: uniforms,
                program: program,
                vertexBuffer: iBuffer,
                indexBuffer: Some(iIndexes),
                textures: iTextures
            }
        }
        | None => failwith("Program creation failed");
        };
    };

    let draw = (drawState, canvas) => {
        switch (drawState.indexBuffer) {
        | Some(indexBuffer) => {
            Canvas.drawIndexes(
                canvas,
                drawState.program,
                drawState.uniforms,
                drawState.vertexBuffer,
                indexBuffer,
                drawState.textures
            );
        }
        | None => {
            Canvas.drawVertices(
                canvas,
                drawState.program,
                drawState.vertexBuffer
            );
        }
        }
    };
};