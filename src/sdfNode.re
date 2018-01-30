open Gpu;

type fragCoords =
  | ByModel
  | ZeroToOne;

type t = {
    sdfDist: string,
    fragCoords: fragCoords,
    width: float,
    height: float,
    model: option(Data.Mat3.t),
    color: Color.t,
    opacity: option(float),
    alphaLimit: option(float),
    lightPos: Data.Vec3.t
};

type inited = {
    self: t,
    drawState: DrawState.t,
    canvas: Canvas.t
};

let makeVertexSource = (self) => {
    switch (self.model, self.fragCoords) {
    | (Some(_model), ByModel) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 model;
        uniform mat3 layout;
        varying vec2 vPosition;
        void main() {
            vec3 toModel = vec3(position, 1.0) * model;
            vec2 pos = (vec3(position, 1.0) * layout).xy;
            vPosition = toModel.xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
    | (Some(_model), ZeroToOne) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 model;
        uniform mat3 layout;
        varying vec2 vPosition;
        void main() {
            vec2 pos = (vec3(position, 1.0) * (layout * model)).xy;
            vPosition = position;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
    | (None, ZeroToOne) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 layout;
        varying vec2 vPosition;
        void main() {
            vPosition = (position + vec2(1.0, -1.0)) * vec2(0.5, -0.5);
            vec2 pos = (vec3(position, 1.0) * layout).xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
    | (None, _) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 layout;
        varying vec2 vPosition;
        void main() {
            vPosition = position;
            vec2 pos = (vec3(position, 1.0) * layout).xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
    }
};

let makeFragmentSource = (self) => {
    let fragCoords = switch (self.model, self.fragCoords) {
    | (Some(_model), ByModel) => "vPosition"
    | (_, ZeroToOne) => "vPosition"
    | (None, ByModel) => failwith("ByModel fragCoords requested, but no model provided")
    };
    let sf = string_of_float;
    let glColor = Color.toGlsl(self.color);
    let glAlpha = switch(self.alphaLimit, self.opacity) {
    | (Some(alphaLimit), Some(opacity)) => "(pixelEye.z - dist < " ++ sf(alphaLimit) ++ ") ? 0.0 : " ++ sf(opacity)
    | (Some(alphaLimit), None) => "(pixelEye.z - dist < " ++ sf(alphaLimit) ++ ") ? 0.0 : 1.0"
    | (None, Some(opacity)) => sf(opacity)
    | (None, None) => "1.0"
    };
    let source = {|
        precision mediump float;
        varying vec2 vPosition;
        
        float epsilon = 0.00005;
        float minDist = 1.0;
        float maxDist = 5.0;
        const int marchingSteps = 30;
        
        float sdfDist(vec3 point) {
            |} ++ self.sdfDist ++ {|
        }
        
        vec3 estimateNormal(vec3 point) {
            return normalize(vec3(
                sdfDist(vec3(point.x + epsilon, point.y, point.z)) - sdfDist(vec3(point.x - epsilon, point.y, point.z)),
                sdfDist(vec3(point.x, point.y + epsilon, point.z)) - sdfDist(vec3(point.x, point.y - epsilon, point.z)),
                sdfDist(vec3(point.x, point.y, point.z + epsilon)) - sdfDist(vec3(point.x, point.y, point.z - epsilon))
            ));
        }
        
        float shortestDistance(vec3 eye, vec3 dir) {
            float depth = minDist;
            float end = maxDist;
            for (int i = 0; i < marchingSteps; i++) {
                float dist = sdfDist(eye + depth * dir);
                if (dist < epsilon) {
                    return depth;
                }
                depth += dist;
                if (depth >= end) {
                    /* Moved beyond end */
                    return end;
                }
            }
            return end;
        }

        float lighting(vec3 surfacePoint) {
            vec3 N = estimateNormal(surfacePoint);
            vec3 diffuseDir = vec3(0.4, -0.3, 0.3);
            float NdotD = max(dot(diffuseDir, N), 0.0);
            vec3 pointPos = |} ++ Data.Vec3.toGlsl(self.lightPos) ++ {|;
            vec3 pointVec = pointPos - surfacePoint;
            vec3 pointDir = normalize(pointVec);
            float NdotP = max(dot(pointDir, N), 0.0);
            float ambient = 0.0;
            float c = NdotD * 0.1 + ambient + NdotP * 0.6 * max(0.0, 1.0 - length(pointVec));
            //c = NdotD;
            //c = NdotP;
            return c;
        }
        
        void main() {
            vec2 viewport = vec2(1.0, 1.0);
            vec2 fragCoord = |} ++ fragCoords ++ {|;
            vec3 eye = vec3(0.0, 0.0, 5.0);
            vec3 pixelEye = vec3(fragCoord, 4.0);
            float dist = shortestDistance(pixelEye, vec3(0.0, 0.0, -1.0));
            // All points should hit a shape in this shader
            vec3 p = pixelEye + dist * vec3(0.0, 0.0, -1.0);
            vec3 color = |} ++ glColor  ++ {|;
            float light = lighting(p);
            color = mix(mix(color, vec3(1.0, 1.0, 1.0), max(light - 0.5, 0.0)) * 0.9, vec3(0.0, 0.0, 0.0), max(0.5 + light * -1.0, 0.0) * 1.5);
            float alpha = |} ++ glAlpha  ++ {|;
            gl_FragColor = vec4(color, alpha);
        }
    |};
    source
};

let makeProgram = (self) => {
    let uniforms = switch(self.model) {
    | Some(model) => [|Uniform.make("model", UniformMat3f(ref(Data.Mat3.fromArray(model))))|]
    | None => [||]
    };
    Program.make(
        Shader.make(makeVertexSource(self)),
        Shader.make(makeFragmentSource(self)),
        uniforms
    )
};

let makeDrawState = (self, canvas : Canvas.t) => {
    DrawState.init(
        canvas.context,
        makeProgram(self),
        VertexBuffer.makeQuad(()),
        IndexBuffer.makeQuad(),
        [||]
    )
};

let draw = (self : inited) => {
    switch (self.self.opacity, self.self.alphaLimit) {
    | (None, None) => DrawState.draw(self.drawState, self.canvas)
    | _ =>
        let context = self.canvas.context;
        Gl.enable(~context, Constants.blend);
        Gl.blendFunc(~context, Constants.src_alpha, Constants.one_minus_src_alpha);
        DrawState.draw(self.drawState, self.canvas);
        Gl.disable(~context, Constants.blend);
    };
};

let make = (
    sdfDist,
    fragCoords,
    model,
    ~width=1.0,
    ~height=1.0,
    ~color=Color.fromFloats(0.6, 0.6, 0.6),
    ~opacity=?,
    ~alphaLimit=?,
    ~lightPos=Data.Vec3.make(0.6, 0.2, 0.4),
    ()
    ) => {
    {
        sdfDist,
        fragCoords,
        width,
        height,
        model,
        color,
        opacity,
        alphaLimit,
        lightPos
    }
};

let init = (self, canvas : Canvas.t) => {
    {
        self,
        canvas,
        drawState: makeDrawState(self, canvas)
    }
};

let makeNode = (
    self,
    ~key="sdfNode",
    ~aspect=?,
    ~drawToTexture=?,
    ~children=[],
    ()
) => {
    let transparent = switch((self.opacity, self.alphaLimit)) {
    | (_, Some(_alphaLimit)) => true
    | (Some(opacity), _) => (opacity < 1.0)
    | _ => false
    };
    let uniforms = switch(self.model) {
    | Some(model) => 
        [
            ("model", UniformMat3f(ref(model))),
        ]
    | None => []
    };
    let size = switch (aspect) {
    | Some(aspect) => Scene.Aspect(aspect)
    | None => Dimensions(Scale(self.width), Scale(self.height))
    };
    Scene.makeNode(
        key,
        ~updateOn=[UpdateFlags.Init],
        ~vertShader=Shader.make(makeVertexSource(self)),
        ~fragShader=Shader.make(makeFragmentSource(self)),
        ~size,
        ~transparent,
        ~uniforms,
        ~layoutUniform=true,
        ~children,
        ~drawToTexture=?drawToTexture,
        ()
    )
};