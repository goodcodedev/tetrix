open Gpu;

type fragCoords =
  | ByModel
  | ZeroToOne;

type t = {
    sdfDist: string,
    fragCoords: fragCoords,
    model: option(Coords.Mat3.t)
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
        varying vec2 vPosition;
        void main() {
            vec2 pos = (vec3(position, 1.0) * model).xy;
            vPosition = pos;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
    | (Some(_model), ZeroToOne) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 model;
        varying vec2 vPosition;
        void main() {
            vec2 pos = (vec3(position, 1.0) * model).xy;
            vPosition = position;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
    | (None, _) => {|
        precision mediump float;
        attribute vec2 position;
        varying vec2 vPosition;
        void main() {
            vPosition = position;
            gl_Position = vec4(position, 0.0, 1.0);
        }
    |}
    }
};

let makeFragmentSource = (self) => {
    let fragCoords = switch (self.model, self.fragCoords) {
    | (Some(_model), ByModel) => "vPosition"
    | (_, ZeroToOne) => "(vPosition + vec2(1.0, -1.0)) * vec2(0.5, -0.5)"
    | (None, ByModel) => failwith("ByModel fragCoords requested, but no model provided")
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

        vec3 lighting(vec3 surfacePoint) {
            vec3 N = estimateNormal(surfacePoint);
            vec3 diffuseDir = vec3(0.4, -0.3, 0.3);
            float NdotD = max(dot(diffuseDir, N), 0.0);
            vec3 pointPos = vec3(0.6, 0.2, 0.4);
            vec3 pointVec = pointPos - surfacePoint;
            vec3 pointDir = normalize(pointVec);
            float NdotP = max(dot(pointDir, N), 0.0);
            float ambient = 0.1;
            float c = NdotD * 0.1 + ambient + NdotP * 0.6 * max(0.0, 1.0 - length(pointVec));
            //c = NdotD;
            //c = NdotP;
            return vec3(c, c, c);
        }
        
        void main() {
            vec2 viewport = vec2(1.0, 1.0);
            vec2 fragCoord = |} ++ fragCoords ++ {|;
            vec3 eye = vec3(0.0, 0.0, 5.0);
            vec3 pixelEye = vec3(fragCoord, 4.0);
            float dist = shortestDistance(pixelEye, vec3(0.0, 0.0, -1.0));
            // All points should hit a shape in this shader
            vec3 p = pixelEye + dist * vec3(0.0, 0.0, -1.0);
            gl_FragColor = vec4(lighting(p), 1.0);
        }
    |};
    source
};

let makeProgram = (self) => {
    let uniforms = switch(self.model) {
    | Some(_model) => [|Uniform.make("model", GlType.Mat3f)|]
    | None => [||]
    };
    Program.make(
        Shader.make(makeVertexSource(self)),
        Shader.make(makeFragmentSource(self)),
        uniforms
    )
};

let makeDrawState = (self, canvas : Canvas.t) => {
    let uniforms = switch(self.model) {
    | Some(model) => [|Uniform.UniformMat3f(model)|]
    | None => [||]
    };
    DrawState.init(
        canvas.context,
        makeProgram(self),
        uniforms,
        VertexBuffer.makeQuad(()),
        IndexBuffer.makeQuad(),
        [||]
    )
};

let draw = (self) => {
    DrawState.draw(self.drawState, self.canvas);
};

let make = (sdfDist, fragCoords, model) => {
    {
        sdfDist,
        fragCoords,
        model
    }
};

let init = (self, canvas : Canvas.t) => {
    {
        self,
        canvas,
        drawState: makeDrawState(self, canvas)
    }
};