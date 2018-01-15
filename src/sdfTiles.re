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
    
    float epsilon = 0.00005;
    float minDist = 1.0;
    float maxDist = 5.0;
    const int marchingSteps = 30;
    
    float sdfDist(vec3 point) {
        // Repeat x and y for each tile
        /*
        point.x -= 0.5;
        point.y -= 0.5;
        */
        point.x = mod(point.x, 1.0 / 14.0) - 1.0 / 28.0;
        point.y = mod(point.y, 1.0 / 28.0) - 1.0 / 56.0;
        float boxWidth = 1.0 / 28.0;
        float boxHeight = 1.0 / 56.0;
        float boxDepth = 0.01;
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
        vec3 diffuseDir = vec3(0.4, 0.3, 0.3);
        float NdotD = max(dot(diffuseDir, N), 0.0);
        vec3 pointPos = vec3(0.6, 0.8, 0.4);
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
        vec2 fragCoord = (vPosition + 1.0) * 0.5;
        vec3 eye = vec3(0.0, 0.0, 5.0);
        vec3 pixelEye = vec3(fragCoord, 4.0);
        float dist = shortestDistance(pixelEye, vec3(0.0, 0.0, -1.0));
        // All points should hit a shape in this shader
        vec3 p = pixelEye + dist * vec3(0.0, 0.0, -1.0);
        gl_FragColor = vec4(lighting(p), 1.0);
    }
|};

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
        VertexBuffer.makeQuad(),
        IndexBuffer.makeQuad(),
        [||]
    )
};
let createCanvas = () => {
    let canvas = Canvas.init(280, 560);
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