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
    
    float epsilon = 0.0001;
    float minDist = 1.0;
    float maxDist = 200.0;
    const int marchingSteps = 300;

    // Octahedron turned towards z
    float sdfOcta(vec3 point) {
        point = point - vec3(0.5, 0.5, 0.0);
        vec3 h = vec3(0.2, 0.2, 0.1);
        float d = 0.0;
        float size = 0.2;
        d = max( d, abs( dot(point, vec3( -size, 0, size )) ));
        d = max( d, abs( dot(point, vec3(  size, 0, size )) ));
        d = max( d, abs( dot(point, vec3(  0, -size, size )) ));
        d = max( d, abs( dot(point, vec3(  0, size, size )) ));
        float octa = d - size / 2.0;
        return octa;
    }
    
    float sdfDist(vec3 point) {
        float octa2 = sdfOcta(point);
        point = point - vec3(0.5, 0.5, 0.0);
        float box2 = length(max(abs(point) - vec3(0.5, 0.5, 0.1), vec3(0.0, 0.0, 0.0)));
        return max(octa2, box2);

        
        // Repeat x and y for each tile
        point.x = mod(point.x, 1.0 / 14.0) - 1.0 / 28.0;
        point.y = mod(point.y, 1.0 / 28.0) - 1.0 / 56.0;
        float x = 1.0 / 30.5;
        float y = 1.0 / 62.0;
        float box = length(max(abs(point) - vec3(x, y, y), vec3(0.0, 0.0, 0.0)));
        // Pyramid
        vec3 h = vec3(0.03, 0.03, 0.03);
        float d = 0.0;
        d = max( d, abs( dot(point, vec3( -h.x, h.y, 0 )) ));
        d = max( d, abs( dot(point, vec3(  h.x, h.y, 0 )) ));
        d = max( d, abs( dot(point, vec3(  0, h.y, h.x )) ));
        d = max( d, abs( dot(point, vec3(  0, h.y,-h.x )) ));
        float octa = d - h.z;
        return octa;
        return max(-octa,box); // Subtraction
        return length(point) - 1.0 / 28.0;
    }
    
    /*
     * Normalized direction to march in from the eye point
     * for a single pixel.
     */
    vec3 rayDirection(float fieldOfView, vec2 size, vec2 fragCoord) {
        vec2 xy = fragCoord - size / 2.0;
        float z = size.y / tan(radians(fieldOfView) / 2.0);
        return normalize(vec3(xy, -z));
    }
    
    /*
     * Returns a transform matrix that will transform
     * a ray from view space to world coordinates.
     */
    mat3 viewMatrix(vec3 eye, vec3 center, vec3 up) {
        vec3 f = normalize(center - eye);
        vec3 s = cross(f, up);
        vec3 u = cross(s, f);
        return mat3(s, u, -f);
    }
    
    vec3 estimateNormal(vec3 point) {
        return normalize(vec3(
            sdfDist(vec3(point.x + epsilon, point.y, point.z)) - sdfDist(vec3(point.x - epsilon, point.y, point.z)),
            sdfDist(vec3(point.x, point.y + epsilon, point.z)) - sdfDist(vec3(point.x, point.y - epsilon, point.z)),
            sdfDist(vec3(point.x, point.y, point.z + epsilon)) - sdfDist(vec3(point.x, point.y, point.z - epsilon))
        ));
    }
    
    /*
     * Lighting contribution of a single point light source
     * via phong illumination.
     * 
     */
    vec3 phongContribForLight(vec3 diffuse, vec3 specular, float alpha,
            vec3 point, vec3 eye, vec3 lightPos, vec3 lightIntensity) {
        vec3 N = estimateNormal(point);
        vec3 L = normalize(lightPos - point);
        vec3 V = normalize(eye - point);
        vec3 R = normalize(reflect(L * -1.0, N));
        float dotLN = dot(L, N);
        float dotRV = dot(R, V);
        if (dotLN < 0.0) {
            /* Light not visible from this point on the surface */
            return vec3(0.0, 0.0, 0.0);
        }
        if (dotRV < 0.0) {
            /* Light reflection in opposite direction, apply only diffuse */
            return lightIntensity * (diffuse * dotLN);
        }
        return lightIntensity * (diffuse * dotLN + specular * pow(dotRV, alpha));
    }
    
    /*
     * Lighting via phong illumination
     * https://en.wikipedia.org/wiki/Phong_reflection_model#Description
     */
    vec3 phongIllumination(vec3 ambient, vec3 diffuse, vec3 specular, 
            float alpha, vec3 point, vec3 eye) {
        vec3 ambientLight = vec3(0.5, 0.5, 0.5);
        vec3 color = ambientLight * ambient;
        vec3 light1Pos = vec3(4.0,
                                2.0,
                                4.0);
        vec3 light1Intensity = vec3(0.4, 0.4, 0.4);
        color += phongContribForLight(diffuse, specular, alpha,
                                        point, eye,
                                        light1Pos,
                                        light1Intensity);
        vec3 light2Pos = vec3(2.0,
                                2.0,
                                2.0);
        vec3 light2Intensity = vec3(0.4, 0.4, 0.4);
        color += phongContribForLight(diffuse, specular, alpha,
                                        point, eye,
                                        light2Pos,
                                        light2Intensity);
        return color;
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
    
    void main() {
        vec2 viewport = vec2(1.0, 1.0);
        vec2 fragCoord = (vPosition + 1.0) * 0.5;
        vec3 viewDir = rayDirection(60.0, viewport, fragCoord);
        vec3 eye = vec3(0.0, 0.0, 5.0);
        mat3 viewToWorld = viewMatrix(eye, vec3(0.0, 0.0, -1.0), vec3(0.0, 1.0, 0.0));
        vec3 worldDir = viewToWorld * viewDir;
        vec3 pixelEye = vec3(fragCoord, 4.0);
        float dist = shortestDistance(pixelEye, vec3(0.0, 0.0, -1.0));
        if (dist > (maxDist - epsilon)) {
            /* No shape in ray */
            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
            gl_FragColor = vec4(worldDir, 1.0);
        } else {
            vec3 p = pixelEye + dist * vec3(0.0, 0.0, -1.0);
            vec3 alpha = (estimateNormal(p) + vec3(1.0)) / 2.0;
            vec3 diffuse = alpha;
            vec3 specular = vec3(1.0, 1.0, 1.0);
            float shininess = 10.0;
            vec3 color = phongIllumination(alpha, diffuse, specular, shininess, p, pixelEye);
            gl_FragColor = vec4(color, 1.0);
        }
    }
|};

type t = {
    drawState: Gpu.DrawState.t,
    canvas: Gpu.Canvas.t
};

open Gpu;

let createCanvas = () => {
    let canvas = Canvas.init(280, 560);
    let drawState = DrawState.init(
        canvas.context,
        Program.make(
            Shader.make(vertexSource),
            Shader.make(fragmentSource),
            [||]
        ),
        VertexBuffer.makeQuad(),
        IndexBuffer.makeQuad(),
        [||]
    );
    DrawState.draw(drawState, canvas);
    {
        drawState: drawState,
        canvas: canvas
    }
};

let draw = (bp) => {
    DrawState.draw(bp.drawState, bp.canvas);
};