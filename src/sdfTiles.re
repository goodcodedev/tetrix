let sdfDist = {|
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
|};

open Gpu;

type t = {
    sdfProgram: SdfNode.inited,
    texture: Texture.t,
    fbuffer: FrameBuffer.inited,
    canvas: Canvas.t
};

let make = (canvas: Canvas.t) => {
    let sdfProgram = SdfNode.init(SdfNode.make(sdfDist, SdfNode.ZeroToOne, None, ()), canvas);
    let texture = Texture.make(IntDataTexture(Array.make(1024*1024*4, 0), 1024, 1024), Texture.RGBA, Texture.LinearFilter);
    let fbuffer = FrameBuffer.init(FrameBuffer.make(1024, 1024), canvas.context);
    {
        sdfProgram,
        texture,
        fbuffer,
        canvas
    }
};

let drawToTexture = (self) => {
    FrameBuffer.bindTexture(self.fbuffer, self.canvas.context, self.texture);
    Canvas.setFramebuffer(self.canvas, self.fbuffer);
    SdfNode.draw(self.sdfProgram);
    Canvas.clearFramebuffer(self.canvas);
};

let createCanvas = () => {
    let canvas = Canvas.init(240, 580);
    let p = make(canvas);
    SdfNode.draw(p.sdfProgram);
};

let makeNode = () => {
    let aspect = (12.0 /. 26.0);
    let sdfNode = SdfNode.make(sdfDist, SdfNode.ZeroToOne, None, ());
    SdfNode.makeNode(
        sdfNode,
        ~key="sdfTiles",
        ~aspect,
        ~drawTo=Scene.TextureRGB,
        ()
    )
};