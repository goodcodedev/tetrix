let sdfDist = (cols, rows) => {
    let colsF = float_of_int(cols);
    let rowsF = float_of_int(rows);
    let colsGl = string_of_float(colsF);
    let rowsGl = string_of_float(rowsF);
    {|
        float cols = |} ++ colsGl ++ {|;
        float rows = |} ++ rowsGl ++ {|;
        float cols2 = cols * 2.0;
        float rows2 = rows * 2.0;
        point.x = mod(point.x, 1.0 / cols) - 1.0 / cols2;
        point.y = mod(point.y, 1.0 / rows) - 1.0 / rows2;
        float boxWidth = 1.0 / cols2;
        float boxHeight = 1.0 / rows2;
        float boxDepth = 0.1;
        float box = length(max(abs(point) - vec3(boxWidth, boxHeight, boxDepth), vec3(0.0, 0.0, 0.0)));
        // Octahedron towards z
        /*
        float rot = 20.0;
        mat3 xrot = mat3(
            1.0, 0.0, 0.0,
            0.0, cos(rot), -sin(rot),
            0.0, sin(rot), cos(rot)
        );
        point = point * xrot;
        */
        float d = 0.0;
        // Dont know too much of what I'm doing here..
        // sdpyramid from https://www.shadertoy.com/view/Xds3zN with some modifications
        vec3 octa = vec3(0.5 * boxHeight / boxWidth, 0.5, 0.24);
        d = max( d, abs( dot(point, vec3( -octa.x, 0, octa.z )) ));
        d = max( d, abs( dot(point, vec3(  octa.x, 0, octa.z )) ));
        d = max( d, abs( dot(point, vec3(  0, -octa.y, octa.z )) ));
        d = max( d, abs( dot(point, vec3(  0, octa.y, octa.z )) ));
        // Some spacing added, maybe this should be calibrated to a pixel
        float o = d - octa.z / (rows + rows / 20.0);
        // Intersection
        //return o;
        return max(o, box);
    |}
};

open Gpu;

type t = {
    sdfProgram: SdfNode.inited,
    texture: Texture.t,
    fbuffer: FrameBuffer.inited,
    canvas: Canvas.t
};

let make = (canvas: Canvas.t, lighting) => {
    let sdfProgram = SdfNode.init(SdfNode.make(sdfDist(12, 26), SdfNode.ZeroToOne, None, lighting, ()), canvas);
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

let createCanvas = (lighting) => {
    let canvas = Canvas.init(240, 580);
    let p = make(canvas, lighting);
    SdfNode.draw(p.sdfProgram);
};

let makeNode = (cols, rows, lighting) => {
    let aspect = (float_of_int(cols) /. float_of_int(rows));
    let sdfNode = SdfNode.make(sdfDist(cols, rows), SdfNode.ZeroToOne, None, lighting, ());
    SdfNode.makeNode(
        sdfNode,
        ~key="sdfTiles",
        ~aspect,
        ~drawTo=Scene.TextureRGB,
        ()
    )
};