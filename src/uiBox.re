let sdfDist = {|
    point -= 0.5;
    float boxWidth = 0.3;
    float boxHeight = 0.3;
    float boxDepth = 0.2;
    float rounding = 0.1;
    float box = length(max(abs(point) - vec3(boxWidth, boxHeight, boxDepth), vec3(0.0, 0.0, 0.0))) - rounding;
    return box;
|};

open Gpu;

let makeSdfProgram = (canvas: Canvas.t, model) => {
    let sdfProgram = SdfProgram.make(sdfDist, SdfProgram.ZeroToOne, Some(model));
    SdfProgram.init(sdfProgram, canvas)
};
