let sdfDist = {|
    point -= 0.5;
    float boxWidth = 0.43;
    float boxHeight = 0.43;
    float boxDepth = 0.2;
    float rounding = 0.03;
    float box = length(max(abs(point) - vec3(boxWidth, boxHeight, boxDepth), vec3(0.0, 0.0, 0.0))) - rounding;
    return box;
|};

open Gpu;

let makeSdfProgram = (canvas: Canvas.t, model, ~color=?, ()) => {
    let sdfNode = SdfNode.make(
        sdfDist,
        SdfNode.ZeroToOne,
        Some(model),
        ~color=?color,
        ~alphaLimit=0.0,
        ~opacity=0.5,
        ()
    );
    SdfNode.init(sdfNode, canvas)
};

let makeNode = (model, ~color=?, ()) => {
    let sdfNode = SdfNode.make(
        sdfDist,
        SdfNode.ZeroToOne,
        Some(model),
        ~color=?color,
        ~alphaLimit=0.0,
        ~opacity=0.5,
        ()
    );
    SdfNode.makeNode(
        sdfNode,
        ~aspect=(12.0 /. 26.0),
        ()
    )
};
