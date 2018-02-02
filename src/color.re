type t = array(float);

let from255 = (r, g, b) => {
    [|
        float_of_int(r) /. 255.0,
        float_of_int(g) /. 255.0,
        float_of_int(b) /. 255.0
    |]
};

let fromFloats = (r, g, b) => {
    [|r, g, b|]
};

let toArray = (c : t) : array(float) => {
    c
};

let toVec3 = (c : t) => {
    Data.Vec3.fromArray(toArray(c))
};

let toGlsl = (c : t) => {
    "vec3(" ++ string_of_float(c[0]) ++ "," ++ string_of_float(c[1]) ++ "," ++ string_of_float(c[2]) ++ ")"
};

let black = () : t => {
    fromFloats(0.0, 0.0, 0.0)
};
let white = () : t => {
    fromFloats(1.0, 1.0, 1.0)
};
