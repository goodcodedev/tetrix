module Vec2 {
    type t = array(float);

    let asArray = (v : t) : array(float) => {
        v
    };

    let fromArray = (a : array(float)) : t => {
        if (Array.length(a) != 2) {
            failwith("Array does not have 2 elements for vec2");
        };
        a
    };

    let make = (a, b) => {
        [|a, b|]
    };

    let zeros = () : t => {
        [|0.0, 0.0|]
    };

    let toGlsl = (v : t) => {
        "vec2(" ++ string_of_float(v[0]) ++ "," ++ string_of_float(v[1]) ++ ")"
    }
};

module Vec3 {
    type t = array(float);

    let asArray = (v : t) : array(float) => {
        v
    };

    let make = (a, b, c) => {
        [|a, b, c|]
    };

    let fromArray = (a : array(float)) : t => {
        if (Array.length(a) != 3) {
            failwith("Array does not have 3 elements for vec3");
        };
        a
    };

    let zeros = () : t => {
        [|0.0, 0.0, 0.0|]
    };

    let toGlsl = (v : t) => {
        "vec3(" ++ string_of_float(v[0]) ++ "," ++ string_of_float(v[1]) ++ "," ++ string_of_float(v[2]) ++ ")"
    }
};

module Vec4 {
    type t = array(float);

    let asArray = (v : t) : array(float) => {
        v
    };

    let make = (a, b, c, d) => {
        [|a, b, c, d|]
    };

    let zeros = () : t => {
        [|0.0, 0.0, 0.0, 0.0|]
    };

    let fromArray = (a : array(float)) : t => {
        if (Array.length(a) != 4) {
            failwith("Array does not have 4 elements for vec4");
        };
        a
    };

    let toGlsl = (v : t) => {
        "vec4("
            ++ string_of_float(v[0]) ++ ","
            ++ string_of_float(v[1]) ++ ","
            ++ string_of_float(v[2]) ++ ","
            ++ string_of_float(v[3]) ++ ")"
    }
};

module Mat3 {
    type t = array(float);
    let asArray = (mat: t) : array(float) => {
        mat
    };
    let fromArray = (a : array(float)) : t => {
        if (Array.length(a) != 9) {
            failwith("Array does not have 9 elements for mat3");
        };
        a
    };
    let matmul = (mat1 : t, mat2 : t) : t => {
        [|
            mat1[0] *. mat2[0] +. mat1[1] *. mat2[3] +. mat1[2] *. mat2[6],
            mat1[0] *. mat2[1] +. mat1[1] *. mat2[4] +. mat1[2] *. mat2[7],
            mat1[0] *. mat2[2] +. mat1[1] *. mat2[5] +. mat1[2] *. mat2[8],
            mat1[3] *. mat2[0] +. mat1[4] *. mat2[3] +. mat1[5] *. mat2[6],
            mat1[3] *. mat2[1] +. mat1[4] *. mat2[4] +. mat1[5] *. mat2[7],
            mat1[3] *. mat2[2] +. mat1[4] *. mat2[5] +. mat1[5] *. mat2[8],
            mat1[6] *. mat2[0] +. mat1[7] *. mat2[3] +. mat1[8] *. mat2[6],
            mat1[6] *. mat2[1] +. mat1[7] *. mat2[4] +. mat1[8] *. mat2[7],
            mat1[6] *. mat2[2] +. mat1[7] *. mat2[5] +. mat1[8] *. mat2[8]
        |]
    };
    let scale = (scaleX, scaleY) : t => {
        [|
            scaleX, 0., 0.,
            0., scaleY, 0.,
            0., 0., 1.
        |]
    };

    let trans = (translateX, translateY) : t => {
        [|
            1., 0., translateX,
            0., 1., translateY,
            0., 0., 1.
        |]
    };

    let id = () : t => {
        [|
            1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.
        |]
    };
};

module Mat4 {
    type t = array(float);
    let asArray = (mat: t) : array(float) => {
        mat
    };
    let matmul = (mat1 : t, mat2 : t) : t => {
        [|
            mat1[0] *. mat2[0] +. mat1[1] *. mat2[4] +. mat1[2] *. mat2[8] +. mat1[3] *. mat2[12],
            mat1[0] *. mat2[1] +. mat1[1] *. mat2[5] +. mat1[2] *. mat2[9] +. mat1[3] *. mat2[13],
            mat1[0] *. mat2[2] +. mat1[1] *. mat2[6] +. mat1[2] *. mat2[10] +. mat1[3] *. mat2[14],
            mat1[0] *. mat2[3] +. mat1[1] *. mat2[7] +. mat1[2] *. mat2[11] +. mat1[3] *. mat2[15],

            mat1[4] *. mat2[0] +. mat1[5] *. mat2[4] +. mat1[6] *. mat2[8] +. mat1[7] *. mat2[12],
            mat1[4] *. mat2[1] +. mat1[5] *. mat2[5] +. mat1[6] *. mat2[9] +. mat1[7] *. mat2[13],
            mat1[4] *. mat2[2] +. mat1[5] *. mat2[6] +. mat1[6] *. mat2[10] +. mat1[7] *. mat2[14],
            mat1[4] *. mat2[3] +. mat1[5] *. mat2[7] +. mat1[6] *. mat2[11] +. mat1[7] *. mat2[15],
            
            mat1[8] *. mat2[0] +. mat1[9] *. mat2[3] +. mat1[10] *. mat2[8] +. mat1[11] *. mat2[12],
            mat1[8] *. mat2[1] +. mat1[9] *. mat2[4] +. mat1[10] *. mat2[9] +. mat1[11] *. mat2[13],
            mat1[8] *. mat2[2] +. mat1[9] *. mat2[5] +. mat1[10] *. mat2[10] +. mat1[11] *. mat2[14],
            mat1[8] *. mat2[3] +. mat1[9] *. mat2[5] +. mat1[10] *. mat2[11] +. mat1[11] *. mat2[15],

            mat1[12] *. mat2[0] +. mat1[13] *. mat2[3] +. mat1[14] *. mat2[8] +. mat1[15] *. mat2[12],
            mat1[12] *. mat2[1] +. mat1[13] *. mat2[4] +. mat1[14] *. mat2[9] +. mat1[15] *. mat2[13],
            mat1[12] *. mat2[2] +. mat1[13] *. mat2[5] +. mat1[14] *. mat2[10] +. mat1[15] *. mat2[14],
            mat1[12] *. mat2[3] +. mat1[13] *. mat2[5] +. mat1[14] *. mat2[11] +. mat1[15] *. mat2[15]
        |]
    };
    let scale = (scaleX, scaleY, scaleZ) : t => {
        [|
            scaleX, 0., 0., 0.,
            0., scaleY, 0., 0.,
            0., 0., scaleZ, 0.,
            0., 0., 0., 1.
        |]
    };

    let trans = (translateX, translateY, translateZ) : t => {
        [|
            1., 0., 0., translateX,
            0., 1., 0., translateY,
            0., 0., 1., translateZ,
            0., 0., 0., 1.
        |]
    };

    let id = () : t => {
        [|
            1., 0., 0., 0.,
            0., 1., 0., 0.,
            0., 0., 1., 0.,
            0., 0., 0., 1.
        |]
    };
};