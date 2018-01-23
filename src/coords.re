module Mat3 {
    type t = array(float);
    let asArray = (mat: t) : array(float) => {
        mat
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
    let scaleMat = (scaleX, scaleY) : t => {
        [|
            scaleX, 0., 0.,
            0., scaleY, 0.,
            0., 0., 1.
        |]
    };

    let transMat = (translateX, translateY) : t => {
        [|
            1., 0., translateX,
            0., 1., translateY,
            0., 0., 1.
        |]
    };

    let idMat = () : t => {
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
    let scaleMat = (scaleX, scaleY, scaleZ) : t => {
        [|
            scaleX, 0., 0., 0.,
            0., scaleY, 0., 0.,
            0., 0., scaleZ, 0.,
            0., 0., 0., 1.
        |]
    };

    let transMat = (translateX, translateY, translateZ) : t => {
        [|
            1., 0., 0., translateX,
            0., 1., 0., translateY,
            0., 0., 1., translateZ,
            0., 0., 0., 1.
        |]
    };

    let idMat = () : t => {
        [|
            1., 0., 0., 0.,
            0., 1., 0., 0.,
            0., 0., 1., 0.,
            0., 0., 0., 1.
        |]
    };
};

type boardCoords = {
    pixelWidth: float,
    pixelHeight: float,
    scaleX: float,
    scaleY: float,
    glWidth: float,
    glHeight: float,
    glX: float,
    glY: float,
    mat: Mat3.t
};
let getBoardCoords = (canvas : Gpu.Canvas.t) => {
    /* todo: with room for other elements */
    let vpWidth = float_of_int(canvas.width);
    let vpHeight = float_of_int(canvas.height);
    let vpAspect = vpWidth /. vpHeight;
    let boardAspect = 12.0 /. 26.0;
    let paddingPercent = 0.15;
    let (boardWidth, boardHeight) = if (boardAspect < vpAspect) {
        /* Limit by viewport y */
        let y = vpHeight -. (paddingPercent *. vpHeight);
        let x = y *. boardAspect;
        (x, y)
    } else {
        /* Limit by viewport x */
        let x = vpWidth -. (paddingPercent *. vpWidth);
        let y = x /. boardAspect;
        (x, y)
    };
    let glWidth = boardWidth /. vpWidth *. 2.0;
    let glHeight = boardHeight /. vpHeight *. 2.0;
    /*let glX = -1.0 +. (vpWidth *.paddingPercent /. vpWidth);
    let glY = 1.0 -. (vpHeight *.paddingPercent /. vpHeight);*/
    let glX = 0.0;
    let glY = 0.0;
    let scaleX = boardWidth /. vpWidth;
    let scaleY = boardHeight /. vpHeight;
    let scaleMat = Mat3.scaleMat(scaleX, scaleY);
    let transMat = Mat3.transMat(glX, glY);
    let mat = Mat3.matmul(transMat, scaleMat);
    {
        pixelWidth: boardWidth,
        pixelHeight: boardHeight,
        scaleX,
        scaleY,
        glWidth,
        glHeight,
        glX,
        glY,
        mat
    }
};