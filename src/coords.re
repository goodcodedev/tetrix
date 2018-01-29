open Data;

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
    let scaleMat = Mat3.scale(scaleX, scaleY);
    let transMat = Mat3.trans(glX, glY);
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