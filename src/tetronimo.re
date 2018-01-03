type t = {
    points: list((int, int)),
    points90: list((int, int)),
    points180: list((int, int)),
    points270: list((int, int)),
    colorIndex: int
};

let make = (points, (translateX, translateY), colorIndex) => {
    let rotate90 = ((x, y)) => {
        (y, x * -1)
    };
    let points90 = List.map(rotate90, points);
    let points180 = List.map(rotate90, points90);
    let points270 = List.map(rotate90, points180);
    /* Translate and set upper left corner points */
    let toTilePoints = ((x, y)) => {
        ((x - translateX) / 2 - 1, ((y - translateY) / -2) - 1)
    };
    {
        points: List.map(toTilePoints, points),
        points90: List.map(toTilePoints, points90),
        points180: List.map(toTilePoints, points180),
        points270: List.map(toTilePoints, points270),
        colorIndex
    }
};