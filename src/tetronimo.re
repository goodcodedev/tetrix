type t = {
    points: list((int, int)),
    points90: list((int, int)),
    points180: list((int, int)),
    points270: list((int, int))
};

let make = (points) => {
    let rotate90 = ((x, y)) => (y, x * -1);
    let points90 = List.map(rotate90, points);
    let points180 = List.map(rotate90, points90);
    let points270 = List.map(rotate90, points180);
    {
        points,
        points90,
        points180,
        points270
    }
};