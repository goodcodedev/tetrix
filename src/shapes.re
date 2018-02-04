module Rect {
    type t = {
        x: float,
        y: float,
        w: float,
        h: float
    };

    let make = (x, y, w, h) => {x, y, w, h};

    let contains = (self, rect) => {
        (
            self.x <= rect.x
            && self.y <= rect.y
            && self.x +. self.w >= rect.x +. rect.w
            && self.y +. self.h >= rect.y +. rect.h
        )
    };

};