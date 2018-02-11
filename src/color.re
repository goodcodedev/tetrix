type t = array(float);

type channel =
  | Red
  | Green
  | Blue;

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

/* http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/ */
module Hsl = {
    /* Hue in degrees from 0-360,
       saturation from 0.0 - 1.0
       luminance from 0.0 - 1.0 */
    type hsl = array(float);

    let clone = (hsl : hsl) : hsl => {
        Array.copy(hsl)
    };

    let incrH = (hsl : hsl, increment) => {
        hsl[0] = mod_float(hsl[0] +. increment, 360.0);
    };

    let setL = (hsl : hsl, l) => {
        hsl[2] = l;
    };

    let fromRgb = (c : t) : hsl => {
        let (r, g, b) = (c[0], c[1], c[2]);
        let (rgbMin, rgbMax, maxChannel) = {
            if (r > g) {
                if (r > b) {
                    if (g > b) {
                        (b, r, Red)
                    } else {
                        (g, r, Red)
                    }
                } else {
                    (g, b, Blue)
                }
            } else if (r > b) {
                (b, g, Green)
            } else if (g > b) {
                (r, g, Green)
            } else {
                (r, b, Blue)
            }
        };
        /* Luminance */
        let l = (rgbMin +. rgbMax) /. 2.0;
        /* Saturation */
        let s = if (rgbMin == rgbMax) {
            0.0
        } else if (l < 0.5) {
            (rgbMax -. rgbMin) /. (rgbMax +. rgbMin)
        } else {
            (rgbMax -. rgbMin) /. (2.0 -. rgbMax -. rgbMin)
        };
        let h = if (s == 0.0) {
            0.0
        } else {
            switch (maxChannel) {
            | Red => (g -. b) /. (rgbMax -. rgbMin) *. 60.0
            | Green => (2.0 +. (b -. r) /. (rgbMax -. rgbMin)) *. 60.0
            | Blue => (4.0 +. (r -. g) /. (rgbMax -. rgbMin)) *. 60.0
            }
        };
        let h = (h < 0.0) ? h +. 360.0 : h;
        [|h, s, l|]
    };

    let toRgb = (c : hsl) : t => {
        let (h, s, l) = (c[0], c[1], c[2]);
        if (s == 0.0) {
            [|l, l, l|]
        } else {
            let tmp1 = if (l < 0.5) {
                l *. (1.0 +. s)
            } else {
                l +. s -. l *. s
            };
            let tmp2 = 2.0 *. l -. tmp1;
            let hN = h /. 360.0;
            let inRange = (x) => if (x < 0.0) {
                x +. 1.0
            } else if (x > 1.0) {
                x -. 1.0
            } else {
                x
            };
            let tmpR = inRange(hN +. 0.333);
            let tmpG = inRange(hN);
            let tmpB = inRange(hN -. 0.333);
            let resolveChannel = (cnlTmp) => {
                let test1 = 6.0 *. cnlTmp;
                if (test1 < 1.0) {
                    tmp2 +. (tmp1 -. tmp2) *. test1
                } else if (2.0 *. cnlTmp < 1.0) {
                    tmp1
                } else if (3.0 *. cnlTmp < 2.0) {
                    tmp2 +. (tmp1 -. tmp2) *. (0.666 -. cnlTmp) *. 6.0
                } else {
                    tmp2
                }
            };
            let r = resolveChannel(tmpR);
            let g = resolveChannel(tmpG);
            let b = resolveChannel(tmpB);
            [|r, g, b|]
        }
    };
};