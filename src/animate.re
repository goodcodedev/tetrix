let pi = 4.0 *. atan(1.0);
let halfPi = pi /. 2.0;

let getEasingFunction = (easing, from, last, duration) => {
    switch (easing) {
    | Scene.Linear =>
        ((anim : Scene.anim('a, 'b)) => from +. (last *. anim.elapsed /. duration))
    | Scene.SineOut =>
        ((anim : Scene.anim('a, 'b)) => from +. (last *. sin(anim.elapsed /. duration *. halfPi)))
    | Scene.SineInOut =>
        ((anim : Scene.anim('a, 'b)) => from +. (last *. (sin(anim.elapsed /. duration *. pi -. halfPi) *. 0.5 +. 0.5)))
    }
};

let nodeUniform = (
    node,
    uniform,
    ~key=?,
    ~from,
    ~last,
    ~duration=2.0,
    ~easing=Scene.Linear,
    ~next=?,
    ()
) => {
    let nextVal = getEasingFunction(easing, from, last, duration);
    Scene.makeAnim(
        node,
        (_scene, node, anim) => {
            let newVal = nextVal(anim);
            Scene.setUniformFloat(node, uniform, newVal);
        },
        duration,
        ~key=?key,
        ~next=?next,
        ()
    )
};