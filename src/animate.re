type animTarget('state, 'flags) =
  | AnimUniform(Scene.sceneUniform)
  | AnimNodeUniform(Scene.node('state, 'flags), string);

/* Anim value, from to */
type animValue =
  | AnimFloat(float, float)
  | AnimVec2(Data.Vec2.t, Data.Vec2.t);

let pi = 4.0 *. atan(1.0);
let halfPi = pi /. 2.0;

/* Returns 0.0 to 1.0 based on duration and given elapsed */
let getEasingFunction = (easing, duration) => {
    switch (easing) {
    | Scene.Linear =>
        (elapsed) => (elapsed /. duration)
    | Scene.SineOut =>
        (elapsed) => (sin(elapsed /. duration *. halfPi))
    | Scene.SineInOut =>
        (elapsed) => (sin(elapsed /. duration *. pi -. halfPi) *. 0.5 +. 0.5)
    }
};

let anim = (
    animTarget,
    animValue,
    ~key=?,
    ~easing=Scene.SineOut,
    ~duration=2.0,
    ~next=?,
    ()
) => {
    let easingFunc = getEasingFunction(easing, duration);
    let animFunc = switch (animValue) {
    | AnimFloat(from, last) =>
        let remaining = last -. from;
        switch (animTarget) {
        | AnimUniform(u) =>
            (scene, anim : Scene.anim('state, 'flags)) => Scene.UFloat.set(scene, u, from +. (remaining *. easingFunc(anim.elapsed)))
        | AnimNodeUniform(n, u) =>
            (scene, anim : Scene.anim('state, 'flags)) => Scene.setUniformFloat(scene, n, u, from +. (remaining *. easingFunc(anim.elapsed)))
        }
    | AnimVec2(from, last) =>
        let fromX = Data.Vec2.getX(from);
        let fromY = Data.Vec2.getY(from);
        let remainingX = Data.Vec2.getX(last) -. fromX;
        let remainingY = Data.Vec2.getY(last) -. fromY;
        switch (animTarget) {
        | AnimUniform(u) =>
            (scene, anim : Scene.anim('state, 'flags)) => {
                let easing = easingFunc(anim.elapsed);
                Scene.UVec2f.set(scene, u, Data.Vec2.make(fromX +. (remainingX *. easing), fromY +. (remainingY *. easing)))
            }
        | AnimNodeUniform(n, u) =>
            (scene, anim : Scene.anim('state, 'flags)) => {
                let easing = easingFunc(anim.elapsed);
                Scene.setUniformVec2f(scene, n, u, Data.Vec2.make(fromX +. (remainingX *. easing), fromY +. (remainingY *. easing)))
            }
        }
    };
    Scene.makeAnim(
        animFunc,
        duration,
        ~key=?key,
        ~next=?next,
        ()
    )
};

/* Maybe nice with helper functions, this particular is a bit accidental */
let nodeUniform = (
    node,
    uniform,
    ~key=?,
    ~from,
    ~last,
    ~duration=2.0,
    ~easing=Scene.SineOut,
    ~next=?,
    ()
) => {
    anim(
        AnimNodeUniform(node, uniform),
        AnimFloat(from, last),
        ~key=?key,
        ~easing,
        ~duration,
        ~next=?next,
        ()
    )
};