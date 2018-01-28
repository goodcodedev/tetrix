let uniform = (
    node,
    uniform,
    ~from,
    ~last,
    ~duration=2.0,
    ~easing=Scene.Linear,
    ~next=?,
    ()
) => {
    Scene.makeAnim(
        node,
        (scene, node, anim) => {
            let newVal = from +. (last *. anim.elapsed /. duration);
            Scene.setUniformVal(node, uniform, Gpu.Uniform.UniformFloat(newVal));
        },
        duration,
        ~next=?next,
        ()
    )
};