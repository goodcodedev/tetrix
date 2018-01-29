open Game;
open Gpu;


type sceneState = {
  quadVertices: VertexBuffer.t,
  quadIndices: IndexBuffer.t,
  tiles: array(int)
};

let resize = (state) => {
  /* todo: throttle? */
  /*BoardProgram.onResize(state.boardProgram);*/
  ()
};

let setup = (canvas) => {
  {
    quadVertices: VertexBuffer.makeQuad(),
    quadIndices: IndexBuffer.makeQuad(),
    tiles: Array.make(10, 0)
  }
};

let createRootNode = () => {
    Background.makeNode([
      Layout.horizontal(
        ~size=Scene.Aspect((14.0 +. 10.0) /. 26.0),
        ~spacing=Scale(0.02),
        [
          Layout.vertical(
            ~size=Scene.Dimensions(Scale(0.22), Scale(1.0)),
            ~spacing=Scale(0.1),
            [
              UiBox.makeNode([
                FontDraw.makeNode(
                  "HOLD",
                  "digitalt",
                  ~height=0.35,
                  ~align=SdfFont.TextLayout.AlignCenter,
                  ()
                )
              ])
            ]
          ),
          UiBox.makeNode([]),
          Layout.vertical(
            ~size=Scene.Dimensions(Scale(0.22), Scale(1.0)),
            ~spacing=Scale(0.1),
            [
              UiBox.makeNode([
                FontDraw.makeNode(
                  "NEXT",
                  "digitalt",
                  ~height=0.35,
                  ~align=SdfFont.TextLayout.AlignCenter,
                  ()
                )
              ])
            ]
          )
        ]
      )
    ])
};

let createScene = (canvas, state) => {
  let scene = Scene.make(
    canvas,
    state,
    UpdateFlags.Init,
    UpdateFlags.Frame,
    UpdateFlags.Resize,
    createRootNode()
  );
  switch (Scene.getNode(scene, "background")) {
  | Some(bg) =>
    let anim = Animate.uniform(
      bg,
      "anim",
      ~from=0.0,
      ~last=1.0,
      ~duration=1.0,
      ()
    );
    Scene.doAnim(scene, anim);
  | None => ()
  };
  scene
};

let draw = (state, scene, canvas) => {
  Scene.update(scene, [UpdateFlags.Frame]);
  state
};

let keyPressed = (state, canvas) => {
  state
};


let (viewportX, viewportY) = Gpu.Canvas.getViewportSize();
Scene.run(viewportX, viewportY, setup, createScene, draw, ~keyPressed, ~resize, ());
