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
      UiBox.makeNode(()),
      Layout.vertical(
        ~width=0.2,
        ~spacing=Scale(0.1),
        []
      )
    ])
};

let createScene = (canvas, state) => {

  Scene.make(
    canvas,
    state,
    UpdateFlags.Init,
    UpdateFlags.Frame,
    UpdateFlags.Resize,
    createRootNode()
  );
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
