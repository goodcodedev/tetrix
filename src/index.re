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

let createScene = (canvas, state) => {

  Scene.make(
    state,
    Background.makeItem(canvas, ())
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
