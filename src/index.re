open Game;
let resize = (state : stateT) => {
  /* todo: throttle? */
  BoardProgram.onResize(state.boardProgram);
};

let (viewportX, viewportY) = Gpu.Canvas.getViewportSize();
/*Gpu.Canvas.run(240, 520, setup, draw, ~keyPressed, ());*/
Gpu.Canvas.run(viewportX, viewportY, setup, draw, ~keyPressed, ~resize, ());
