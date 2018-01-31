type elState = {
  vo: Gpu.VertexObject.t,
  pos: Gpu.uniform,
  color: Gpu.uniform
};

type sceneState = {
  tiles: array(int),
  tilesTex: Gpu.Texture.t,
  elState: elState,
  nextEls: array(elState),
  beamVO: Gpu.VertexObject.t,
  mutable gameState: Game.stateT
};