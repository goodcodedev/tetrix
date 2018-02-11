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
  holdingEl: elState,
  beamVO: Gpu.VertexObject.t,
  dropBeamVO: Gpu.VertexObject.t,
  dropColor: Gpu.uniform,
  sceneLight: Light.ProgramLight.t,
  mutable gameState: Game.stateT
};