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
  blinkVO: Gpu.VertexObject.t,
  sceneLight: Light.ProgramLight.t,
  elLightPos: Gpu.uniform,
  sceneAndElLight: Light.ProgramLight.t,
  bgColor: Gpu.uniform,
  boardColor: Gpu.uniform,
  lineColor: Gpu.uniform,
  mutable gameState: Game.stateT
};