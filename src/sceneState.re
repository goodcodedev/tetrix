type elState = {
  vo: Scene.sceneVertexObject,
  pos: Scene.sceneUniform,
  color: Scene.sceneUniform
};

type sceneState = {
  tiles: array(int),
  tilesTex: Scene.sceneTexture,
  elState: elState,
  elCenterRadius: Scene.sceneUniform,
  nextEls: array(elState),
  holdingEl: elState,
  beamVO: Scene.sceneVertexObject,
  dropBeamVO: Scene.sceneVertexObject,
  dropColor: Scene.sceneUniform,
  blinkVO: Scene.sceneVertexObject,
  sceneLight: Light.ProgramLight.t,
  elLightPos: Scene.sceneUniform,
  sceneAndElLight: Light.ProgramLight.t,
  bgColor: Scene.sceneUniform,
  boardColor: Scene.sceneUniform,
  lineColor: Scene.sceneUniform,
  mutable gameState: Game.stateT
};