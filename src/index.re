open Game;
open Gpu;
open Config;

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
    tiles: Array.make(tileRows * tileCols, 0)
  }
};

let createBoardNode = (state) => {
  /* Texture with tiles data */
  let tilesTex = Texture.make(IntDataTexture(state.tiles, tileCols, tileRows), Texture.Luminance, Texture.NearestFilter);
  /* Sdf tiles give 3d texture to tiles */
  let sdfTilesTex = Texture.makeEmptyRgba(());
  let sdfTiles = SdfTiles.makeNode(sdfTilesTex);
  /* Beam from current element downwards */
  let beamTex = Texture.makeEmptyRgba(());
  let beamNode = TileBeam.makeNode(beamTex);
  /* Shadow of tiles */
  let shadowTex = Texture.makeEmptyRgba(());
  let tileShadows = TileShadows.makeNode(tilesTex, shadowTex);
  /* Element position and color uniforms */
  let elPos = UniformVec2f(ref(Data.Vec2.zeros()));
  let elColor = UniformVec3f(ref(Data.Vec3.zeros()));
  /* Grid node draws background to board */
  /* Just passing deps for now, maybe it
     would be nice to wire textures to nodes
     or something */
  GridProgram.makeNode(
    tilesTex,
    shadowTex,
    beamTex,
    elPos,
    elColor,
    [
      sdfTiles,
      beamNode,
      tileShadows
    ]
  );
};

let createRootNode = (state) => {
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
          createBoardNode(state),
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
    createRootNode(state)
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
