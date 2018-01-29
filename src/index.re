open Game;
open Gpu;
open Config;


type sceneState = {
  tiles: array(int),
  currElVertices: VertexBuffer.t,
  currElIndices: IndexBuffer.t,
  beamVertices: VertexBuffer.t,
  beamIndices: IndexBuffer.t,
  elPos: Gpu.uniform,
  elColor: Gpu.uniform,
  mutable gameState: Game.stateT
};

let resize = (state) => {
  /* todo: throttle? */
  /*BoardProgram.onResize(state.boardProgram);*/
  ()
};

let setup = (canvas) => {
  /* Element position and color uniforms */
  let elPos = UniformVec2f(ref(Data.Vec2.zeros()));
  let elColor = UniformVec3f(ref(Data.Vec3.zeros()));
  let currElVertices = VertexBuffer.make(
    [|-0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0.5, -0.5|],
    [|VertexAttrib.make("position", Vec2f)|],
    DynamicDraw
  );
  let currElIndices = IndexBuffer.make(IndexBuffer.makeQuadsData(1), DynamicDraw);
  let beamVertices = VertexBuffer.makeQuad(());
  let beamIndices = IndexBuffer.makeQuad();
  let tiles = Array.make(tileRows * tileCols, 0);
  let gameState = Game.setup(canvas, tiles);
  {
    tiles,
    currElVertices,
    currElIndices,
    beamVertices,
    beamIndices,
    elPos,
    elColor,
    gameState
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
  let beamNode = TileBeam.makeNode(beamTex, state.elColor, state.beamVertices, state.beamIndices);
  /* Shadow of tiles */
  let shadowTex = Texture.makeEmptyRgba(());
  let tileShadows = TileShadows.makeNode(tilesTex, shadowTex);
  /* Tiles draw */
  let tilesDraw = TilesDraw.makeNode(tilesTex, sdfTilesTex);
  /* Cur el node */
  let currEl = CurrEl.makeNode(
    sdfTilesTex,
    state.elColor,
    state.elPos,
    state.currElVertices,
    state.currElIndices
  );
  /* Grid node draws background to board */
  /* Just passing deps for now, maybe it
     would be nice to wire textures to nodes
     or something */
  GridProgram.makeNode(
    tilesTex,
    shadowTex,
    beamTex,
    state.elPos,
    state.elColor,
    [
      sdfTiles,
      beamNode,
      tileShadows
    ],
    [
      tilesDraw,
      currEl
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

let updateElTiles = (curEl, state) => {
    let tiles = Game.elTiles(curEl.el, curEl.rotation);
    let color = Game.tileColors2[curEl.color];
    let x = curEl.pos.x;
    let y = curEl.pos.y;
    Gpu.Uniform.setVec3f(state.elColor, color);
    /* Translate to -1.0 to 1.0 coords */
    let tileHeight = 2.0 /. float_of_int(tileRows);
    let tileWidth = 2.0 /. float_of_int(tileCols);
    /* Translation */
    let elPos = [|
      -1. +. float_of_int(x) *. tileWidth,
      1. +. float_of_int(y) *. -.tileHeight
    |];
    Gpu.Uniform.setVec2f(state.elPos, elPos);
    let currElTiles = Array.concat(List.map(((tileX, tileY)) => {
      /* 2x coord system with y 1.0 at top and -1.0 at bottom */
      let tileYScaled = float_of_int(tileY * -1) *. tileHeight;
      let tileXScaled = float_of_int(tileX) *. tileWidth;
      /* Bottom left, Top left, Top right, Bottom right */
      [|
        tileXScaled, tileYScaled -. tileHeight,
        tileXScaled, tileYScaled,
        tileXScaled +. tileWidth, tileYScaled,
        tileXScaled +. tileWidth, tileYScaled -. tileHeight
      |]
    }, tiles));
    VertexBuffer.setDataT(state.currElVertices, currElTiles);
    let indexData = IndexBuffer.makeQuadsData(Array.length(currElTiles) / 4 / 2);
    IndexBuffer.setDataT(state.currElIndices, indexData);
};

let updateBeams = (state) => {
  /* Create vertices for beams */
  let rowHeight = 2.0 /. float_of_int(tileRows);
  let colWidth = 2.0 /. float_of_int(tileCols);
  let (_, vertices) = Array.fold_left(((i, vertices), (beamFrom, beamTo)) => {
    if (beamFrom > beamNone) {
      let beamVertices = [|
        -1.0 +. float_of_int(i) *. colWidth,
        1.0 -. (float_of_int(beamTo) *. rowHeight +. rowHeight),
        -1.0 +. float_of_int(i) *. colWidth,
        1.0 -. float_of_int(beamFrom) *. rowHeight,
        -1.0 +. (float_of_int(i) *. colWidth +. colWidth),
        1.0 -. float_of_int(beamFrom) *. rowHeight,
        -1.0 +. (float_of_int(i) *. colWidth +. colWidth),
        1.0 -. (float_of_int(beamTo) *. rowHeight +. rowHeight),
      |];
      (i + 1, Array.append(vertices, beamVertices))
    } else {
      (i + 1, vertices)
    };
  }, (0, [||]), state.gameState.beams);
  VertexBuffer.setDataT(state.beamVertices, vertices);
  let indexData = IndexBuffer.makeQuadsData(Array.length(vertices) / 8);
  IndexBuffer.setDataT(state.beamIndices, indexData);
};

let draw = (state : sceneState, scene, canvas) => {
  state.gameState = Game.draw(state.gameState, canvas);
  let elChanged = (state.gameState.posChanged || state.gameState.rotateChanged);
  state.gameState = if (elChanged) {
    updateElTiles(state.gameState.curEl, state);
    updateBeams(state);
    {
      ...state.gameState,
      posChanged: false,
      rotateChanged: false
    }
  } else {
    state.gameState
  };
  let flags = (elChanged) ? [UpdateFlags.ElPosChanged] : [];
  let flags = if (state.gameState.updateTiles) {
    [UpdateFlags.TilesChanged, ...flags]
  } else {
    flags
  };
  Scene.update(scene, [UpdateFlags.Frame, ...flags]);
  state
};

let keyPressed = (state, canvas) => {
  state.gameState = Game.keyPressed(state.gameState, canvas);
  state
};


let (viewportX, viewportY) = Gpu.Canvas.getViewportSize();
Scene.run(viewportX, viewportY, setup, createScene, draw, ~keyPressed, ~resize, ());
