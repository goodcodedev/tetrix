open Game;
open Gpu;
open Config;


type sceneState = {
  tiles: array(int),
  tilesTex: Texture.t,
  currElVO: VertexObject.t,
  beamVO: VertexObject.t,
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
  let elPos = Scene.UVec2f.zeros();
  let elColor = Scene.UVec3f.zeros();
  let currElVO = VertexObject.makeQuad(~usage=DynamicDraw, ());
  let beamVO = VertexObject.makeQuad(~usage=DynamicDraw, ());
  let tiles = Array.make(tileRows * tileCols, 0);
  /* Texture with tiles data */
  let tilesTex = Texture.make(
    IntDataTexture(tiles, tileCols, tileRows),
    Texture.Luminance,
    Texture.NearestFilter
  );
  let gameState = Game.setup(canvas, tiles);
  {
    tiles,
    tilesTex,
    currElVO,
    beamVO,
    elPos,
    elColor,
    gameState
  }
};

let createBoardNode = (state) => {
  /* Todo: Greyscale textures, and implicit setup via pool by some param */
  /* Sdf tiles give 3d texture to tiles */
  let sdfTiles = SdfTiles.makeNode(tileCols, tileRows);
  /* Beam from current element downwards */
  let beamNode = TileBeam.makeNode(state.elColor, state.beamVO);
  /* Shadow of tiles */
  let tileShadows = TileShadows.makeNode(state.tilesTex);
  /* Tiles draw */
  let tilesDraw = TilesDraw.makeNode(state.tilesTex, sdfTiles);
  /* Cur el node */
  let currEl = CurrEl.makeNode(
    state.elColor,
    state.elPos,
    state.currElVO,
    sdfTiles
  );
  /* Grid node draws background to board */
  /* Just passing deps for now, maybe it
     would be nice to wire textures to nodes
     or something */
  GridProgram.makeNode(
    state.tilesTex,
    tileShadows,
    beamNode,
    sdfTiles,
    state.elPos,
    state.elColor,
    [
      tilesDraw,
      currEl
    ]
  );
};

let createLeftRow = (state) => {
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
  )
};

let createRightRow = (state) => {
  let sdfTiles = SdfTiles.makeNode(4, 12);
  Layout.stacked(
    ~size=Scene.WidthRatio(Scale(0.22), 4. /. 12.),
    [
      sdfTiles,
      Layout.vertical(
        ~size=Scene.Dimensions(Scale(1.0), Scale(1.0)),
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
};

let createRootNode = (state) => {
    Background.makeNode([
      Layout.horizontal(
        ~size=Scene.Aspect((14.0 +. 10.0) /. 26.0),
        ~spacing=Scale(0.02),
        [
          createLeftRow(state),
          createBoardNode(state),
          createRightRow(state)
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
  VertexObject.updateQuads(state.currElVO, currElTiles, 8);
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
  VertexObject.updateQuads(state.beamVO, vertices, 8);
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
    Js.log("Tiles changed");
    switch (state.tilesTex.inited) {
    | Some(inited) => inited.update = true;
    | None => ()
    };
    state.gameState = {
      ...state.gameState,
      updateTiles: false
    };
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
