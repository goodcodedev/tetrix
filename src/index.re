open Gpu;
open Config;
open SceneState;

let resize = (state) => {
  /* todo: throttle? */
  /*BoardProgram.onResize(state.boardProgram);*/
  ()
};

let makeElState = () => {
  let pos = Scene.UVec2f.zeros();
  let color = Scene.UVec3f.zeros();
  let vo = VertexObject.makeQuad(~usage=DynamicDraw, ());
  {
    vo,
    pos,
    color
  }
};

let setup = (canvas) => {
  /* Element position and color uniforms */
  let beamVO = VertexObject.makeQuad(~usage=DynamicDraw, ());
  let tiles = Array.make(tileRows * tileCols, 0);
  /* Texture with tiles data */
  let tilesTex = Texture.make(
    IntDataTexture(tiles, tileCols, tileRows),
    Texture.Luminance,
    Texture.NearestFilter
  );
  let elState = makeElState();
  let gameState = Game.setup(canvas, tiles);
  {
    tiles,
    tilesTex,
    elState,
    nextEls: [|
      makeElState(),
      makeElState(),
      makeElState()
    |],
    holdingEl: makeElState(),
    beamVO,
    gameState
  }
};

let createBoardNode = (state) => {
  /* Todo: Greyscale textures, and implicit setup via pool by some param */
  /* Sdf tiles give 3d texture to tiles */
  let sdfTiles = SdfTiles.makeNode(tileCols, tileRows);
  /* Beam from current element downwards */
  let beamNode = TileBeam.makeNode(state.elState.color, state.beamVO);
  /* Shadow of tiles */
  let tileShadows = TileShadows.makeNode(state.tilesTex);
  /* Tiles draw */
  let tilesDraw = TilesDraw.makeNode(state.tilesTex, sdfTiles);
  /* Cur el node */
  let currEl = CurrEl.makeNode(
    state.elState,
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
    state.elState,
    [
      tilesDraw,
      currEl
    ]
  );
};

let createLeftRow = (state) => {
  Layout.vertical(
    ~size=Scene.Dimensions(Scale(0.22), Scale(1.0)),
    [
      FontDraw.makeNode(
        "HOLD",
        "digitalt",
        ~height=0.3,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      ),
      DrawElement.makeNode(state.holdingEl)
    ]
  )
};

let createRightRow = (state) => {
  Layout.vertical(
    ~size=Scene.Dimensions(Scale(0.22), Scale(1.0)),
    [
      FontDraw.makeNode(
        "NEXT",
        "digitalt",
        ~height=0.3,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      ),
      DrawElement.makeNode(state.nextEls[0]),
      DrawElement.makeNode(state.nextEls[1]),
      DrawElement.makeNode(state.nextEls[2])
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

let resetElState = (elState) => {
  VertexObject.updateQuads(elState.vo, [||], 8);
};

let updateElTiles = (el : Game.elData, elState, rows, cols) => {
  /* todo: Consider caching some of this */
  let tiles = Game.elTiles(el.el, el.rotation);
  let color = Game.tileColors2[el.color];
  let x = el.posX;
  let y = el.posY;
  Gpu.Uniform.setVec3f(elState.color, color);
  /* Translate to -1.0 to 1.0 coords */
  let tileHeight = 2.0 /. float_of_int(rows);
  let tileWidth = 2.0 /. float_of_int(cols);
  /* Translation */
  let elPos = [|
    -1. +. float_of_int(x) *. tileWidth,
    1. +. float_of_int(y) *. -.tileHeight
  |];
  Gpu.Uniform.setVec2f(elState.pos, elPos);
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
  VertexObject.updateQuads(elState.vo, currElTiles, 8);
};

let updateBeams = (state) => {
  /* Create vertices for beams */
  let rowHeight = 2.0 /. float_of_int(tileRows);
  let colWidth = 2.0 /. float_of_int(tileCols);
  let (_, vertices) = Array.fold_left(((i, vertices), (beamFrom, beamTo)) => {
    if (beamFrom > Game.beamNone) {
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
  let elChanged = state.gameState.elChanged;
  state.gameState = if (elChanged) {
    /* Redraw next elements */
    let _ = Queue.fold((i, nextEl) => {
      updateElTiles(nextEl, state.nextEls[i], 3, 4);
      i + 1
    }, 0, state.gameState.elQueue);
    /* Handle holding element */
    switch (state.gameState.holdingEl) {
    | Some(holdingEl) => updateElTiles(holdingEl, state.holdingEl, 3, 4);
    | None => resetElState(state.holdingEl);
    };
    {
      ...state.gameState,
      elChanged: false
    }
  } else {
    state.gameState
  };
  let elMoved = (state.gameState.posChanged || state.gameState.rotateChanged);
  state.gameState = if (elMoved) {
    updateElTiles(state.gameState.curEl, state.elState, tileRows, tileCols);
    updateBeams(state);
    {
      ...state.gameState,
      posChanged: false,
      rotateChanged: false
    }
  } else {
    state.gameState
  };
  let flags = (elMoved) ? [UpdateFlags.ElPosChanged] : [];
  let flags = (elChanged) ? [UpdateFlags.ElChanged, ...flags] : flags;
  let flags = if (state.gameState.updateTiles) {
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
