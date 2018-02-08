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
  let dropBeamVO = VertexObject.make(
    VertexBuffer.make(
      [||],
      [|
        VertexAttrib.make("position", GlType.Vec2f),
        VertexAttrib.make("fromDrop", GlType.Float)
      |],
      DynamicDraw
    ),
    Some(IndexBuffer.make([||], DynamicDraw))
  );
  let tiles = Array.make(tileRows * tileCols, 0);
  /* Texture with tiles data */
  let tilesTex = Texture.make(
    IntDataTexture(tiles, tileCols, tileRows),
    Texture.Luminance,
    Texture.NearestFilter
  );
  let elState = makeElState();
  let dropColor = Scene.UVec3f.zeros();
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
    dropBeamVO,
    dropColor,
    gameState
  }
};

let createBoardNode = (state) => {
  /* Todo: Greyscale textures, and implicit setup via pool by some param */
  /* Sdf tiles give 3d texture to tiles */
  let sdfTiles = SdfTiles.makeNode(tileCols, tileRows);
  /* Beam from current element downwards */
  let beamNode = TileBeam.makeNode(state.elState.color, state.beamVO);
  /* Drop down animation */
  let dropNode = DropBeams.makeNode(state.dropBeamVO);
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
  let gridNode = GridProgram.makeNode(
    state.tilesTex,
    tileShadows,
    beamNode,
    dropNode,
    state.dropColor,
    sdfTiles,
    state.elState
  );
  Layout.stacked(
    ~key="boardLayout",
    ~size=Scene.Aspect(
      float_of_int(Config.tileCols) /. float_of_int(Config.tileRows)
    ),
    [
      gridNode,
      tilesDraw,
      currEl
    ]
  )
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
  VertexObject.updateQuads(elState.vo, [||]);
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
  VertexObject.updateQuads(elState.vo, currElTiles);
};

let updateBeams = (beams, beamsVO, withFromDrop) => {
  /* Create vertices for beams */
  let rowHeight = 2.0 /. float_of_int(tileRows);
  let colWidth = 2.0 /. float_of_int(tileCols);
  let (_, vertices) = Array.fold_left(((i, vertices), (beamFrom, beamTo)) => {
    if (beamFrom > Game.beamNone) {
      let lx = -1.0 +. float_of_int(i) *. colWidth;
      let rx = -1.0 +. (float_of_int(i) *. colWidth +. colWidth);
      let by = 1.0 -. (float_of_int(beamTo) *. rowHeight +. rowHeight);
      let ty = 1.0 -. float_of_int(beamFrom) *. rowHeight;
      let beamVertices = if (withFromDrop) {
        [|
          lx, by, 1.0,
          lx, ty, 0.0,
          rx, ty, 0.0,
          rx, by, 1.0
        |]
      } else {
        [|
          lx, by,
          lx, ty,
          rx, ty,
          rx, by
        |]
      };
      (i + 1, Array.append(vertices, beamVertices))
    } else {
      (i + 1, vertices)
    };
  }, (0, [||]), beams);
  VertexObject.updateQuads(beamsVO, vertices);
};

let draw = (state, scene, canvas) => {
  state.gameState = Game.draw(state.gameState, canvas);
  let (hasDroppedDown, elMoved, elChanged) = {
    let gs = state.gameState;
    (
      gs.hasDroppedDown,
      gs.posChanged || gs.rotateChanged,
      gs.elChanged
    )
  };
  if (hasDroppedDown) {
    let dropNode = Scene.getNode(scene, "dropBeams");
    switch (dropNode) {
    | None => ()
    | Some(dropNode) =>
      /* Copy beam before updating it to use in animation */
      updateBeams(state.gameState.dropBeams, state.dropBeamVO, true);
      Gpu.Uniform.setVec3f(state.dropColor, Color.toVec3(state.gameState.dropColor));
      let dropAnim = Animate.uniform(
        dropNode,
        "sinceDrop",
        ~from=0.0,
        ~last=1.0,
        ~duration=0.15,
        ()
      );
      Scene.doAnim(scene, dropAnim);
    };
  };
  if (elChanged) {
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
  };
  if (elMoved) {
    updateElTiles(state.gameState.curEl, state.elState, tileRows, tileCols);
    updateBeams(state.gameState.beams, state.beamVO, false);
  };
  let flags = (elMoved) ? [UpdateFlags.ElPosChanged] : [];
  let flags = (elChanged) ? [UpdateFlags.ElChanged, ...flags] : flags;
  let flags = if (state.gameState.updateTiles) {
    switch (state.tilesTex.inited) {
    | Some(inited) => inited.update = true;
    | None => ()
    };
    [UpdateFlags.TilesChanged, ...flags]
  } else {
    flags
  };
  /* Reset state used by scene */
  state.gameState = {
    ...state.gameState,
    hasDroppedDown: false,
    elChanged: false,
    posChanged: false,
    rotateChanged: false,
    updateTiles: false
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
