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
  let vo = Scene.SVertexObject.makeQuad(~usage=DynamicDraw, ());
  {
    vo,
    pos,
    color
  }
};

let makeSceneLight = (camera) => {
  open Light;
  let dirLight = Directional.make(
    ~dir=StaticDir(Data.Vec3.make(0.4, 0.3, 0.3)),
    ()
  );
  let pointLight = PointLight.make(
    ~pos=StaticPos(Data.Vec3.make(0.0, -0.4, 2.0)),
    ~specular=12,
    ()
  );
  ProgramLight.make(
    dirLight,
    [pointLight],
    camera
  )
};

let setBg = (state) => {
  open Color;
  let bg = Hsl.fromRgb(fromFloats(0.14, 0.09, 0.20));
  let board = Hsl.clone(bg);
  Hsl.incrH(board, 20.0);
  let line = Hsl.clone(board);
  Hsl.setL(line, 0.2);
  Scene.UVec3f.setQuiet(state.bgColor, toVec3(Hsl.toRgb(bg)));
  Scene.UVec3f.setQuiet(state.boardColor, toVec3(Hsl.toRgb(board)));
  Scene.UVec3f.setQuiet(state.lineColor, toVec3(Hsl.toRgb(line)));
  state
};

let setup = (canvas) => {
  /* Element position and color uniforms */
  let beamVO = Scene.SVertexObject.makeQuad(~usage=DynamicDraw, ());
  let blinkVO = Scene.SVertexObject.makeQuad(~usage=DynamicDraw, ());
  let dropBeamVO = Scene.SVertexObject.make(
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
  let tilesTex = Scene.SceneTex.tex(Texture.make(
    IntDataTexture(tiles, tileCols, tileRows),
    Texture.Luminance,
    Texture.NearestFilter
  ));
  let elState = makeElState();
  let dropColor = Scene.UVec3f.zeros();
  let gameState = Game.setup(canvas, tiles);
  let camera = Camera.make(Data.Vec3.make(0.0, 0.4, 4.0));
  let sceneLight = makeSceneLight(camera);
  let elCenterRadius = Scene.UVec4f.zeros();
  let elLightPos = Scene.UVec3f.zeros();
  let elLight = Light.PointLight.make(
    ~pos=Light.DynamicPos(elLightPos),
    ~color=Light.DynamicColor(elState.color),
    ~specular=0,
    ()
  );
  let sceneAndElLight = {
    ...sceneLight,
    points: [elLight, ...sceneLight.points]
  };
  let bgColor = Scene.UVec3f.zeros();
  let boardColor = Scene.UVec3f.zeros();
  let lineColor = Scene.UVec3f.zeros();
  let state = {
    tiles,
    tilesTex,
    elState,
    elCenterRadius,
    nextEls: [|
      makeElState(),
      makeElState(),
      makeElState()
    |],
    holdingEl: makeElState(),
    beamVO,
    dropBeamVO,
    dropColor,
    blinkVO,
    sceneLight,
    elLightPos,
    sceneAndElLight,
    bgColor,
    boardColor,
    lineColor,
    gameState
  };
  setBg(state)
};

let createBoardNode = (state) => {
  /* Todo: Greyscale textures, and implicit setup via pool by some param */
  /* Sdf tiles give 3d texture to tiles */
  let sdfTiles = SdfTiles.makeNode(tileCols, tileRows, state.sceneLight);
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
    state.boardColor,
    state.lineColor,
    state.tilesTex,
    tileShadows,
    beamNode,
    dropNode,
    state.dropColor,
    sdfTiles,
    state.elState,
    state.elCenterRadius
  );
  let tileBlink = TileBlink.makeNode(state.blinkVO);
  Layout.stacked(
    ~key="boardLayout",
    ~size=Scene.Aspect(
      float_of_int(Config.tileCols) /. float_of_int(Config.tileRows)
    ),
    [
      gridNode,
      tilesDraw,
      currEl,
      tileBlink
    ]
  )
};

let createLeftRow = (state) => {
  open Geom3d;
  let bgShape = AreaBetweenQuads.make(
    Quad.make(
      Point.make(-1.0, -0.45, 0.0),
      Point.make(1.0, -0.7, 0.0),
      Point.make(1.0, 1.0, 0.0),
      Point.make(-1.0, 1.0, 0.0)
    ),
    Quad.make(
      Point.make(-0.7, -0.287, 0.5),
      Point.make(0.8, -0.34, 0.5),
      Point.make(0.8, 1.0, 0.5),
      Point.make(-0.7, 1.0, 0.5)
    )
  );
  Layout.stacked(
    ~size=Scene.Dimensions(Scale(0.22), Scale(1.0)),
    [
      Node3d.make(
        AreaBetweenQuads.makeVertexObject(bgShape, ()),
        ~size=Scene.(Dimensions(Scale(1.0), Scale(0.6))),
        ~light=state.sceneAndElLight,
        ()
      ),
      Layout.vertical(
        ~margin=Scene.(
          MarginRBLT(
            Scale(0.0),
            Scale(0.0),
            Scale(0.0),
            Scale(0.03)
          )
        ),
        ~spacing=Scene.Scale(0.04),
        [
          FontDraw.makeNode(
            "HOLD",
            "digitalt",
            ~height=0.27,
            ~align=SdfFont.TextLayout.AlignCenter,
            ()
          ),
          DrawElement.makeNode(state.holdingEl)
        ]
      )
    ]
  )
};

let createRightRow = (state) => {
  open Geom3d;
  let bgShape = AreaBetweenQuads.make(
    Quad.make(
      Point.make(-1.0, -0.95, 0.0),
      Point.make(1.0, -0.7, 0.0),
      Point.make(1.0, 1.0, 0.0),
      Point.make(-1.0, 1.0, 0.0)
    ),
    Quad.make(
      Point.make(-0.7, -0.54, 0.5),
      Point.make(0.8, -0.5, 0.5),
      Point.make(0.8, 1.0, 0.5),
      Point.make(-0.7, 1.0, 0.5)
    )
  );
  Layout.stacked(
    ~size=Scene.Dimensions(Scale(0.22), Scale(1.0)),
    [
      Node3d.make(
        AreaBetweenQuads.makeVertexObject(bgShape, ()),
        ~size=Scene.(Dimensions(Scale(1.0), Scale(0.6))),
        ~light=state.sceneAndElLight,
        ()
      ),
      Layout.vertical(
        ~margin=Scene.(
          MarginRBLT(
            Scale(0.0),
            Scale(0.0),
            Scale(0.0),
            Scale(0.03)
          )
        ),
        ~spacing=Scene.Scale(0.04),
        [
          FontDraw.makeNode(
            "NEXT",
            "digitalt",
            ~height=0.27,
            ~align=SdfFont.TextLayout.AlignCenter,
            ()
          ),
          DrawElement.makeNode(state.nextEls[0]),
          DrawElement.makeNode(state.nextEls[1]),
          DrawElement.makeNode(state.nextEls[2])
        ]
      )
    ]
  )
};

let createRootNode = (state) => {
  let mainSize = Scene.Aspect((14.0 +. 10.0) /. 26.0);
  Background.makeNode(
    state.bgColor,
    [
      Layout.stacked(
        ~vAlign=Scene.AlignMiddle,
        [
          Layout.horizontal(
            ~size=mainSize,
            ~hidden=false,
            [
              createLeftRow(state),
              createBoardNode(state),
              createRightRow(state)
            ]
          ),
          FontDraw.makeNode(
            "3210000123",
            "digitalt",
            ~key="countdown",
            ~height=0.27,
            ~align=SdfFont.TextLayout.AlignCenter,
            ~hidden=false,
            ()
          )
        ]
      )
    ]
  )
};

let createScene = (canvas, state) => {
  let scene = Scene.make(
    canvas,
    state,
    createRootNode(state),
    ~drawListDebug=false,
    ()
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

let resetElState = (scene, elState) => {
  Scene.SVertexObject.updateQuads(scene, elState.vo, [||]);
};

let updateElTiles = (scene, el : Game.elData, elState, rows, cols, uCenterRadius) => {
  /* todo: Consider caching some of this */
  let tiles = Game.elTiles(el.el, el.rotation);
  let color = Game.tileColors2[el.color];
  let x = el.posX;
  let y = el.posY;
  Scene.UVec3f.set(scene, elState.color, color);
  /* Translate to -1.0 to 1.0 coords */
  let tileHeight = 2.0 /. float_of_int(rows);
  let tileWidth = 2.0 /. float_of_int(cols);
  /* Translation */
  let elPos = [|
    -1. +. float_of_int(x) *. tileWidth,
    1. +. float_of_int(y) *. -.tileHeight
  |];
  Scene.UVec2f.set(scene, elState.pos, elPos);
  /* If we got center radius uniform (gridProgram currently uses this) */
  switch (uCenterRadius) {
  | None => ()
  | Some(uCenterRadius) =>
    let data = Hashtbl.find(Game.centerRadius, (el.el, el.rotation));
    let cx = elPos[0] +. data.centerX *. tileWidth;
    let cy = elPos[1] +. data.centerY *. tileHeight;
    let rx = data.radiusX *. tileWidth;
    let ry = data.radiusY *. tileHeight;
    Scene.UVec4f.set(scene, uCenterRadius, Data.Vec4.make(cx, cy, rx, ry));
  };
  let currElTiles = Array.concat(List.map(((tileX, tileY)) => {
    /* 2x coord system with y 1.0 at top and -1.0 at bottom */
    let tileYScaled = float_of_int(tileY * -1) *. tileHeight;
    let tileXScaled = float_of_int(tileX) *. tileWidth;
    /* Bottom left, Bottom right, Top right, Top left */
    [|
      tileXScaled, tileYScaled -. tileHeight,
      tileXScaled +. tileWidth, tileYScaled -. tileHeight,
      tileXScaled +. tileWidth, tileYScaled,
      tileXScaled, tileYScaled
    |]
  }, tiles));
  Scene.SVertexObject.updateQuads(scene, elState.vo, currElTiles);
};

let updateBeams = (scene, beams, beamsVO, withFromDrop) => {
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
          rx, by, 1.0,
          rx, ty, 0.0,
          lx, ty, 0.0
        |]
      } else {
        [|
          lx, by,
          rx, by,
          rx, ty,
          lx, ty
        |]
      };
      (i + 1, Array.append(vertices, beamVertices))
    } else {
      (i + 1, vertices)
    };
  }, (0, [||]), beams);
  Scene.SVertexObject.updateQuads(scene, beamsVO, vertices);
};

let blinkInit = (scene, state) => {
  let blinkRows = state.gameState.blinkRows;
  /* Update VO */
  let rowHeight = 2.0 /. float_of_int(Config.tileRows);
  let vertices = Array.fold_left((vertices, rowIdx) => {
      Array.append(vertices, [|
          -1.0, 1.0 -. rowHeight *. float_of_int(rowIdx + 1),
          1.0, 1.0 -. rowHeight *. float_of_int(rowIdx + 1),
          1.0, 1.0 -. rowHeight *. float_of_int(rowIdx),
          -1.0, 1.0 -. rowHeight *. float_of_int(rowIdx)
      |]);
  }, [||], blinkRows.rows);
  Scene.SVertexObject.updateQuads(scene, state.blinkVO, vertices);
  blinkRows.elapsed = 0.0;
  blinkRows.state = Blinking;
  /* Show tileBlink node */
  switch (Scene.getNode(scene, "tileBlink")) {
  | Some(tileBlink) => Scene.showNode(scene, tileBlink);
  | _ => ()
  };
};

let blinkEnd = (scene, state) => {
  let blinkRows = state.gameState.blinkRows;
  blinkRows.state = JustBlinked;
  /* Hide tileBlink node */
  switch (Scene.getNode(scene, "tileBlink")) {
  | Some(tileBlink) => Scene.hideNode(scene, tileBlink);
  | _ => ()
  };
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
      updateBeams(scene, state.gameState.dropBeams, state.dropBeamVO, true);
      Scene.UVec3f.set(scene, state.dropColor, Color.toVec3(state.gameState.dropColor));
      let dropAnim = Animate.uniform(
        dropNode,
        "sinceDrop",
        ~from=0.0,
        ~last=1.0,
        ~duration=1.2,
        ()
      );
      Scene.doAnim(scene, dropAnim);
    };
  };
  if (elChanged) {
    /* Redraw next elements */
    let _ = Queue.fold((i, nextEl) => {
      updateElTiles(scene, nextEl, state.nextEls[i], 3, 4, None);
      i + 1
    }, 0, state.gameState.elQueue);
    /* Handle holding element */
    switch (state.gameState.holdingEl) {
    | Some(holdingEl) => updateElTiles(scene, holdingEl, state.holdingEl, 3, 4, None);
    | None => resetElState(scene, state.holdingEl);
    };
  };
  if (elMoved) {
    updateElTiles(scene, state.gameState.curEl, state.elState, tileRows, tileCols, Some(state.elCenterRadius));
    /* We want elState.pos transformed by layout as light pos for element */
    let elPos = switch (Scene.UVec2f.get(state.elState.pos)) {
    | Some(elPos) => elPos
    | _ => Data.Vec2.zeros()
    };
    switch (Scene.getNode(scene, "grid")) {
    | Some(gridNode) => {
      switch (gridNode.layoutUniform) {
      | Some(Gpu.UniformMat3f(layout)) =>
        let pointPos = Data.Mat3.vecmul(layout^, Data.Vec3.fromVec2(elPos, 1.0));
        Data.Vec3.setZ(pointPos, 0.25);
        Scene.UVec3f.set(scene, state.elLightPos, pointPos);
      | _ => ()
      };
    }
    | None => ()
    };
    updateBeams(scene, state.gameState.beams, state.beamVO, false);
  };
  if (state.gameState.updateTiles) {
    switch (state.tilesTex.texture.inited) {
    | Some(inited) =>
      inited.update = true;
      /* Data is updated on the array in place */
      Scene.queueUpdates(scene, state.tilesTex.nodes);
    | None => ()
    };
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
  /* Blink rows */
  switch (state.gameState.blinkRows.state) {
  | BlinkInit =>
    blinkInit(scene, state);
  | Blinking =>
    let blinkRows = state.gameState.blinkRows;
    blinkRows.elapsed = blinkRows.elapsed +. scene.canvas.deltaTime;
    if (blinkRows.elapsed >= Config.blinkTime) {
      /* End blinking */
      blinkEnd(scene, state);
    }
  | _ => ()
  };
  Scene.update(scene, []);
  state
};

let keyPressed = (state, canvas) => {
  state.gameState = Game.keyPressed(state.gameState, canvas);
  state
};


let (viewportX, viewportY) = Gpu.Canvas.getViewportSize();
Scene.run(viewportX, viewportY, setup, createScene, draw, ~keyPressed, ~resize, ());
