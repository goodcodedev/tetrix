open Gpu;
open Config;
open SceneState;

let resize = (state) => {
  /* todo: throttle? */
  /*BoardProgram.onResize(state.boardProgram);*/
  ()
};

let makeElState = () => {
  let pos = Scene.UMat3f.id();
  let color = Scene.UVec3f.zeros();
  let vo = Scene.SVertexObject.makeQuad(~usage=DynamicDraw, ());
  {
    vo,
    pos,
    elPos: Data.Vec2.zeros(),
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
    IntDataTexture(Some(tiles), tileCols, tileRows),
    Texture.Luminance,
    Texture.NearestFilter
  ));
  let elState = makeElState();
  let dropColor = Scene.UVec3f.zeros();
  let gameState = Game.setup(tiles);
  let camera = Camera.make(Data.Vec3.make(0.0, 0.4, 4.0));
  let sceneLight = makeSceneLight(camera);
  let elCenterRadius = Scene.UVec4f.zeros();
  let elLightPos = Scene.UVec3f.zeros();
  let elLight = Light.PointLight.make(
    ~pos=Light.DynamicPos(elLightPos),
    ~color=Light.DynamicColor(elState.color),
    ~specular=0,
    ~factor=0.4,
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
    sceneLayout: StartScreen,
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
  let sdfTiles = SdfTiles.makeNode(
    float_of_int(tileCols),
    float_of_int(tileRows),
    state.sceneLight,
    ~key="sdfTiles",
    ~drawTo=Scene.TextureRGB,
    ()
  );
  /* Beam from current element downwards */
  let beamNode = TileBeam.makeNode(state.beamVO);
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
    ~vAlign=Scene.AlignTop,
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
        ~vAlign=Scene.AlignTop,
        [
          FontDraw.makeNode(
            "HOLD",
            "digitalt",
            ~opacity=0.7,
            ~height=0.27,
            ~align=SdfFont.TextLayout.AlignCenter,
            ()
          ),
          DrawElement.makeNode(state.holdingEl, state.sceneLight)
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
    ~vAlign=Scene.AlignTop,
    [
      Node3d.make(
        AreaBetweenQuads.makeVertexObject(bgShape, ()),
        ~size=Scene.(Dimensions(Scale(1.0), Scale(0.6))),
        ~light=state.sceneAndElLight,
        ()
      ),
      CacheResult.makeNode(
        ~transparent=true,
        ~partialDraw=true,
        ~size=Scene.(Dimensions(Scale(1.0), Scale(0.6))),
        Layout.vertical(
          ~key="nextElements",
          ~margin=Scene.(
            MarginRBLT(
              Scale(0.0),
              Scale(0.0),
              Scale(0.0),
              Scale(0.03)
            )
          ),
          ~spacing=Scene.Scale(0.04),
          ~vAlign=Scene.AlignTop,
          [
            FontDraw.makeNode(
              "NEXT",
              "digitalt",
              ~key="next",
              ~opacity=0.7,
              ~height=0.27,
              ~align=SdfFont.TextLayout.AlignCenter,
              ()
            ),
            DrawElement.makeNode(state.nextEls[0], state.sceneLight),
            DrawElement.makeNode(state.nextEls[1], state.sceneLight),
            DrawElement.makeNode(state.nextEls[2], state.sceneLight)
          ]
        ),
        ()
      )
    ]
  )
};

let createStartScreen = (state) => {
  Layout.vertical(
    ~key="startScreen",
    [
      FontDraw.makeNode(
        "Vimtris",
        "digitalt",
        ~key="heading",
        ~height=0.25,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      ),
      FontDraw.makeNode(
        "Press N to play",
        "digitalt",
        ~key="startText",
        ~height=0.08,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      )
    ]
  )
};

let createPauseScreen = (state) => {
  Layout.stacked(
    ~key="pauseScreen",
    ~hidden=true,
    [
      Layout.vertical(
        [
          FontDraw.makeNode(
            "Pause",
            "digitalt",
            ~key="paused",
            ~height=0.26,
            ~align=SdfFont.TextLayout.AlignCenter,
            ()
          ),
          FontDraw.makeNode(
            "Press Space to continue",
            "digitalt",
            ~height=0.06,
            ~align=SdfFont.TextLayout.AlignCenter,
            ()
          )
        ]
      )
    ]
  )
};

let createGameOverScreen = (state) => {
  Layout.vertical(
    [
      FontDraw.makeNode(
        "Game over",
        "digitalt",
        ~key="gameOver",
        ~height=0.25,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      ),
      FontDraw.makeNode(
        "Press N to start new game",
        "digitalt",
        ~height=0.08,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      )
    ]
  )
};

let createHelpScreen = (state) => {
  let helpLines = [
    "Space - pause",
    "H - move left",
    "L - move right",
    "J - move down",
    "K - cancel down",
    "C - rotate clockwise",
    "S - rotate counter clockwise",
    ". - drop",
    "W - move block right",
    "B - move block left",
    "0 - move leftmost",
    "$ - move rightmost"
  ];
  Layout.vertical(
    ~margin=Scene.MarginXY(Scene.Scale(0.12), Scene.Scale(0.0)),
    [
      FontDraw.makeNode(
        "Help",
        "digitalt",
        ~key="gameOver",
        ~height=0.12,
        ~align=SdfFont.TextLayout.AlignLeft,
        ()
      ),
      FontDraw.makeNode(
        String.concat("\n", helpLines),
        "digitalt",
        ~height=0.07,
        ~numLines=List.length(helpLines),
        ~align=SdfFont.TextLayout.AlignLeft,
        ()
      )
    ]
  )
};

let createNextLevelScreen = (state) => {
  Layout.vertical(
    [
      FontDraw.makeNode(
        "Level complete",
        "digitalt",
        ~key="gameOver",
        ~height=0.25,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      ),
      FontDraw.makeNode(
        "Press N to start new level",
        "digitalt",
        ~height=0.08,
        ~align=SdfFont.TextLayout.AlignCenter,
        ()
      )
    ]
  )
};

let createRootNode = (state) => {
  let mainSize = Scene.Aspect((14.0 +. 10.0) /. 26.0);
  Layout.stacked(
    ~size=Scene.Dimensions(Scale(1.0), Scale(1.0)),
    ~vAlign=Scene.AlignMiddle,
    [
      Background.makeNode(
        state.bgColor,
        [
          Layout.horizontal(
            ~key="gameHorizontal",
            ~size=mainSize,
            ~hidden=true,
            [
              createLeftRow(state),
              createBoardNode(state),
              createRightRow(state)
            ]
          )
        ]
      ),
      Mask.makeNode(),
      createStartScreen(state),
      createPauseScreen(state)
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
  let anim = Animate.anim(
    AnimNodeUniform(Scene.getNodeUnsafe(scene, "background"), "anim"),
    AnimFloat(0.0, 1.0),
    ~easing=Scene.SineOut,
    ~duration=1.5,
    ()
  );
  Scene.doAnim(scene, anim);
  scene
};

let resetElState = (scene, elState) => {
  Scene.SVertexObject.updateQuads(scene, elState.vo, [||]);
};

let updateElTiles = (scene, el : Game.elData, elState, rows, cols, uCenterRadius, isSideEl) => {
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
  let xPos = -1. +. float_of_int(x) *. tileWidth;
  let yPos = 1. +. float_of_int(y) *. -.tileHeight;
  Data.Vec2.set(elState.elPos, xPos, yPos);
  let elPos = if (isSideEl) {
    Data.Mat3.scaleTrans(
      0.75, 0.75,
      xPos *. 0.375, yPos *. 0.375
    )
  } else {
    Data.Mat3.trans(xPos, yPos)
  };
  Scene.UMat3f.set(scene, elState.pos, elPos);
  /* If we got center radius uniform (gridProgram currently uses this) */
  switch (uCenterRadius) {
  | None => ()
  | Some(uCenterRadius) =>
    let data = Hashtbl.find(Game.centerRadius, (el.el, el.rotation));
    let cx = xPos +. data.centerX *. tileWidth;
    let cy = yPos +. data.centerY *. tileHeight;
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

let blinkInit = (scene, state, blinkRows) => {
  /* Update VO */
  let rowHeight = 2.0 /. float_of_int(Config.tileRows);
  let vertices = Array.fold_left((vertices, rowIdx) => {
      Array.append(vertices, [|
          -1.0, 1.0 -. rowHeight *. float_of_int(rowIdx + 1),
          1.0, 1.0 -. rowHeight *. float_of_int(rowIdx + 1),
          1.0, 1.0 -. rowHeight *. float_of_int(rowIdx),
          -1.0, 1.0 -. rowHeight *. float_of_int(rowIdx)
      |]);
  }, [||], blinkRows);
  Scene.SVertexObject.updateQuads(scene, state.blinkVO, vertices);
  /* Show tileBlink node */
  switch (Scene.getNode(scene, "tileBlink")) {
  | Some(tileBlink) => Scene.showNode(scene, tileBlink);
  | _ => ()
  };
};

let blinkEnd = (scene) => {
  /* Hide tileBlink node */
  switch (Scene.getNode(scene, "tileBlink")) {
  | Some(tileBlink) => Scene.hideNode(scene, tileBlink);
  | _ => ()
  };
};

let drawGame = (state, scene) => {
  /* Handle element moved (changed position or rotated) */
  if (state.gameState.elMoved) {
    updateElTiles(scene, state.gameState.curEl, state.elState, tileRows, tileCols, Some(state.elCenterRadius), false);
    updateBeams(scene, state.gameState.beams, state.beamVO, false);
    /* We want elState.pos transformed by layout as light pos for element */
    let elPos = state.elState.elPos;
    let gridNode = Scene.getNodeUnsafe(scene, "grid");
    switch (gridNode.layoutUniform) {
    | Some(Gpu.UniformMat3f(layout)) =>
      open Data;
      let pointPos = Mat3.vecmul(layout^, Vec3.fromVec2(elPos, 1.0));
      Vec3.setZ(pointPos, 0.25);
      let lightAnim = "elLightAnim";
      Scene.clearAnim(scene, lightAnim);
      if (state.gameState.elChanged) {
        /* Set pos without animation */
        Scene.UVec3f.set(scene, state.elLightPos, pointPos);
      } else {
        let from = Scene.UVec3f.get(state.elLightPos);
        let (isDropDown, completedRows) = switch (state.gameState.touchDown) {
        | None => (false, false)
        | Some(touchDown) => (touchDown.isDropDown, touchDown.completedRows)
        };
        let anim = if (isDropDown) {
          /* Let anim go from reduced distance */
          Vec3.setY(from, Vec3.getY(pointPos) -. (Vec3.getY(pointPos) -. Vec3.getY(from)) /. 1.5);
          if (completedRows) {
            /* SineIn easing for extra boom effect */
            Animate.anim(
              Animate.AnimUniform(state.elLightPos),
              Animate.AnimVec3(from, pointPos),
              ~key=lightAnim,
              ~duration=Config.dropDownBeforeTouchDown,
              ~easing=Scene.SineIn,
              ~frameInterval=2,
              ()
            )
          } else {
            Animate.anim(
              Animate.AnimUniform(state.elLightPos),
              Animate.AnimVec3(from, pointPos),
              ~key=lightAnim,
              ~duration=Config.dropDownBeforeTick /. 2.0,
              ~frameInterval=2,
              ()
            )
          }
        } else {
          Animate.anim(
            Animate.AnimUniform(state.elLightPos),
            Animate.AnimVec3(from, pointPos),
            ~key=lightAnim,
            ~duration=Config.tickDuration /. 3.0,
            ~frameInterval=3,
            ()
          )
        };
        Scene.doAnim(scene, anim);
      }
    | _ => ()
    };
  };
  /* Handle update tiles */
  if (state.gameState.updateTiles) {
    switch (state.tilesTex.texture.inited) {
    | Some(inited) =>
      inited.update = true;
      /* Data is updated on the array in place */
      Scene.queueUpdates(scene, state.tilesTex.nodes);
    | None => ()
    };
  };
  switch (state.gameState.touchDown) {
  | None =>
    /* Handle new element */
    if (state.gameState.elChanged) {
      /* Redraw next elements */
      let _ = Queue.fold((i, nextEl) => {
        updateElTiles(scene, nextEl, state.nextEls[i], 3, 4, None, true);
        i + 1
      }, 0, state.gameState.elQueue);
      /* Handle holding element */
      switch (state.gameState.holdingEl) {
      | Some(holdingEl) => updateElTiles(scene, holdingEl, state.holdingEl, 3, 4, None, true);
      | None => resetElState(scene, state.holdingEl);
      };
    };
  | Some(touchDown) =>
    switch (touchDown.state) {
    | Game.TouchDown.Blinking =>
      let elapsed = touchDown.elapsed +. scene.canvas.deltaTime;
      let untilElapsed = (touchDown.isDropDown) ? Config.dropDownBeforeTouchDown +. Config.blinkTime : Config.blinkTime;
      if (elapsed >= untilElapsed) {
        /* End blinking */
        blinkEnd(scene);
        /* Signal done to game logic by setting Done state */
        state.gameState = {
          ...state.gameState,
          touchDown: Some({
            ...touchDown,
            state: Game.TouchDown.Done
          })
        };
      } else {
        /* Update elapsed time */
        state.gameState = {
          ...state.gameState,
          touchDown: Some({
            ...touchDown,
            elapsed
          })
        };
      };
    | Game.TouchDown.DropOnly =>
      let elapsed = touchDown.elapsed +. scene.canvas.deltaTime;
      if (touchDown.completedRows) {
        if (elapsed >= Config.dropDownBeforeTouchDown) {
          /* Start blink */
          blinkInit(scene, state, touchDown.rows);
          state.gameState = {
            ...state.gameState,
            touchDown: Some({
              ...touchDown,
              state: Game.TouchDown.Blinking,
              elapsed
            })
          };
        } else {
          /* Update elapsed time */
          state.gameState = {
            ...state.gameState,
            touchDown: Some({
              ...touchDown,
              elapsed
            })
          };
        };
      } else {
        if (elapsed >= Config.dropDownBeforeTick) {
          /* Signal done to start next tick */
          state.gameState = {
            ...state.gameState,
            touchDown: Some({
              ...touchDown,
              state: Game.TouchDown.Done
            })
          };
        } else {
          /* Update elapsed time */
          state.gameState = {
            ...state.gameState,
            touchDown: Some({
              ...touchDown,
              elapsed
            })
          };
        };
      };
    | Game.TouchDown.TouchDownInit =>
      if (touchDown.isDropDown) {
        let dropNode = Scene.getNodeUnsafe(scene, "dropBeams");
        /* Update dropbeams */
        updateBeams(scene, state.gameState.dropBeams, state.dropBeamVO, true);
        Scene.UVec3f.set(scene, state.dropColor, Color.toVec3(state.gameState.dropColor));
        let dropAnim = Animate.anim(
          AnimNodeUniform(dropNode, "sinceDrop"),
          AnimFloat(0.0, 1.0),
          ~easing=Scene.SineOut,
          ~duration=Config.dropAnimDuration,
          ()
        );
        Scene.doAnim(scene, dropAnim);
        /* When this is dropdown, don't blink immediately */
        state.gameState = {
          ...state.gameState,
          touchDown: Some({
            ...touchDown,
            state: Game.TouchDown.DropOnly
          })
        };
      } else {
        /* Touchdown init without dropdown, so there should be completed rows
           and blink right away */
        blinkInit(scene, state, touchDown.rows);
        state.gameState = {
          ...state.gameState,
          touchDown: Some({
            ...touchDown,
            state: Game.TouchDown.Blinking
          })
        };
      };
    | Game.TouchDown.Done => ()
    };
  };
  /* Reset state used by scene */
  state.gameState = {
    ...state.gameState,
    hasDroppedDown: false,
    elChanged: false,
    elMoved: false,
    updateTiles: false
  };
  state
};

let setScreenState = (state, screen) => {
  ...state,
  sceneLayout: screen
};

let showMask = (scene) => {
  let maskNode = Scene.getNodeUnsafe(scene, "mask");
  let animKey = "maskAnim";
  Scene.clearAnim(scene, animKey);
  let anim = Animate.anim(
    AnimNodeUniform(maskNode, "anim"),
    AnimFloat(0.0, 1.0),
    ~key=animKey,
    ~easing=Scene.SineOut,
    ~duration=3.0,
    ()
  );
  Scene.showNode(scene, maskNode);
  Scene.doAnim(scene, anim);
};

let hideMask = (scene) => {
  Scene.hideNodeByKey(scene, "mask");
};

let draw = (state, scene, canvas : Gpu.Canvas.t) => {
  state.gameState = Game.processAction({
    ...state.gameState,
    deltaTime: canvas.deltaTime
  });
  let state = switch (state.gameState.gameState) {
  | Running =>
    let state = switch (state.sceneLayout) {
    | GameScreen => state
    | StartScreen =>
      Scene.hideNodeByKey(scene, "startScreen");
      Scene.showNodeByKey(scene, "gameHorizontal");
      setScreenState(state, GameScreen)
    | PauseScreen =>
      Scene.hideNodeByKey(scene, "pauseScreen");
      hideMask(scene);
      setScreenState(state, GameScreen)
    };
    drawGame(state, scene)
  | StartScreen =>
    state
  | HelpScreen => state
  | Paused =>
    switch (state.sceneLayout) {
    | PauseScreen => state
    | GameScreen =>
      showMask(scene);
      Scene.showNodeByKey(scene, "pauseScreen");
      setScreenState(state, PauseScreen)
    | _ => failwith("Paused from other than game screen")
    }
  | NextLevel => state
  | GameOver => state
  };
  Scene.update(scene, []);
  state
};

let keyPressed = (state, canvas) => {
  {
    ...state,
    gameState: {
      ...state.gameState,
      action: Game.keyPressed(state.gameState, canvas)
    }
  }
};


let (viewportX, viewportY) = Gpu.Canvas.getViewportSize();
Scene.run(viewportX, viewportY, setup, createScene, draw, ~keyPressed, ~resize, ());
