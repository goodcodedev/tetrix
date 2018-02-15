module Document = {
  type window;
  let window: window = [%bs.raw "window"];
  [@bs.send] external addEventListener : ('window, string, 'eventT => unit) => unit = "addEventListener";
};

[@bs.set] external setLastKeyCode : ('a, int) => unit = "__lastKeyCode";
[@bs.get] external lastKeyCode : 'a => int = "__lastKeyCode";
[@bs.get] external getWhich : 'eventT => int = "which";

open Config;

let tickDuration = 0.5;
let elColorOffset = 2;
let boardOffsetX = 50;
let boardOffsetY = 20;
let tileWidth = 20;
let tileHeight = 20;
let tilePadding = 3;
let boardWidth = tileCols * tileWidth;

type element =
  | Cube
  | Line
  | Triangle
  | RightTurn
  | LeftTurn
  | LeftL
  | RightL
  ;

type gameState =
  | StartScreen
  | HelpScreen
  | Running
  | Paused
  | NextLevel
  | GameOver;


type startScreenAction =
  | StartGame
  | NoAction;

type gameAction =
  | MoveLeft
  | MoveRight
  | MoveDown
  | BlockLeft
  | BlockRight
  | BlockEnd
  | CancelDown
  | DropDown
  | RotateCW
  | RotateCCW
  | HoldElement
  | MoveBeginning
  | MoveEnd
  | Pause
  | NoAction
  ;

type nextLevelAction =
  | NextLevel
  | NoAction;

type gameOverAction =
  | NewGame
  | NoAction;

type pauseAction =
  | Resume
  | NoAction;

type helpScreenAction =
  | Resume
  | NoAction;

type inputAction =
  | GameAction(gameAction)
  | NextLevelAction(nextLevelAction)
  | GameOverAction(gameOverAction)
  | PauseAction(pauseAction)
  | StartScreenAction(startScreenAction)
  | HelpScreenAction(helpScreenAction);

let getTetronimo = (element) => {
  switch element {
  | Cube => Tetronimo.cubeTiles
  | Line => Tetronimo.lineTiles
  | Triangle => Tetronimo.triangleTiles
  | RightTurn => Tetronimo.rightTurnTiles
  | LeftTurn => Tetronimo.leftTurnTiles
  | LeftL => Tetronimo.leftLTiles
  | RightL => Tetronimo.rightLTiles
  }
};

let elTiles = (element, rotation) => {
  let tetronimo = getTetronimo(element);
  switch rotation {
  | 1 => tetronimo.points90
  | 2 => tetronimo.points180
  | 3 => tetronimo.points270
  | _ => tetronimo.points
  }
};

let tileColors2 = Array.map((color) => {
  Array.map((component) => {
    float_of_int(component) /. 255.0
  }, color)
},
[|
  [|199, 214, 240, 255|], /* Standard unfilled color */
  [|205, 220, 246, 255|], /* Standard lighter color */
  [|130, 240, 250, 255|], /* Magenta line */
  [|120, 130, 250, 255|], /* Blue left L */
  [|250, 210, 80, 255|], /* Orange right L */
  [|250, 250, 130, 255|], /* Yellow cube */
  [|140, 250, 140, 255|], /* Green right shift */
  [|180, 100, 230, 255|], /* Purple triangle */
  [|240, 130, 120, 255|], /* Red left shift */
|]);

let listRange = (countDown) => {
  let rec addToList = (list, countDown) => {
    if (countDown <= 0) {
      list
    } else {
      addToList([countDown, ...list], countDown - 1)
    }
  };
  addToList([], countDown)
};

/* Center position and radius
   for each tetronimo in each rotation
   Can add other aggregate element data */

type posRadius = {
  centerX: float,
  centerY: float,
  radiusX: float,
  radiusY: float,
  width: int,
  height: int,
  offsetX: int
};

let centerRadius = Hashtbl.create(7 * 4);
let addCenterRadius = (el, rot) => {
  let left = ref(10);
  let right = ref(-10);
  let top = ref(10);
  let bottom = ref(-10);
  List.iter(((x, y)) => {
    if (left^ > x) {
      left := x;
    };
    if (right^ < x + 1) {
      right := x + 1; };
    /* Y is flipped from definition in tetronimo.re and canvas,
      some accidentality. Maybe generally use gl coord system
      possibly clean up */
    if (top^ > y) {
      top := y;
    };
    if (bottom^ < y + 1) {
      bottom := y + 1;
    };
  }, elTiles(el, rot));
  let width = right^ - left^;
  let height = top^ - bottom^;
  let radiusX = float_of_int(width);
  let centerX = float_of_int(left^) +. radiusX /. 2.0;
  let radiusY = float_of_int(height) *. -1.0;
  let centerY = float_of_int(top^ * -1) -. radiusY /. 2.0;
  let offsetX = left^;
  Hashtbl.add(centerRadius, (el, rot), {
    centerX,
    centerY,
    radiusX,
    radiusY,
    width,
    height,
    offsetX
  });
};

List.iter((el) => {
  addCenterRadius(el, 0);
  addCenterRadius(el, 1);
  addCenterRadius(el, 2);
  addCenterRadius(el, 3);
}, [
  Cube,
  Line,
  Triangle,
  RightTurn,
  LeftTurn,
  LeftL,
  RightL
]);

type elData = {
  el: element,
  posX: int,
  posY: int,
  color: int,
  rotation: int
};

let beamNone = -2;

/* Todo: see if this can be refactored */
module BlinkRows {
    type blinkState =
      | NotBlinking
      | BlinkInit
      | Blinking
      | JustBlinked;

    type t = {
        mutable state: blinkState,
        mutable drawn: bool,
        mutable rows: array(int),
        mutable elapsed: float
    };

    let make = () => {
        {
            state: NotBlinking,
            drawn: false,
            rows: [||],
            elapsed: 0.0
        }
    };

    let endBlink = (self) => {
        self.state = JustBlinked;
        self.drawn = false;
        self.elapsed = 0.0;
    };
};


module ElQueue {
  type t = Queue.t(elData);

  let randomEl = () => {
    let elType = switch (Random.int(7)) {
    | 0 => Cube
    | 1 => Line
    | 2 => Triangle
    | 3 => RightTurn
    | 4 => LeftTurn
    | 5 => LeftL
    | 6 => RightL
    | _ => Cube
    };
    let tetronimo = getTetronimo(elType);
    /* Positions needs to work with display of next element */
    let (posX, posY) = switch (elType) {
    | Cube => (4, 1)
    | Line => (5, 2)
    | Triangle => (3, 3)
    | RightTurn => (3, 3)
    | LeftTurn => (3, 3)
    | LeftL => (3, 3)
    | RightL => (3, 3)
    };
    {
      el: elType,
      color: tetronimo.colorIndex,
      rotation: 0,
      posX,
      posY
    }
  };

  let setHoldPos = (elData) => {
    let (posX, posY) = switch (elData.el) {
    | Cube => (4, 1)
    | Line => (5, 2)
    | Triangle => (3, 3)
    | RightTurn => (3, 3)
    | LeftTurn => (3, 3)
    | LeftL => (3, 3)
    | RightL => (3, 3)
    };
    {
      ...elData,
      rotation: elData.rotation,
      posX,
      posY
    }
  };

  let initQueue = (queue) => {
    Queue.clear(queue);
    Queue.push(randomEl(), queue);
    Queue.push(randomEl(), queue);
    Queue.push(randomEl(), queue);
  };

  let make = () : t => { let q = Queue.create();
    initQueue(q);
    q
  };

  let setBoardInitPos = (elData) => {
    let middleX = tileCols / 2;
    let (posX, posY) = switch (elData.el) {
    | Cube => (middleX + 1, 3)
    | Line => (middleX + 1, 2)
    | Triangle => (middleX, 3)
    | RightTurn => (middleX, 4)
    | LeftTurn => (middleX, 4)
    | LeftL => (middleX, 3)
    | RightL => (middleX, 3)
    };
    {
      ...elData,
      posX,
      posY
    }
  };

  /* Use nextEl(state) to account for holding element */
  let pop = (queue) => {
    Queue.push(randomEl(), queue);
    Queue.pop(queue)
  };
};

type stateT = {
  action: inputAction,
  curEl: elData,
  holdingEl: option(elData),
  posChanged: bool,
  rotateChanged: bool,
  elChanged: bool,
  hasDroppedDown: bool,
  lastTick: float,
  curTime: float,
  tiles: array(array(int)),
  sceneTiles: array(int),
  updateTiles: bool,
  beams: array((int, int)),
  dropBeams: array((int, int)),
  dropColor: Color.t,
  gameState: gameState,
  paused: bool,
  lastCompletedRows: array(int),
  blinkRows: BlinkRows.t,
  elQueue: ElQueue.t,
  deltaTime: float
};

let nextEl = (state) => {
  let next = switch (state.holdingEl) {
  | Some(holdingEl) => holdingEl
  | None => ElQueue.pop(state.elQueue)
  };
  {
    ...state,
    holdingEl: None,
    elChanged: true,
    posChanged: true,
    rotateChanged: true,
    curEl: ElQueue.setBoardInitPos(next)
  }
};


let updateBeams = (state) => {
  /* Reset element tile rows */
  Array.iteri((i, (tileRow, _toRow)) => {
    if (tileRow > beamNone) {
      state.beams[i] = (beamNone, 0);
    }
  }, state.beams);
  /* Set row where element tile is */
  List.iter(((x, y)) => {
    let pointX = state.curEl.posX + x;
    let pointY = state.curEl.posY + y;
    let (beamFrom, beamTo) = state.beams[pointX];
    if (beamFrom == beamNone) {
      state.beams[pointX] = (pointY, beamTo);
    };
  }, elTiles(state.curEl.el, state.curEl.rotation));
  /* Set end of beam */
  /* This could almost be cached, but there are edge cases
     where tile is navigated below current beamTo.
     Could make update when moved below */
  Array.iteri((i, (beamFrom, _beamTo)) => {
    if (beamFrom > beamNone) {
      let beamTo = ref(0);
      for (j in beamFrom to tileRows - 1) {
        if (beamTo^ == 0) {
          if (state.tiles[j][i] > 0) {
            beamTo := j;
          };
        };
      };
      if (beamTo^ == 0) {
        beamTo := tileRows;
      };
      state.beams[i] = (beamFrom, beamTo^);
    };
  }, state.beams);
};


let setup = (tiles) : stateT => {
  Document.addEventListener(
    Document.window,
    "keydown",
    (e) => {
      setLastKeyCode(Document.window, getWhich(e))
    }
  );
  Random.self_init();
  let elQueue = ElQueue.make();
  /*Mandelbrot.createCanvas();*/
  /*let sdf = SdfTiles.createCanvas();
  SdfTiles.draw(sdf);*/
  let state = {
    action: StartScreenAction(NoAction),
    curEl: ElQueue.setBoardInitPos(ElQueue.pop(elQueue)),
    holdingEl: None,
    elChanged: true,
    posChanged: true,
    rotateChanged: true,
    hasDroppedDown: false,
    lastTick: 0.,
    curTime: 0.,
    tiles: Array.make_matrix(tileRows, tileCols, 0),
    sceneTiles: tiles,
    updateTiles: true,
    beams: Array.make(tileCols, (beamNone, 0)),
    dropBeams: Array.make(tileCols, (beamNone, 0)),
    dropColor: Color.white(),
    gameState: StartScreen,
    paused: false,
    lastCompletedRows: [||],
    blinkRows: BlinkRows.make(),
    elQueue,
    deltaTime: 0.0
  };
  state
};

let newGame = (state) => {
  for (y in 0 to tileRows - 1) {
    for (x in 0 to tileCols - 1) {
      state.tiles[y][x] = 0;
      state.sceneTiles[tileCols*y + x] = 0;
    }
  };
  ElQueue.initQueue(state.elQueue);
  {
    ...state,
    action: GameAction(NoAction),
    curEl: ElQueue.pop(state.elQueue),
    holdingEl: None,
    elChanged: true,
    posChanged: true,
    rotateChanged: true,
    updateTiles: true,
    lastTick: 0.,
    curTime: 0.,
    gameState: Running
  }
};

let isCollision = (state) => {
  List.exists(((tileX, tileY)) => {
    state.curEl.posY + tileY >= tileRows
    || state.curEl.posX + tileX < 0 || state.curEl.posX + tileX > tileCols - 1
    || state.tiles[state.curEl.posY + tileY][state.curEl.posX + tileX] > 0
  }, elTiles(state.curEl.el, state.curEl.rotation))
};

let attemptMove = (state, (x, y)) => {
  let moved = {
    ...state,
    posChanged: true,
    curEl: {
      ...state.curEl,
      posX: state.curEl.posX + x,
      posY: state.curEl.posY + y
    }
  };
  (isCollision(moved)) ? state : moved
};
let attemptMoveTest = (state, (x, y)) => {
  let moved = {
    ...state,
    posChanged: true,
    curEl: {
      ...state.curEl,
      posX: state.curEl.posX + x,
      posY: state.curEl.posY + y
    }
  };
  (isCollision(moved)) ? (false, state) : (true, moved)
};

/* Wall kicks http://tetris.wikia.com/wiki/SRS */
let wallTests = (state, newRotation, positions) => {
  let rec loop = (positions) => {
    switch (positions) {
    | [] => (false, state)
    | [(x, y), ...rest] => {
      let rotated = {
        ...state,
        rotateChanged: true,
        curEl: {
          ...state.curEl,
          rotation: newRotation,
          posX: state.curEl.posX + x,
          posY: state.curEl.posY - y
        }
      };
      if (isCollision(rotated)) {
        loop(rest)
      } else {
        (true, rotated)
      }
    }
    }
  };
  loop(positions)
};
let attemptRotateCW = (state) => {
  let newRotation = (state.curEl.rotation + 1) mod 4;
  /* First test for successful default rotation */
  let rotated = {
    ...state,
    rotateChanged: true,
    curEl: {
      ...state.curEl,
      rotation: newRotation
    }
  };
  if (!isCollision(rotated)) {
    (true, rotated)
  } else {
    /* Loop wall kick tests */
    let testPositions = switch (state.curEl.el) {
    | Line => switch (newRotation) {
    | 1 => [(-2, 0), (1, 0), (-2, -1), (1, 2)]
    | 2 => [(-1, 0), (2, 0), (-1, 2), (2, -1)]
    | 3 => [(2, 0), (-1, 0), (2, 1), (-1, -2)]
    | 0 => [(1, 0), (-2, 0), (1, -2), (-2, 1)]
    | _ => []
    }
    | _ => switch (newRotation) {
    | 1 => [(-1, 0), (-1, 1), (0, -2), (-1, -2)]
    | 2 => [(1, 0), (1, -1), (0, 2), (1, 2)]
    | 3 => [(1, 0), (1, 1), (0, -2), (1, -2)]
    | 0 => [(-1, 0), (-1, -1), (0, 2), (-1, 2)]
    | _ => []
    }
    };
    wallTests(state, newRotation, testPositions)
  }
};

let attemptRotateCCW = (state) => {
  let newRotation = (state.curEl.rotation == 0) ? 3 : (state.curEl.rotation - 1);
  /* First test for successful default rotation */
  let rotated = {
    ...state,
    rotateChanged: true,
    curEl: {
      ...state.curEl,
      rotation: newRotation
    }
  };
  if (!isCollision(rotated)) {
    (true, rotated)
  } else {
    /* Loop wall kick tests */
    let testPositions = switch (state.curEl.el) {
    | Line => switch (newRotation) {
    | 1 => [(1, 0), (-2, 0), (1, -2), (-2, 1)]
    | 2 => [(-2, 0), (1, 0), (-2, -1), (1, 2)]
    | 3 => [(-1, 0), (2, 0), (-1, 2), (2, -1)]
    | 0 => [(2, 0), (-1, 0), (2, 1), (-1, -2)]
    | _ => []
    }
    | _ => switch (newRotation) {
    | 1 => [(-1, 0), (-1, 1), (0, -2), (-1, -2)]
    | 2 => [(-1, 0), (-1, -1), (0, 2), (-1, 2)]
    | 3 => [(1, 0), (1, 1), (0, -2), (1, -2)]
    | 0 => [(1, 0), (1, -1), (0, 2), (1, 2)]
    | _ => []
    }
    };
    wallTests(state, newRotation, testPositions)
  }
};

let elToTiles = (state) => {
  List.iter(((tileX, tileY)) => {
    let posy = state.curEl.posY + tileY;
    let posx = state.curEl.posX + tileX;
    state.tiles[state.curEl.posY + tileY][state.curEl.posX + tileX] = state.curEl.color;
    state.sceneTiles[posy * tileCols + posx] = state.curEl.color - 1;
  }, elTiles(state.curEl.el, state.curEl.rotation));
};

let processGameInput = (state, gameAction) => {
  switch (gameAction) {
  | MoveLeft  => {
    ...attemptMove(state, (-1, 0)),
    action: GameAction(NoAction)
  }
  | MoveRight => {
    ...attemptMove(state, (1, 0)),
    action: GameAction(NoAction)
  }
  | BlockLeft => {
    let data = Hashtbl.find(centerRadius, (state.curEl.el, state.curEl.rotation));
    let leftPos = state.curEl.posX + data.offsetX;
    let toMove = leftPos - if (leftPos > 9) {
      9
    } else if (leftPos > 3) {
      3
    } else {
      0
    };
    {
      ...List.fold_left((state, _) => attemptMove(state, (-1, 0)), state, listRange(toMove)),
      action: GameAction(NoAction)
    }
  }
  | BlockRight => {
    let data = Hashtbl.find(centerRadius, (state.curEl.el, state.curEl.rotation));
    let leftPos = state.curEl.posX + data.offsetX;
    let toMove = (if (leftPos < 3) {
      3
    } else if (leftPos < 9) {
      9
    } else {
      14
    }) - leftPos;
    {
      ...List.fold_left((state, _) => attemptMove(state, (1, 0)), state, listRange(toMove)),
      action: GameAction(NoAction)
    }
  }
  | BlockEnd => {
    let data = Hashtbl.find(centerRadius, (state.curEl.el, state.curEl.rotation));
    let rightPos = state.curEl.posX + data.offsetX + data.width;
    let toMove = (if (rightPos < 3) {
      3
    } else if (rightPos < 9) {
      9
    } else {
      12
    }) - rightPos;
    {
      ...List.fold_left((state, _) => attemptMove(state, (1, 0)), state, listRange(toMove)),
      action: GameAction(NoAction)
    }
  }
  | MoveBeginning => {
    ...List.fold_left((state, _) => attemptMove(state, (-1, 0)), state, listRange(state.curEl.posX)),
    action: GameAction(NoAction)
  }
  | MoveEnd => {
    ...List.fold_left((state, _) => attemptMove(state, (1, 0)), state, listRange(tileCols - state.curEl.posX)),
    action: GameAction(NoAction)
  }
  | MoveDown => {
    ...attemptMove(state, (0, 1)),
    action: GameAction(NoAction)
  }
  | CancelDown => state
  | DropDown => {
    /* Drop down until collision */
    let rec dropDown = (state) => {
      switch (attemptMoveTest(state, (0, 1))) {
      | (false, state) => state
      | (true, state) => dropDown(state)
      }
    };
    let elColor = tileColors2[state.curEl.color];
    let dropColor = Color.fromFloats(elColor[0], elColor[1], elColor[2]);
    /* Set dropbeams,
       copy drop beams "from", "last" from position after drop */
    Array.iteri((i, (from, _last)) => {
      state.dropBeams[i] = (from, beamNone);
    }, state.beams);
    let state = dropDown({
      ...state,
      dropColor,
      hasDroppedDown: true,
      posChanged: true,
      action: GameAction(NoAction)
    });
    /* Set new "last" from current position */
    List.iter(((x, y)) => {
      let pointX = state.curEl.posX + x;
      let pointY = state.curEl.posY + y;
      let (beamFrom, beamTo) = state.dropBeams[pointX];
      if (beamTo < pointY) {
        state.dropBeams[pointX] = (beamFrom, pointY);
      };
    }, elTiles(state.curEl.el, state.curEl.rotation));
    state
  }
  | RotateCW => {
    switch (attemptRotateCW(state)) {
    | (true, state) => {
      ...state,
      action: GameAction(NoAction)
    }
    | (false, state) => {
      ...state,
      action: GameAction(NoAction)
    }
    }
  }
  | RotateCCW => {
    switch (attemptRotateCCW(state)) {
    | (true, state) => {
      ...state,
      action: GameAction(NoAction)
    }
    | (false, state) => {
      ...state,
      action: GameAction(NoAction)
    }
    }
  }
  | HoldElement => {
    let holdingEl = Some(ElQueue.setHoldPos(state.curEl));
    let state = nextEl(state);
    {
      ...state,
      action: GameAction(NoAction),
      holdingEl
    }
  }
  | Pause => {
    ...state,
    action: GameAction(NoAction),
    paused: !state.paused
  }
  | NoAction => state
  }
};

let afterTouchdown = (state) => {
  let curTime = state.curTime +. state.deltaTime;
  let state = if (Array.length(state.lastCompletedRows) > 0) {
    /* Move rows above completed down */
    Array.iter((currentRow) => {
      for (y in currentRow downto 1) {
        state.tiles[y] = Array.copy(state.tiles[y - 1]);
        for (tileIdx in (y * tileCols) to (y * tileCols + tileCols) - 1) {
          state.sceneTiles[tileIdx] = state.sceneTiles[tileIdx - tileCols];
        };
      };
    }, state.lastCompletedRows);
    {
      ...state,
      updateTiles: true
    }
  } else {
    state
  };
  let state = nextEl(state);
  updateBeams(state);
  if (isCollision(state)) {
    {
      ...state,
      action: GameOverAction(NoAction),
      gameState: GameOver
    }
  } else {
    {
      ...state,
      curTime: curTime,
      lastTick: curTime
    }
  }
};

let doGame = (state) => {
  let timeStep = state.deltaTime;
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. tickDuration;
  let (state, isTouchdown) = if (isNewTick) {
    switch (state.action) {
    | GameAction(CancelDown) => (state, false)
    | _ => {
      switch (attemptMoveTest(state, (0, 1))) {
      | (true, state) => (state, false)
      | (false, state) => (state, true)
      }
    }
    }
  } else {
    (state, false)
  };
  /* Handle element has touched down */
  switch (isTouchdown) {
  | false =>
    if (state.posChanged || state.rotateChanged) {
      updateBeams(state);
    };
    {
      ...state,
      curTime: curTime,
      lastTick: (isNewTick) ? curTime : state.lastTick
    }
  | true =>
    /* Put element into tiles */
    elToTiles(state);
    let state = {
      ...state,
      updateTiles: true
    };
    /* Check for completed rows */
    let completedRows = Array.map(
      tileRow => {
        !Array.fold_left((hasEmpty, tileState) => hasEmpty || tileState == 0, false, tileRow);
      },
      state.tiles
    );
    /* Get array with indexes of completed rows */
    let (_, completedRowIndexes) = Array.fold_left(((i, rows), completed) => {
      switch (completed) {
      | true => (i + 1, Array.append(rows, [|i|]))
      | false => (i + 1, rows)
      }
    }, (0, [||]), completedRows);
    let state = {
      ...state,
      lastCompletedRows: completedRowIndexes
    };
    if (Array.length(completedRowIndexes) > 0) {
      state.blinkRows.rows = completedRowIndexes;
      state.blinkRows.state = BlinkRows.BlinkInit;
      state
    } else {
      afterTouchdown(state)
    }
  }
};


/*
let drawInfo = (state, env) => {
  let infoOffsetX = boardOffsetX * 2 + boardWidth;
  let infoOffsetY = boardOffsetY;
  Draw.text(
    ~font=state.headingFont,
    ~body="Vimtris",
    ~pos=(infoOffsetX, infoOffsetY),
    env
  );
  List.iteri((i, text) => {
    Draw.text(
      ~font=state.infoFont,
      ~body=text,
      ~pos=(infoOffsetX + 4, infoOffsetY + 40 + (18 * i)),
      env
    );
  }, [
    "Space - pause",
    "H - move left",
    "L - move right",
    "J - move down",
    "K - cancel down",
    "W - move 3 tiles right",
    "B - move 3 tiles left",
    "0 - move leftmost",
    "$ - move rightmost",
    "S - rotate counter clockwise",
    "C - rotate clockwise",
    ". - drop",
  ]);
};
*/

let rec processGameAction = (state, action : gameAction) => {
  let mainProcess = (state) => {
    let state = processGameInput(state, action);
    doGame(state)
  };
  switch (state.blinkRows.state) {
  | BlinkRows.NotBlinking =>
    mainProcess(state);
  | BlinkRows.Blinking | BlinkRows.BlinkInit =>
    /* Blink animation */
    state
  | BlinkRows.JustBlinked =>
    state.blinkRows.state = BlinkRows.NotBlinking;
    /* Run after touchdown now that animation is done */
    let state = afterTouchdown(state);
    mainProcess(state);
  }
}
and processStartScreenAction = (state, action : startScreenAction) => {
    switch (action) {
    | NoAction => state
    | StartGame =>
      processAction({
        ...state,
        gameState: Running,
        action: GameAction(NoAction)
      })
    }
}
and processHelpScreenAction = (state, action : helpScreenAction) => {
    switch (action) {
    | NoAction => state
    | Resume =>
      processAction({
        ...state,
        gameState: Running,
        action: GameAction(NoAction)
      })
    }
}
and processPauseAction = (state, action : pauseAction) => {
  switch (action) {
  | NoAction => state
  | Resume =>
    processAction({
      ...state,
      gameState: Running,
      action: GameAction(NoAction)
    })
  }
}
and processNextLevelAction = (state, action : nextLevelAction) => {
  switch (action) {
  | NoAction => state
  | NextLevel =>
    processAction({
      ...state,
      gameState: Running,
      action: GameAction(NoAction)
    })
  }
}
and processGameOverAction = (state, action : gameOverAction) => {
  switch (action) {
  | NoAction => state
  | NewGame =>
    newGame(state)
  }
}
and processAction = (state) => {
  switch (state.gameState, state.action) {
  | (Running, GameAction(action)) => processGameAction(state, action)
  | (StartScreen, StartScreenAction(action)) => processStartScreenAction(state, action)
  | (HelpScreen, HelpScreenAction(action)) => processHelpScreenAction(state, action)
  | (Paused, PauseAction(action)) => processPauseAction(state, action)
  | (NextLevel, NextLevelAction(action)) => processNextLevelAction(state, action)
  | (GameOver, GameOverAction(action)) => processGameOverAction(state, action)
  | _ => failwith("Invalid gamestate and action")
  }
};

let keyPressed = (state, canvas : Gpu.Canvas.t) => {
  let keyCode = canvas.keyboard.keyCode;
  switch (state.gameState) {
  | StartScreen =>
    StartScreenAction(switch (keyCode) {
    | N => StartGame
    | _ => NoAction
    })
  | Running =>
    GameAction(switch (keyCode) {
    | H => MoveLeft
    | L => MoveRight
    | W => BlockRight
    | E => BlockEnd
    | B => BlockLeft
    | J => MoveDown
    | K => CancelDown
    | S | R => RotateCCW
    | C => RotateCW
    | D | X => HoldElement
    | Period => DropDown
    | Space => Pause
    | _ =>
      switch (lastKeyCode(Document.window)) {
      | 48 | 173 => MoveBeginning
      | 52 => MoveEnd
      | _ => NoAction
      }
    })
  | Paused =>
    PauseAction(switch (keyCode) {
    | Space => Resume
    | _ => NoAction
    })
  | HelpScreen =>
    HelpScreenAction(switch (keyCode) {
    | Space => Resume
    | _ => NoAction
    })
  | NextLevel =>
    NextLevelAction(switch (keyCode) {
    | N => NextLevel
    | _ => NoAction
    })
  | GameOver =>
    GameOverAction(switch (keyCode) {
    | N => NewGame
    | _ => NoAction
    })
  }
};
