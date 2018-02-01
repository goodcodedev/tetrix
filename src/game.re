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

type inputAction =
  | None
  | MoveLeft
  | MoveRight
  | MoveDown
  | BlockLeft
  | BlockRight
  | CancelDown
  | DropDown
  | RotateCW
  | RotateCCW
  | HoldElement
  | MoveBeginning
  | MoveEnd
  | Pause
  ;

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

type elData = {
  el: element,
  posX: int,
  posY: int,
  color: int,
  rotation: int
};

type gameState =
  | Running
  | GameOver;

let beamNone = -2;

/* Todo: see if this can be refactored */
module BlinkRows {
    type blinkState =
      | NotBlinking
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

  /* Use nextEl(state) to account for holding element */
  let pop = (queue) => {
    Queue.push(randomEl(), queue);
    let next = Queue.pop(queue);
    {
      ...next,
      posX: tileCols / 2,
      posY: 2
    }
  };
};

type stateT = {
  action: inputAction,
  curEl: elData,
  holdingEl: option(elData),
  posChanged: bool,
  rotateChanged: bool,
  elChanged: bool,
  lastTick: float,
  curTime: float,
  tiles: array(array(int)),
  sceneTiles: array(int),
  updateTiles: bool,
  beams: array((int, int)),
  gameState: gameState,
  paused: bool,
  lastCompletedRows: array(int),
  blinkRows: BlinkRows.t,
  elQueue: ElQueue.t
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
    curEl: {
      ...next,
      posX: tileCols / 2,
      posY: 2
    }
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
    if (beamFrom < pointY) {
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


let setup = (canvas, tiles) : stateT => {
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
    action: None,
    curEl: ElQueue.pop(elQueue),
    holdingEl: None,
    elChanged: true,
    posChanged: true,
    rotateChanged: true,
    lastTick: 0.,
    curTime: 0.,
    tiles: Array.make_matrix(tileRows, tileCols, 0),
    sceneTiles: tiles,
    updateTiles: true,
    beams: Array.make(tileCols, (beamNone, 0)),
    gameState: Running,
    paused: false,
    lastCompletedRows: [||],
    blinkRows: BlinkRows.make(),
    elQueue
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
    action: None,
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

let processAction = (state) => {
  switch state.action {
  | MoveLeft  => {
    ...attemptMove(state, (-1, 0)),
    action: None
  }
  | MoveRight => {
    ...attemptMove(state, (1, 0)),
    action: None
  }
  | BlockLeft => {
    ...List.fold_left((state, _) => attemptMove(state, (-1, 0)), state, listRange(3)),
    action: None
  }
  | BlockRight => {
    ...List.fold_left((state, _) => attemptMove(state, (1, 0)), state, listRange(3)),
    action: None
  }
  | MoveBeginning => {
    ...List.fold_left((state, _) => attemptMove(state, (-1, 0)), state, listRange(state.curEl.posX)),
    action: None
  }
  | MoveEnd => {
    ...List.fold_left((state, _) => attemptMove(state, (1, 0)), state, listRange(tileCols - state.curEl.posX)),
    action: None
  }
  | MoveDown => {
    ...attemptMove(state, (0, 1)),
    action: None
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
    dropDown({
      ...state,
      action: None
    })
  }
  | RotateCW => {
    switch (attemptRotateCW(state)) {
    | (true, state) => {
      ...state,
      action: None
    }
    | (false, state) => {
      ...state,
      action: None
    }
    }
  }
  | RotateCCW => {
    switch (attemptRotateCCW(state)) {
    | (true, state) => {
      ...state,
      action: None
    }
    | (false, state) => {
      ...state,
      action: None
    }
    }
  }
  | HoldElement => {
    let holdingEl = Some(ElQueue.setHoldPos(state.curEl));
    let state = nextEl(state);
    {
      ...state,
      action: None,
      holdingEl
    }
  }
  | Pause => {
    ...state,
    action: None,
    paused: !state.paused
  }
  | None => state
  }
};


let afterTouchdown = (state, canvas : Gpu.Canvas.t) => {
  let curTime = state.curTime +. canvas.deltaTime;
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
      gameState: GameOver
    }
  } else {
    {
      ...state,
      curTime: curTime,
      lastTick: curTime,
      posChanged: false,
      rotateChanged: false
    }
  }
};

let drawGame = (state, canvas : Gpu.Canvas.t) => {
  let timeStep = canvas.deltaTime;
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. tickDuration;
  let (state, isTouchdown) = if (isNewTick) {
    switch (state.action) {
    | CancelDown => (state, false)
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
      state.blinkRows.state = BlinkRows.Blinking;
      state
    } else {
      afterTouchdown(state, canvas)
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

let draw = (state, canvas) => {
  /* todo: Process by state, pause (etc)? */
  let state = processAction(state);
  let mainProcess = (state) => {
    let state = drawGame(state, canvas);
    switch (state.gameState) {
    | Running => state
    | GameOver => newGame(state)
    }
  };
  if (state.paused) {
    state
  } else {
    switch (state.blinkRows.state) {
    | BlinkRows.NotBlinking =>
      mainProcess(state);
    | BlinkRows.Blinking =>
      /* Blink animation */
      state
    | BlinkRows.JustBlinked =>
      state.blinkRows.state = BlinkRows.NotBlinking;
      /* Run after touchdown now that animation is done */
      let state = afterTouchdown(state, canvas);
      mainProcess(state);
    }
  }
};

let keyPressed = (state, canvas : Gpu.Canvas.t) => {
  Reasongl.Gl.Events.(
    switch (canvas.keyboard.keyCode) {
    | H => {
      ...state,
      action: MoveLeft
    }
    | L => {
      ...state,
      action: MoveRight
    }
    | W | E => {
      ...state,
      action: BlockRight
    }
    | B => {
      ...state,
      action: BlockLeft
    }
    | J => {
      ...state,
      action: MoveDown
    }
    | K => {
      ...state,
      action: CancelDown
    }
    | S | R => {
      ...state,
      action: RotateCCW
    }
    | C => {
      ...state,
      action: RotateCW
    }
    | D | X => {
      ...state,
      action: HoldElement
    }
    | Period => {
      ...state,
      action: DropDown
    }
    | Space => {
      ...state,
      action: Pause
    }
    | _ => {
      /* Js.log(lastKeyCode(Document.window)); */
      /* Todo: check shift press */
      switch (lastKeyCode(Document.window)) {
      | 48 | 173 => {
          ...state,
          action: MoveBeginning
        }
      | 52 => {
          ...state,
          action: MoveEnd
        }
      | _ => state
      }
    }
    }
  );
};
