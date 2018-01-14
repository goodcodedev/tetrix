open Reprocessing;

module Document = {
  type window;
  let window: window = [%bs.raw "window"];
  [@bs.send] external addEventListener : ('window, string, 'eventT => unit) => unit = "addEventListener";
};

[@bs.set] external setLastKeyCode : ('a, int) => unit = "__lastKeyCode";
[@bs.get] external lastKeyCode : 'a => int = "__lastKeyCode";
[@bs.get] external getWhich : 'eventT => int = "which";

let tickDuration = 0.5;
let elColorOffset = 2;
let boardOffsetX = 50;
let boardOffsetY = 20;
let tileCols = 14;
let tileRows = 28;
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


let tileColors = [|
  Utils.color(~r=199, ~g=214, ~b=240, ~a=255), /* Standard unfilled color */
  Utils.color(~r=205, ~g=220, ~b=246, ~a=255), /* Standard lighter color */
  Utils.color(~r=130, ~g=240, ~b=250, ~a=255), /* Magenta line */
  Utils.color(~r=120, ~g=130, ~b=250, ~a=255), /* Blue left L */
  Utils.color(~r=250, ~g=210, ~b=80, ~a=255), /* Orange right L */
  Utils.color(~r=250, ~g=250, ~b=130, ~a=255), /* Yellow cube */
  Utils.color(~r=140, ~g=250, ~b=140, ~a=255), /* Green right shift */
  Utils.color(~r=180, ~g=100, ~b=230, ~a=255), /* Purple triangle */
  Utils.color(~r=240, ~g=130, ~b=120, ~a=255), /* Red left shift */
|];
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

type pos = {
  x: int,
  y: int
};

type curEl = {
  el: element,
  pos: pos,
  color: int,
  rotation: int
};

type gameState =
  | Running
  | GameOver;

type stateT = {
  action: inputAction,
  curEl: curEl,
  lastTick: float,
  curTime: float,
  tiles: array(array(int)),
  elTiles: array(int),
  gameState: gameState,
  paused: bool,
  headingFont: Reprocessing.fontT,
  infoFont: Reprocessing.fontT,
  boardProgram: BoardProgram.t
};


let newElement = () => {
  let newElType = switch (Random.int(7)) {
  | 0 => Cube
  | 1 => Line
  | 2 => Triangle
  | 3 => RightTurn
  | 4 => LeftTurn
  | 5 => LeftL
  | 6 => RightL
  | _ => Cube
  };
  let tetronimo = getTetronimo(newElType);
  {
    el: newElType,
    color: tetronimo.colorIndex,
    rotation: 0,
    pos: {
      x: tileCols / 2,
      y: 2
    }
  }
};

let setup = (env) : stateT => {
  Document.addEventListener(
    Document.window,
    "keydown",
    (e) => {
      setLastKeyCode(Document.window, getWhich(e))
    }
  );
  Random.self_init();
  Env.size(~width=800, ~height=640, env);
  let tiles = Array.make(tileRows * tileCols, 0);
  let bp = BoardProgram.createCanvas(tiles);
  /*Mandelbrot.createCanvas();*/
  let sdf = SdfTiles.createCanvas();
  SdfTiles.draw(sdf);
  {
    action: None,
    boardProgram: bp,
    curEl: newElement(),
    lastTick: 0.,
    curTime: 0.,
    tiles: Array.make_matrix(tileRows, tileCols, 0),
    elTiles: Array.make(tileCols, 0),
    gameState: Running,
    paused: false,
    headingFont: Draw.loadFont(~filename="./assets/font.fnt", ~isPixel=true, env),
    infoFont: Draw.loadFont(~filename="./assets/roboto.fnt", ~isPixel=true, env),
  }
};

let newGame = (state) => {
  for (y in 0 to tileRows - 1) {
    for (x in 0 to tileCols - 1) {
      state.tiles[y][x] = 0;
      state.boardProgram.tiles[tileCols*y + x] = 0;
    }
  };
  state.boardProgram.updateTiles = true;
  for (x in 0 to tileCols - 1) {
    state.elTiles[x] = 0;
  };
  {
    ...state,
    action: None,
    curEl: newElement(),
    lastTick: 0.,
    curTime: 0.,
    gameState: Running
  }
};



let isCollision = (state) => {
  List.exists(((tileX, tileY)) => {
    state.curEl.pos.y + tileY >= tileRows
    || state.curEl.pos.x + tileX < 0 || state.curEl.pos.x + tileX > tileCols - 1
    || state.tiles[state.curEl.pos.y + tileY][state.curEl.pos.x + tileX] > 0
  }, elTiles(state.curEl.el, state.curEl.rotation))
};

let attemptMove = (state, (x, y)) => {
  let moved = {
    ...state,
    curEl: {
      ...state.curEl,
      pos: {
        x: state.curEl.pos.x + x,
        y: state.curEl.pos.y + y
      }
    }
  };
  (isCollision(moved)) ? state : moved
};
let attemptMoveTest = (state, (x, y)) => {
  let moved = {
    ...state,
    curEl: {
      ...state.curEl,
      pos: {
        x: state.curEl.pos.x + x,
        y: state.curEl.pos.y + y
      }
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
        curEl: {
          ...state.curEl,
          rotation: newRotation,
          pos: {
            x: state.curEl.pos.x + x,
            y: state.curEl.pos.y - y,
          }
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
    let posy = state.curEl.pos.y + tileY;
    let posx = state.curEl.pos.x + tileX;
    state.tiles[state.curEl.pos.y + tileY][state.curEl.pos.x + tileX] = state.curEl.color;
    state.boardProgram.tiles[posy * tileCols + posx] = state.curEl.color - 1;
  }, elTiles(state.curEl.el, state.curEl.rotation));
  state.boardProgram.updateTiles = true;
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

let processAction = (state, env) => {
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
    ...List.fold_left((state, _) => attemptMove(state, (-1, 0)), state, listRange(state.curEl.pos.x)),
    action: None
  }
  | MoveEnd => {
    ...List.fold_left((state, _) => attemptMove(state, (1, 0)), state, listRange(tileCols - state.curEl.pos.x)),
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
  | Pause => {
    ...state,
    action: None,
    paused: !state.paused
  }
  | None => state
  }
};

let drawElTiles = (tiles, color, x, y, env) => {
    Draw.fill(color, env);
    List.iter(((tileX, tileY)) => {
      Draw.rect(
        ~pos=((x + tileX) * tileWidth + boardOffsetX, (y + tileY) * tileHeight + boardOffsetY),
        ~width=tileWidth - 1,
        ~height=tileHeight - 1,
        env
      );
    }, tiles);
};
let drawElTiles2 = (tiles, color, x, y, state) => {
    state.boardProgram.currElColor = color;
    state.boardProgram.currElTiles = Array.concat(List.map(((tileX, tileY)) => {
      /* Translate to -1.0 to 1.0 coords */
      let tileHeight = 2.0 /. float_of_int(tileRows);
      let tileWidth = 2.0 /. float_of_int(tileCols);
      let tileYScaled = float_of_int(tileY) /. float_of_int(tileRows);
      let tileXScaled = float_of_int(tileX) /. float_of_int(tileCols);
      /* Bottom left, Top left, Top right, Bottom right */
      [|
        tileXScaled, tileYScaled -. tileHeight,
        tileXScaled, tileYScaled,
        tileXScaled +. tileWidth, tileYScaled,
        tileXScaled +. tileWidth, tileYScaled -. tileHeight
      |] 
    }, tiles));
    state.boardProgram.updateCurrEl = true;
};

let drawGame = (state, env) => {
  let timeStep = Env.deltaTime(env);
  Draw.background(Utils.color(~r=190, ~g=199, ~b=230, ~a=245), env);
  Draw.clear(env);
  /* Reset element tile rows */
  Array.iteri((i, tileRow) => {
    if (tileRow > 0) {
      state.elTiles[i] = 0;
    }
  }, state.elTiles);
  /* Set row where element tile is */
  List.iter(((x, y)) => {
    let pointX = state.curEl.pos.x + x;
    let pointY = state.curEl.pos.y + y;
    if (state.elTiles[pointX] < pointY) {
      state.elTiles[pointX] = pointY;
    };
  }, elTiles(state.curEl.el, state.curEl.rotation));
  /* Draw tile squares */
  Array.iteri(
    (y, tileRow) => {
      Array.iteri(
        (x, tileVal) => {
          if (state.elTiles[x] > 0 && state.elTiles[x] < y && tileVal == 0) {
            /* Use light standard color */
            Draw.fill(tileColors[1], env);
          } else {
            Draw.fill(tileColors[tileVal], env);
          };
          Draw.rect(
            ~pos=(x * tileWidth + boardOffsetX, y * tileHeight + boardOffsetY),
            ~width=tileWidth - tilePadding,
            ~height=tileHeight - tilePadding,
            env
          );
        },
        tileRow
      );
    },
    state.tiles
  );
  /* Draw element */
  drawElTiles(
    elTiles(state.curEl.el, state.curEl.rotation),
    tileColors[state.curEl.color],
    state.curEl.pos.x, state.curEl.pos.y,
    env
  );
  drawElTiles2(
    elTiles(state.curEl.el, state.curEl.rotation),
    tileColors2[state.curEl.color],
    state.curEl.pos.x, state.curEl.pos.y,
    state
  );
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. tickDuration;
  let (state, newEl) = if (isNewTick) {
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
  let state = switch (newEl) {
  | false => state
  | true => {
    /* Put element into tiles */
    elToTiles(state);
    /* Check for completed rows */
    let completedRows = Array.map(
      tileRow => {
        !Array.fold_left((hasEmpty, tileState) => hasEmpty || tileState == 0, false, tileRow);
      },
      state.tiles
    );
    /* Move rows above completed down */
    let _ = Array.fold_right(
      (isCompleted, (movedRows, currentRow)) => {
        if (isCompleted) {
          for (y in currentRow downto 1) {
            state.tiles[y] = state.tiles[y - 1];
          };
          (movedRows + 1, currentRow - 1)
        } else {
          (movedRows, currentRow - 1)
        }
      },
      completedRows,
      (0, tileRows - 1)
    );
    {
      ...state,
      curEl: newElement()
    }
  }
  };
  if (newEl && isCollision(state)) {
    {
      ...state,
      gameState: GameOver
    }
  } else {
    {
      ...state,
      curTime: curTime,
      lastTick: (isNewTick) ? curTime : state.lastTick
    }
  }
};

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

let draw = (state, env) => {
  let state = processAction(state, env);
  if (state.paused) {
    state
  } else {
    let state = drawGame(state, env);
    BoardProgram.draw(state.boardProgram);
    drawInfo(state, env);
    switch (state.gameState) {
    | Running => state
    | GameOver => {
      newGame(state)
    }
    }
  }
};

let keyPressed = (state, env) => {
  Events.(
    switch (Env.keyCode(env)) {
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

run(~setup, ~draw, ~keyPressed, ());
