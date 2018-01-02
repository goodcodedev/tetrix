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
  ;

let cubeTiles = Tetronimo.make([
  (0, 0),
  (1, 0),
  (0, 1),
  (1, 1)
]);
let lineTiles = Tetronimo.make([
  (0, 0),
  (0, 1),
  (0, 2),
  (0, 3)
]);
let triangleTiles = Tetronimo.make([
  (0, 0),
  (-1, 1),
  (0, 1),
  (1, 1)
]);
let rightTurnTiles = Tetronimo.make([
  (0, 0),
  (1, 0),
  (-1, 1),
  (0, 1)
]);
let leftTurnTiles = Tetronimo.make([
  (0, 0),
  (1, 0),
  (1, 1),
  (2, 1)
]);
let leftLTiles = Tetronimo.make([
  (0, 0),
  (0, 1),
  (-1, 2),
  (0, 2)
]);
let rightLTiles = Tetronimo.make([
  (0, 0),
  (0, 1),
  (0, 2),
  (1, 2)
]);

let elTiles = (element, rotation) => {
  let tetronimo = switch element {
  | Cube => cubeTiles
  | Line => lineTiles
  | Triangle => triangleTiles
  | LeftTurn => leftTurnTiles
  | RightTurn => rightTurnTiles
  | LeftL => leftLTiles
  | RightL => rightLTiles
  };
  switch rotation {
  | 1 => tetronimo.points90
  | 2 => tetronimo.points180
  | 3 => tetronimo.points270
  | _ => tetronimo.points
  }
};

let tileCols = 20;
let tileRows = 31;
let tileWidth = 20;
let tileHeight = 20;
let tilePadding = 3;

let tileColors = [|
  Utils.color(~r=199, ~g=214, ~b=240, ~a=255), /* Standard unfilled color */
  Utils.color(~r=255, ~g=180, ~b=160, ~a=255),
  Utils.color(~r=180, ~g=255, ~b=160, ~a=255),
  Utils.color(~r=180, ~g=240, ~b=250, ~a=255),
  Utils.color(~r=240, ~g=220, ~b=200, ~a=255)
|];

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


type stateT = {
  action: inputAction,
  curEl: curEl,
  lastTick: float,
  curTime: float,
  tiles: array(array(int))
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
  {
    el: newElType,
    color: Random.int(Array.length(tileColors) - 1) + 1,
    rotation: 0,
    pos: {
      x: tileCols / 2,
      y: 0
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
  Env.size(~width=400, ~height=640, env);
  {
    action: None,
    curEl: newElement(),
    lastTick: 0.,
    curTime: 0.,
    tiles: Array.make_matrix(tileRows, tileCols, 0)
  }
};

let fillElTiles = (tiles, color, x, y, env) => {
    Draw.fill(color, env);
    List.iter(((tileX, tileY)) => {
      Draw.rect(
        ~pos=((x + tileX) * tileWidth, (y + tileY) * tileHeight),
        ~width=tileWidth - 1,
        ~height=tileHeight - 1,
        env
      );
    }, tiles);
};


let isCollision = (state) => {
  List.exists(((tileX, tileY)) => {
    state.curEl.pos.y + tileY >= tileRows - 1
    || state.curEl.pos.x + tileX < 0 || state.curEl.pos.x + tileX > tileCols - 1
    || state.tiles[state.curEl.pos.y + tileY + 1][state.curEl.pos.x + tileX] > 0
  }, elTiles(state.curEl.el, state.curEl.rotation))
};

let attemptMoveLeft = (state) => {
  let movedState = {
    ...state,
    curEl: {
      ...state.curEl,
      pos: {
        x: state.curEl.pos.x - 1,
        y: state.curEl.pos.y
      }
    }
  };
  (isCollision(movedState)) ? state : movedState
};
let attemptMoveRight = (state) => {
  let movedState = {
    ...state,
    curEl: {
      ...state.curEl,
      pos: {
        x: state.curEl.pos.x + 1,
        y: state.curEl.pos.y
      }
    }
  };
  (isCollision(movedState)) ? state : movedState
};

let elToTiles = (state) => {
  List.iter(((tileX, tileY)) => {
    state.tiles[state.curEl.pos.y + tileY][state.curEl.pos.x + tileX] = state.curEl.color;
  }, elTiles(state.curEl.el, state.curEl.rotation));
};

let listTo = (countDown) => {
  let rec addToList = (list, countDown) => {
    if (countDown <= 0) {
      list
    } else {
      addToList([countDown, ...list], countDown - 1)
    }
  };
  addToList([], countDown)
};

let draw = (state, env) => {
  let timeStep = Env.deltaTime(env);
  let screenHeight = Env.height(env);
  Draw.background(Utils.color(~r=190, ~g=199, ~b=230, ~a=245), env);
  Draw.clear(env);
  Array.iteri(
    (y, tileRow) => {
      Array.iteri(
        (x, tileVal) => {
          Draw.fill(tileColors[tileVal], env);
          Draw.rect(
            ~pos=(x * tileWidth, y * tileHeight),
            ~width=tileWidth - tilePadding,
            ~height=tileHeight - tilePadding,
            env
          );
        },
        tileRow
      )
    },
    state.tiles
  );
  let state = switch state.action {
  | MoveLeft  => {
    ...attemptMoveLeft(state),
    action: None
  }
  | MoveRight => {
    ...attemptMoveRight(state),
    action: None
  }
  | BlockLeft => {
    ...List.fold_left((state, _) => attemptMoveLeft(state), state, listTo(3)),
    action: None
  }
  | BlockRight => {
    ...List.fold_left((state, _) => attemptMoveRight(state), state, listTo(3)),
    action: None
  }
  | MoveBeginning => {
    ...List.fold_left((state, _) => attemptMoveLeft(state), state, listTo(state.curEl.pos.x)),
    action: None
  }
  | MoveEnd => {
    ...List.fold_left((state, _) => attemptMoveRight(state), state, listTo(tileCols - state.curEl.pos.x)),
    action: None
  }
  | MoveDown => {
    ...state,
    action: None,
    curEl: {
      ...state.curEl,
      pos: {
        x: state.curEl.pos.x,
        y: state.curEl.pos.y + 1
      }
    }
  }
  | CancelDown => state
  | DropDown => {
    /* Drop down until collision */
    let rec dropDown = (state) => {
      if (isCollision(state)) {
        state
      } else {
        dropDown({
          ...state,
          curEl: {
            ...state.curEl,
            pos: {
              x: state.curEl.pos.x,
              y: state.curEl.pos.y + 1
            }
          }
        })
      }
    };
    dropDown({
      ...state,
      action: None
    })
  }
  | RotateCW => {
    ...state,
    action: None,
    curEl: {
      ...state.curEl,
      rotation: (state.curEl.rotation + 1) mod 4
    }
  }
  | RotateCCW => {
    ...state,
    action: None,
    curEl: {
      ...state.curEl,
      rotation: (state.curEl.rotation == 0) ? 3 : (state.curEl.rotation - 1)
    }
  }
  | None => state
  };
  /* Draw element */
  fillElTiles(elTiles(state.curEl.el, state.curEl.rotation), tileColors[state.curEl.color], state.curEl.pos.x, state.curEl.pos.y, env);
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. tickDuration;
  let newEl = (isNewTick && isCollision(state));
  if (newEl) {
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
  };
  let updatePos = (state) => {
    if (isNewTick) {
      if (newEl) {
        {
          ...state,
          curEl: newElement()
        }
      } else {
        switch state.action {
        | CancelDown => state
        | _ => {
            ...state,
            action: None,
            curEl: {
              ...state.curEl,
              pos: {
                x: state.curEl.pos.x,
                y: state.curEl.pos.y + 1
              }
            }
          }
        }
      }
    } else {
      state
    }
  };
  {
    ...updatePos(state),
    curTime: curTime,
    lastTick: (isNewTick) ? curTime : state.lastTick
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
      action: RotateCW
    }
    | C => {
      ...state,
      action: RotateCCW
    }
    | Period => {
      ...state,
      action: DropDown
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
