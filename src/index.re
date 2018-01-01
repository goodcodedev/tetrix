open Reprocessing;

type element =
  | Cube
  | Line
  | Triangle
  | RightTurn
  | LeftTurn
  | LeftL
  | RightL
  ;

let cubeTiles = [
  (0, 0),
  (1, 0),
  (0, 1),
  (1, 1)
];
let lineTiles = [
  (0, 0),
  (0, 1),
  (0, 2),
  (0, 3)
];

let triangleTiles = [
  (0, 0),
  (-1, 1),
  (0, 1),
  (1, 1)
];
let rightTurnTiles = [
  (0, 0),
  (1, 0),
  (-1, 1),
  (0, 1)
];
let leftTurnTiles = [
  (0, 0),
  (1, 0),
  (1, 1),
  (2, 1)
];
let leftLTiles = [
  (0, 0),
  (0, 1),
  (-1, 2),
  (0, 2)
];
let rightLTiles = [
  (0, 0),
  (0, 1),
  (0, 2),
  (1, 2)
];

let elTiles = (element) => {
  switch element {
  | Cube => cubeTiles
  | Line => lineTiles
  | Triangle => triangleTiles
  | LeftTurn => leftTurnTiles
  | RightTurn => rightTurnTiles
  | LeftL => leftLTiles
  | RightL => rightLTiles
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
};

type stateT = {
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
    pos: {
      x: tileCols / 2,
      y: 0
    }
  }
};

let setup = (env) : stateT => {
  Random.self_init();
  Env.size(~width=400, ~height=640, env);
  {
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
    state.curEl.pos.y + tileY >= tileRows - 1 || state.tiles[state.curEl.pos.y + tileY + 1][state.curEl.pos.x + tileX] > 0
  }, elTiles(state.curEl.el))
};

let elToTiles = (state) => {
  List.iter(((tileX, tileY)) => {
    state.tiles[state.curEl.pos.y + tileY][state.curEl.pos.x + tileX] = state.curEl.color;
  }, elTiles(state.curEl.el));
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
  /* Draw element */
  fillElTiles(elTiles(state.curEl.el), tileColors[state.curEl.color], state.curEl.pos.x, state.curEl.pos.y, env);
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. 0.2;
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
    let _ = Array.fold_left(
      ((movedRows, currentRow), isCompleted) => {
        if (isCompleted) {
          for (y in currentRow downto 0) {
            state.tiles[y] = state.tiles[y - 1];
          };
          (movedRows + 1, currentRow - 1)
        } else {
          (movedRows, currentRow - 1)
        }
      },
      (0, tileRows - 1),
      completedRows
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
        {
          ...state,
          curEl: {
            ...state.curEl,
            pos: {
              x: state.curEl.pos.x,
              y: state.curEl.pos.y + 1
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
    | J => {
      ...state,
      curEl: {
        ...state.curEl,
        pos: {
          x: state.curEl.pos.x - 1,
          y: state.curEl.pos.y
        }
      }
    }
    | K => {
      ...state,
      curEl: {
        ...state.curEl,
        pos: {
          x: state.curEl.pos.x + 1,
          y: state.curEl.pos.y
        }
      }
    }
    | S => {
      ...state,
      curEl: {
        ...state.curEl,
        pos: {
          x: state.curEl.pos.x,
          y: state.curEl.pos.y + 1
        }
      }
    }
    | _ => state
    }
  );
};
run(~setup, ~draw, ~keyPressed, ());
