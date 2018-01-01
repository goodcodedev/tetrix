open Reprocessing;

type element =
  | Cube
  | Line;

let cubeTiles = [
  (0, 0),
  (1, 0),
  (0, 1),
  (1, 1)
];
let lineTiles = [
  (0, 0),
  (0, 1),
  (0, 2)
];

let elTiles = (element) => {
  switch element {
  | Cube => cubeTiles
  | Line => lineTiles
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
  Utils.color(~r=180, ~g=240, ~b=250, ~a=255)
|];

type pos = {
  x: int,
  y: int
};

type stateT = {
  curEl: element,
  curElPos: pos,
  curElColor: int,
  lastTick: float,
  curTime: float,
  tiles: array(array(int))
};

let setup = (env) : stateT => {
  Random.self_init();
  Env.size(~width=400, ~height=640, env);
  {
    curEl: Cube,
    curElPos: {x: tileCols / 2, y: 0},
    curElColor: 1,
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
  fillElTiles(elTiles(state.curEl), tileColors[state.curElColor], state.curElPos.x, state.curElPos.y, env);
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. 0.3;
  let isCollision = (state) => {
    List.exists(((tileX, tileY)) => {
      state.curElPos.y + tileY >= tileRows - 1 || state.tiles[state.curElPos.y + tileY + 1][state.curElPos.x + tileX] > 0
    }, elTiles(state.curEl))
  };
  let newEl = if (isNewTick && isCollision(state)) {
    /* Put element into tiles and create set a new random element as current */
    List.iter(((tileX, tileY)) => {
      state.tiles[state.curElPos.y + tileY][state.curElPos.x + tileX] = state.curElColor;
    }, elTiles(state.curEl));
    true
  } else {
    false
  };
  let updatePos = (state) => {
    if (isNewTick) {
      if (newEl) {
        {
          x: tileCols / 2,
          y: 0
        }
      } else {
        {
          x: state.curElPos.x,
          y: state.curElPos.y + 1
        }
      }
    } else {
      state.curElPos
    }
  };

  {
    ...state,
    curElPos: updatePos(state),
    curTime: curTime,
    lastTick: (isNewTick) ? curTime : state.lastTick
  }
};

let keyPressed = (state, env) => {
  Events.(
    switch (Env.keyCode(env)) {
    | J => {
      ...state,
      curElPos: {
        x: state.curElPos.x - 1,
        y: state.curElPos.y
      }
    }
    | K => {
      ...state,
      curElPos: {
        x: state.curElPos.x + 1,
        y: state.curElPos.y
      }
    }
    | _ => state
    }
  );
};
run(~setup, ~draw, ~keyPressed, ());
