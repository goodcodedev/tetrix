open Reprocessing;

type element =
  | Cube
  | Line;


let tileCols = 20;
let tileRows = 31;

let tileWidth = 20;
let tileHeight = 20;
let tilePadding = 3;

type pos = {
  x: int,
  y: int
};

type stateT = {
  curEl: element,
  curElPos: pos,
  lastTick: float,
  curTime: float
};

let setup = (env) : stateT => {
  Env.size(~width=400, ~height=640, env);
  {
    curEl: Cube,
    curElPos: {x: tileCols / 2, y: 0},
    lastTick: 0.,
    curTime: 0.
  }
};

let draw = (state, env) => {
  let timeStep = Env.deltaTime(env);
  let screenHeight = Env.height(env);
  Draw.background(Utils.color(~r=190, ~g=199, ~b=230, ~a=245), env);
  Draw.clear(env);
  for (y in 0 to tileRows) {
    for (x in 0 to tileCols) {
      Draw.fill(Utils.color(~r=199, ~g=214, ~b=240, ~a=255), env);
      Draw.rect(
        ~pos=(x * tileWidth, y * tileHeight),
        ~width=tileWidth - tilePadding,
        ~height=tileHeight - tilePadding,
        env
      );
    };
  };
  /* Draw element */
  switch state.curEl {
  | Cube => {
      Draw.fill(Utils.color(~r=160, ~g=214, ~b=255, ~a=255), env);
      let cubeTiles = [
        (0, 0),
        (1, 0),
        (0, 1),
        (1, 1)
      ];
      List.iter(((tileX, tileY)) => {
        Draw.rect(
          ~pos=((state.curElPos.x + tileX) * tileWidth, (state.curElPos.y + tileY) * tileHeight),
          ~width=tileWidth - 1,
          ~height=tileHeight - 1,
          env
        );
      }, cubeTiles);
  }
  | Line => ()
  };
  let curTime = state.curTime +. timeStep;
  let isNewTick = curTime > state.lastTick +. 2.5;
  let updatePos = (state) => {
    if (isNewTick) {
      {
        x: state.curElPos.x,
        y: state.curElPos.y + 1
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
