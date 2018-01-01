open Reprocessing;

type element =
  | Cube
  | Line;

type stateT = {
  curEl: element
};


let add x y => x + y;


let setup = (env) : stateT => {
  {
    curEl: Cube
  }
};

let draw = (state, env) => {
  let timeStep = Env.deltaTime(env);
  let screenHeight = Env.height(env);
  Draw.clear(env);
  state
};

let keyPressed = (state, env) =>
  Events.(
    switch (state.running, crashed(state, env), Env.keyCode(env)) {
    | (true, false, Space) => {...state, vy: jumpSpeed}
    | (false, true, Space) => resetState(state, env)
    | _ => state
    }
  );

run(~setup, ~draw, ~keyPressed, ());
