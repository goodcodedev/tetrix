/* user defined state and update flags */
type t('state, 'flags) = {
    state: 'state,
    root: item('state, 'flags),
    updateLists: Hashtbl.t(list('flags), list(item('state, 'flags)))
}
and item('state, 'flags) = {
    update: ('state, list('flags)) => unit,
    updateOn: list('flags),
    width: float,
    height: float,
    children: list(item('state, 'flags))
};

let makeItem = (update, updateOn, children, ~width=1.0, ~height=1.0, ()) => {
    {
        update,
        updateOn,
        width,
        height,
        children
    }
};
let make = (state, root) => {
    {
        state,
        root,
        updateLists: Hashtbl.create(5)
    }
};

let getSceneItemsToUpdate = (flags, root) => {
    let rec loop = (item, list) => {
        let hasAnyFlag = List.exists((updateOn) => List.exists((flag) => flag == updateOn, flags), item.updateOn);
        /* todo: tail recursive? */
        let childList = List.fold_left((list, child) => loop(child, list), list, item.children);
        if (hasAnyFlag) {
            [item, ...childList]
        } else {
            childList
        }
    };
    List.rev(loop(root, []))
};

let update = (self, updateFlags) => {
    let sortedFlags = List.sort((a, b) => (a < b) ? -1 : 1, updateFlags);
    if (!Hashtbl.mem(self.updateLists, sortedFlags)) {
        Hashtbl.add(self.updateLists, sortedFlags, getSceneItemsToUpdate(sortedFlags, self.root));
    };
    List.iter((item) => item.update(self.state, sortedFlags), Hashtbl.find(self.updateLists, sortedFlags));
};

module Gl = Reasongl.Gl;

let run = (width, height, setup, createScene, draw, ~keyPressed=?, ~resize=?, ()) => {
    let canvas = Gpu.Canvas.init(width, height);
    let userState = ref(setup(canvas));
    let scene = createScene(canvas, userState^);
    /* Start render loop */
    Gl.render(
        ~window = canvas.window,
        ~displayFunc = (f) => {
            canvas.deltaTime = f /. 1000.;
            userState := draw(userState^, scene, canvas);
        },
        ~windowResize = () => {
            switch (resize) {
            | Some(resize) => resize(userState^)
            | None => ()
            };
        },
        ~keyDown = (~keycode, ~repeat) => {
            canvas.keyboard.keyCode = keycode;
            if (!repeat) {
                switch (keyPressed) {
                | Some(keyPressed) => {
                    userState := keyPressed(userState^, canvas);
                }
                | None => ()
                }
            };
        },
        ~keyUp = (~keycode) => {
            /* Need this to trigger cleaning of keyes pressed
                and repeat marked */
            ()
        },
        ()
    );
};