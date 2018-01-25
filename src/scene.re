type texture =
  | TextureItem(Gpu.Texture.t)
  | TextureRef(string);

type vertices =
  | VerticesItem(Gpu.VertexBuffer.t)
  | VerticesRef(string);

type indices =
  | IndicesItem(Gpu.IndexBuffer.t)
  | IndicesRef(string);

type uniform =
  | UniformItem(Gpu.Uniform.t)
  | UniformRef(string);

type uniformVal =
  | UniformValItem(Gpu.Uniform.uniformValue)
  | UniformValRef(string);

/* user defined state and update flags */
type t('state, 'flags) = {
    state: 'state,
    root: node('state, 'flags),
    updateLists: Hashtbl.t(list('flags), list(node('state, 'flags)))
}
and node('state, 'flags) = {
    update: ('state, list('flags)) => unit,
    updateOn: list('flags),
    width: float,
    height: float,
    deps: list(node('state, 'flags)),
    children: list(node('state, 'flags)),
    vertShader: Gpu.Shader.t,
    fragShader: Gpu.Shader.t,
    textures: Hashtbl.t(string, texture),
    vertices: Hashtbl.t(string, vertices),
    indices: Hashtbl.t(string, indices),
    uniforms: Hashtbl.t(string, uniform),
    uniformVals: Hashtbl.t(string, uniformVal),
    mutable parent: option(node('state, 'flags))
};

let makeItem = (
    update,
    updateOn,
    children,
    ~vertShader,
    ~fragShader,
    ~textures=[],
    ~vertices=[],
    ~indices=[],
    ~uniforms=[],
    ~uniformVals=[],
    ~width=1.0,
    ~height=1.0,
    ()
    ) => {
        let listToTbl = (list) => {
            let listLen = List.length(list);
            let tbl = Hashtbl.create((listLen > 0) ? listLen : 1);
            List.iter(((key, item)) => Hashtbl.add(tbl, key, item), list);
            tbl
        };
        let textures = listToTbl(textures);
        let vertices = listToTbl(vertices);
        let indices = listToTbl(indices);
        let uniforms = listToTbl(uniforms);
        let uniformVals = listToTbl(uniformVals);
    {
        update,
        updateOn,
        width,
        height,
        children,
        deps: [],
        vertShader,
        fragShader,
        textures,
        vertices,
        indices,
        uniforms,
        uniformVals,
        parent: None
    }
};

let make = (state, root) => {
    {
        state,
        root,
        updateLists: Hashtbl.create(5)
    }
};

let setNodeParents = (node) => {
    let rec loop = (node, parent) => {
        node.parent = parent;
        List.iter((dep) => loop(dep, Some(node)), node.deps);
        List.iter((child) => loop(child, Some(node)), node.children);
    };
    loop(node, None)
};

let getTblRef = (node, key, getTbl, resolve) => {
    let rec findInDeps = (deps, key) => {
        switch (deps) {
        | [] => None
        | [dep, ...rest] =>
            let tbl = getTbl(dep);
            if (Hashtbl.mem(tbl, key)) {
                resolve(dep, Hashtbl.find(tbl, key))
            } else {
                findInDeps(rest, key)
            }
        }
    };
    let rec findInParents = (parent, key) => {
        switch (parent) {
        | None => None
        | Some(parent) =>
            let tbl = getTbl(parent);
            if (Hashtbl.mem(tbl, key)) {
                resolve(parent, Hashtbl.find(tbl, key))
            } else {
                switch (findInDeps(parent.deps, key)) {
                | None => findInParents(parent.parent, key)
                | Some(tex) => Some(tex)
                }
            }
        }
    };
    findInParents(node, key)
};

let getTextureRef = (node, key) => {
    let getTbl = (node) => node.textures;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | TextureItem(item) => Some(item)
        | TextureRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(Some(node), key, getTbl, resolve)
};

let getVerticesRef = (node, key) => {
    let getTbl = (node) => node.vertices;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | VerticesItem(item) => Some(item)
        | VerticesRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(node, key, getTbl, resolve)
};

let getIndicesRef = (node, key) => {
    let getTbl = (node) => node.indices;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | IndicesItem(item) => Some(item)
        | IndicesRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(node, key, getTbl, resolve)
};

let getUniformRef = (node, key) => {
    let getTbl = (node) => node.uniforms;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | UniformItem(item) => Some(item)
        | UniformRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(node, key, getTbl, resolve)
};

let getUniformValRef = (node, key) => {
    let getTbl = (node) => node.uniformVals;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | UniformValItem(item) => Some(item)
        | UniformValRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(node, key, getTbl, resolve)
};

let getSceneNodesToUpdate = (flags, root) => {
    let rec loop = (node, list) => {
        let hasAnyFlag = List.exists((updateOn) => List.exists((flag) => flag == updateOn, flags), node.updateOn);
        /* todo: tail recursive? */
        let depsList = List.fold_left((list, dep) => loop(dep, list), list, node.deps);
        let childList = List.fold_left((list, child) => loop(child, list), depsList, node.children);
        if (hasAnyFlag) {
            [node, ...childList]
        } else {
            childList
        }
    };
    List.rev(loop(root, []))
};

let update = (self, updateFlags) => {
    let sortedFlags = List.sort((a, b) => (a < b) ? -1 : 1, updateFlags);
    if (!Hashtbl.mem(self.updateLists, sortedFlags)) {
        Hashtbl.add(self.updateLists, sortedFlags, getSceneNodesToUpdate(sortedFlags, self.root));
    };
    List.iter((node) => node.update(self.state, sortedFlags), Hashtbl.find(self.updateLists, sortedFlags));
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