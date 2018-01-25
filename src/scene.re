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
    canvas: Gpu.Canvas.t,
    root: node('state, 'flags),
    frameFlag: 'flags,
    resizeFlag: 'flags,
    updateLists: Hashtbl.t(list('flags), list(node('state, 'flags)))
}
and node('state, 'flags) = {
    key: string,
    updateOn: list('flags),
    mutable drawState: option(Gpu.DrawState.t),
    update: option((node('state, 'flags), 'state, list('flags)) => unit),
    width: float,
    height: float,
    padding: option(float),
    aspect: option(float),
    transparent: bool,
    deps: list(node('state, 'flags)),
    children: list(node('state, 'flags)),
    vertShader: Gpu.Shader.t,
    fragShader: Gpu.Shader.t,
    textureList: list(string),
    textures: Hashtbl.t(string, texture),
    vertices: vertices,
    indices: indices,
    uniformList: list(string),
    uniforms: Hashtbl.t(string, uniform),
    uniformVals: Hashtbl.t(string, uniformVal),
    mutable parent: option(node('state, 'flags))
};

let quadVertices = Gpu.VertexBuffer.makeQuad(());
let quadIndices = Gpu.IndexBuffer.makeQuad();

let makeNode = (
    key,
    ~vertShader,
    ~fragShader,
    ~updateOn=[],
    ~update=?,
    ~children=[],
    ~textures=[],
    ~vertices=?,
    ~indices=?,
    ~uniforms=[],
    ~uniformVals=[],
    ~width=1.0,
    ~height=1.0,
    ~padding=?,
    ~aspect=?,
    ~transparent=false,
    ~deps=[],
    ()
    ) => {
        let listToTbl = (list) => {
            let listLen = List.length(list);
            let tbl = Hashtbl.create((listLen > 0) ? listLen : 1);
            List.iter(((key, item)) => Hashtbl.add(tbl, key, item), list);
            tbl
        };
        let textureList = List.map(((key, _texture)) => key, textures);
        let textures = listToTbl(textures);
        let uniformList = List.map(((key, _uniform)) => key, uniforms);
        let uniforms = listToTbl(uniforms);
        let uniformVals = listToTbl(uniformVals);
        let vertices = switch(vertices) {
        | Some(vertices) => vertices
        | None => VerticesItem(quadVertices)
        };
        let indices = switch(indices) {
        | Some(indices) => indices
        | None => IndicesItem(quadIndices)
        };
    {
        key,
        update,
        updateOn,
        drawState: None,
        width,
        height,
        padding,
        aspect,
        transparent,
        children,
        deps,
        vertShader,
        fragShader,
        textureList,
        textures,
        vertices,
        indices,
        uniformList,
        uniforms,
        uniformVals,
        parent: None
    }
};

let make = (canvas, state, frameFlag, resizeFlag, root) => {
    {
        state,
        canvas,
        root,
        frameFlag,
        resizeFlag,
        updateLists: Hashtbl.create(5)
    }
};

let setNodeParents = (root) => {
    let rec loop = (node, parent) => {
        node.parent = parent;
        List.iter((dep) => loop(dep, Some(node)), node.deps);
        List.iter((child) => loop(child, Some(node)), node.children);
    };
    loop(root, None)
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
                | Some(resource) => Some(resource)
                }
            }
        }
    };
    findInParents(node, key)
};

let getNodeRef = (node, key, resolve) => {
    let rec findInDeps = (deps, key) => {
        switch (deps) {
        | [] => None
        | [dep, ...rest] =>
            if (dep.key == key) {
                resolve(dep)
            } else {
                findInDeps(rest, key)
            }
        }
    };
    let rec findInParents = (parent, key) => {
        switch (parent) {
        | None => None
        | Some(parent) =>
            if (parent.key == key) {
                resolve(parent)
            } else {
                switch (findInDeps(parent.deps, key)) {
                | None => findInParents(parent.parent, key)
                | Some(resource) => Some(resource)
                }
            }
        }
    };
    findInParents(node, key)
};

let resolveTexture = (node, key) => {
    let getTbl = (node) => node.textures;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | TextureItem(item) => Some(item)
        | TextureRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(Some(node), key, getTbl, resolve)
};

let resolveVertices = (node, key) => {
    let rec resolve = (node) => {
        switch (node.vertices) {
        | VerticesItem(item) => Some(item)
        | VerticesRef(key) => getNodeRef(Some(node), key, resolve)
        }
    };
    getNodeRef(Some(node), key, resolve)
};

let resolveIndices = (node, key) => {
    let rec resolve = (node) => {
        switch (node.indices) {
        | IndicesItem(item) => Some(item)
        | IndicesRef(key) => getNodeRef(Some(node), key, resolve)
        }
    };
    getNodeRef(Some(node), key, resolve)
};

let resolveUniform = (node, key) => {
    let getTbl = (node) => node.uniforms;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | UniformItem(item) => Some(item)
        | UniformRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(Some(node), key, getTbl, resolve)
};

let resolveUniformVal = (node, key) => {
    let getTbl = (node) => node.uniformVals;
    let rec resolve = (node, resource) => {
        switch (resource) {
        | UniformValItem(item) => Some(item)
        | UniformValRef(key) => getTblRef(Some(node), key, getTbl, resolve)
        }
    };
    getTblRef(Some(node), key, getTbl, resolve)
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
    loop(root, [])
};

let createDrawStates = (self) => {
    let rec loop = (node) => {
        let uniforms = Array.of_list(List.map(
            (key) => {
                switch (Hashtbl.find(node.uniforms, key)) {
                | UniformItem(uniform) => uniform
                | UniformRef(key) =>
                    switch (resolveUniform(node, key)) {
                    | Some(uniform) => uniform
                    | None => failwith("Uniform not found: " ++ key)
                    }
                }
            },
            node.uniformList
        ));
        let uniformVals = Array.of_list(List.map(
            (key) => {
                if (Hashtbl.mem(node.uniformVals, key)) {
                    switch (Hashtbl.find(node.uniformVals, key)) {
                    | UniformValItem(uniformVal) => uniformVal
                    | UniformValRef(key) =>
                        switch (resolveUniformVal(node, key)) {
                        | Some(uniformVal) => uniformVal
                        | None => failwith("Uniform value not found: " ++ key);
                        }
                    }
                } else {
                    switch (resolveUniformVal(node, key)) {
                    | Some(uniformVal) => uniformVal
                    | None => failwith("Uniform value not found: " ++ key);
                    }
                }
            },
            node.uniformList
        ));
        let vertices = switch (node.vertices) {
        | VerticesItem(vertices) => vertices
        | VerticesRef(key) => switch (resolveVertices(node, key)) {
        | Some(vertices) => vertices
        | None => failwith("Vertices not found: " ++ key)
        }
        };
        let indices = switch (node.indices) {
        | IndicesItem(vertices) => vertices
        | IndicesRef(key) => switch (resolveIndices(node, key)) {
        | Some(indices) => indices
        | None => failwith("Indices not found: " ++ key)
        }
        };
        let textures = Array.of_list(List.map(
            (key) => {
                switch (Hashtbl.find(node.textures, key)) {
                | TextureItem(item) => Gpu.ProgramTexture.make(key, item)
                | TextureRef(refKey) => switch (resolveTexture(node, refKey)) {
                | Some(item) => Gpu.ProgramTexture.make(key, item)
                | None => failwith("Texture not found: " ++ refKey ++ ", on: " ++ key)
                }
                }
            },
            node.textureList
        ));
        node.drawState = Some(Gpu.DrawState.init(
            self.canvas.context,
            Gpu.Program.make(
                node.vertShader,
                node.fragShader,
                uniforms
            ),
            uniformVals,
            vertices,
            indices,
            textures
        ));
        List.iter((dep) => loop(dep), node.deps);
        List.iter((child) => loop(child), node.children);
    };
    loop(self.root)
};

let calcLayout = (self) => {
    let vpWidth = float_of_int(self.canvas.width);
    let vpHeight = float_of_int(self.canvas.height);
    let rec calcNodeLayout = (node, padding, width, height) => {
        let (padWidth, padHeight) = switch (padding) {
        | None => (width, height)
        | Some(padding) =>
            (width -. (width *. padding), height -. (height *. padding))
        };
        let (nodeWidth, nodeHeight) = switch (node.aspect) {
        | None => (padWidth, padHeight)
        | Some(aspect) =>
            let parentAspect = padWidth /. padHeight;
            if (aspect < parentAspect) {
                /* Limit by height */
                let width = padHeight *. aspect;
                (width, padHeight)
            } else {
                /* Limit by width */
                let height = padWidth /. aspect;
                (padWidth, height)
            }
        };
        let scale = Coords.Mat3.scale(nodeWidth /. vpWidth, nodeHeight /. vpHeight);
        if (Hashtbl.mem(node.uniforms, "layout")) {
            Hashtbl.replace(node.uniformVals, "layout", UniformValItem(Gpu.Uniform.UniformMat3f(scale)));
        };
        List.iter((dep) => calcNodeLayout(dep, padding, width, height), node.deps);
        List.iter((child) => calcNodeLayout(child, node.padding, nodeWidth, nodeHeight), node.children);
    };
    calcNodeLayout(self.root, None, vpWidth, vpHeight);
};

let draw = (self, node) => {
    switch (node.drawState) {
    | Some(drawState) =>
        if (node.transparent) {
            let context = self.canvas.context;
            Gpu.Gl.enable(~context, Gpu.Constants.blend);
            Gpu.Gl.blendFunc(~context, Gpu.Constants.src_alpha, Gpu.Constants.one_minus_src_alpha);
            Gpu.DrawState.draw(drawState, self.canvas);
            Gpu.Gl.disable(~context, Gpu.Constants.blend);
        } else {
            Gpu.DrawState.draw(drawState, self.canvas);
        };
    | None => failwith("Drawstate not found")
    };
};

let update = (self, updateFlags) => {
    let sortedFlags = List.sort((a, b) => (a < b) ? -1 : 1, updateFlags);
    if (!Hashtbl.mem(self.updateLists, sortedFlags)) {
        Hashtbl.add(self.updateLists, sortedFlags, getSceneNodesToUpdate(sortedFlags, self.root));
    };
    /* todo: possibly optimize with a second transformed data structure
       so the drawstate etc is readily available */
    List.iter((node) => {
        switch (node.update) {
        | Some(update) => update(node, self.state, sortedFlags)
        | None => draw(self, node)
        };
    }, Hashtbl.find(self.updateLists, sortedFlags));
};

module Gl = Reasongl.Gl;

let run = (width, height, setup, createScene, draw, ~keyPressed=?, ~resize=?, ()) => {
    let canvas = Gpu.Canvas.init(width, height);
    let userState = ref(setup(canvas));
    let scene = createScene(canvas, userState^);
    setNodeParents(scene.root);
    calcLayout(scene);
    createDrawStates(scene);
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

let makeUniform = (name, glType) => {
    (name, UniformItem(Gpu.Uniform.make(name, glType)))
};

let makeUniformFloat = (name, floatVal) => {
    (name, UniformValItem(Gpu.Uniform.UniformFloat(floatVal)))
};

let makeUniformVec2f = (name, vec2vals) => {
    (name, UniformValItem(Gpu.Uniform.UniformVec2f(vec2vals)))
};

let makeUniformVec3f = (name, vec3vals) => {
    (name, UniformValItem(Gpu.Uniform.UniformVec3f(vec3vals)))
};

let makeUniformMat3f = (name, mat3Vals) => {
    (name, UniformValItem(Gpu.Uniform.UniformMat3f(mat3Vals)))
};