/* Scene nodes maps roughly to "draw states", a program draw with
   uniforms and textures.
   They also provide layout capabilities.

   A scene allows scene nodes to share resources between them.
   By using keys/refs for resources, the system searches upwards
   in the tree by the nodes parents, as well as the dependencies
   on each step.

   The nodes will have some layout uniforms available.
   By specifying a uniform name corresponding to a provided
   value, the system will set values when calculating the layout.

   Each update/draw of the scene will have provided a list of
   user defined "flags". These are meant to signal which part
   of the scene needs to be redrawn.
*/
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

type childLayout =
  | Horizontal
  | Vertical;

type hAlign =
  | AlignCenter
  | AlignLeft
  | AlignRight;

type vAlign =
  | AlignTop
  | AlignMiddle
  | AlignBottom;

/* Scale of available space, or pixel value */
type dimension =
  | Scale(float)
  | Pixel(float);

/* Size by dimensions or aspect with best fit */
type blockSize =
  | Dimensions(dimension, dimension)
  | Aspect(float);

/* user defined state and update flags */
type t('state, 'flags) = {
    state: 'state,
    canvas: Gpu.Canvas.t,
    root: node('state, 'flags),
    mutable inited: bool,
    initFlag: 'flags,
    frameFlag: 'flags,
    resizeFlag: 'flags,
    updateLists: Hashtbl.t(list('flags), list(node('state, 'flags)))
}
and node('state, 'flags) = {
    key: string,
    updateOn: list('flags),
    mutable drawState: option(Gpu.DrawState.t),
    update: option((node('state, 'flags), 'state, list('flags)) => unit),
    layout: layout,
    calcedLayout: calcedLayout,
    selfDraw: bool,
    transparent: bool,
    deps: list(node('state, 'flags)),
    children: list(node('state, 'flags)),
    vertShader: option(Gpu.Shader.t),
    fragShader: option(Gpu.Shader.t),
    textureList: list(string),
    textures: Hashtbl.t(string, texture),
    vertices: vertices,
    indices: indices,
    uniformList: list(string),
    uniforms: Hashtbl.t(string, uniform),
    uniformVals: Hashtbl.t(string, uniformVal),
    mutable parent: option(node('state, 'flags))
}
and layout = {
    size: blockSize,
    padding: option(dimension),
    spacing: option(dimension),
    childLayout: childLayout,
    hAlign: hAlign,
    vAlign: vAlign
}
and calcedLayout = {
    mutable pixelWidth: float,
    mutable pixelHeight: float,
    mutable pixelXOffset: float,
    mutable pixelYOffset: float
};

let quadVertices = Gpu.VertexBuffer.makeQuad(());
let quadIndices = Gpu.IndexBuffer.makeQuad();

let makeLayout = (
    ~size=Dimensions(Scale(1.0), Scale(1.0)),
    ~padding=?,
    ~spacing=?,
    ~childLayout=Horizontal,
    ~hAlign=AlignCenter,
    ~vAlign=AlignTop,
    ()
    ) : layout => {
    {
        size,
        padding,
        spacing,
        childLayout,
        hAlign,
        vAlign
    }
};
let makeNode = (
    key,
    ~vertShader=?,
    ~fragShader=?,
    ~updateOn=[],
    ~update=?,
    ~children=[],
    ~textures=[],
    ~vertices=?,
    ~indices=?,
    ~uniforms=[],
    ~uniformVals=[],
    ~size=Dimensions(Scale(1.0), Scale(1.0)),
    ~padding=?,
    ~spacing=?,
    ~childLayout=Horizontal,
    ~hAlign=AlignCenter,
    ~vAlign=AlignTop,
    ~selfDraw=true,
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
        let layout = {
            size,
            padding,
            spacing,
            childLayout,
            hAlign,
            vAlign
        };
    {
        key,
        update,
        updateOn,
        drawState: None,
        layout,
        calcedLayout: {
            pixelWidth: 0.0,
            pixelHeight: 0.0,
            pixelXOffset: 0.0,
            pixelYOffset: 0.0
        },
        selfDraw,
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

let make = (canvas, state, initFlag, frameFlag, resizeFlag, root) => {
    {
        state,
        canvas,
        root,
        inited: false,
        initFlag,
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

let createNodeDrawState = (self, node) => {
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
    let vertShader = switch (node.vertShader) {
    | Some(vertShader) => vertShader
    | None => failwith("Vertex shader not found on: " ++ node.key)
    };
    let fragShader = switch (node.fragShader) {
    | Some(fragShader) => fragShader
    | None => failwith("Fragment shader not found on: " ++ node.key)
    };
    node.drawState = Some(Gpu.DrawState.init(
        self.canvas.context,
        Gpu.Program.make(
            vertShader,
            fragShader,
            uniforms
        ),
        uniformVals,
        vertices,
        indices,
        textures
    ));
};

let createDrawStates = (self) => {
    let rec loop = (node) => {
        if (node.selfDraw) {
            createNodeDrawState(self, node);
        };
        List.iter((dep) => loop(dep), node.deps);
        List.iter((child) => loop(child), node.children);
    };
    loop(self.root)
};

let calcLayout = (self) => {
    let vpWidth = float_of_int(self.canvas.width);
    let vpHeight = float_of_int(self.canvas.height);
    let calcNodeDimensions = (node, paddedWidth, paddedHeight) => {
        let (nodeWidth, nodeHeight) = switch (node.layout.size) {
        | Aspect(ratio) =>
            let parentAspect = paddedWidth /. paddedHeight;
            if (ratio < parentAspect) {
                /* Limit by height */
                let width = paddedHeight *. ratio;
                (width, paddedHeight)
            } else {
                /* Limit by width */
                let height = paddedWidth /. ratio;
                (paddedWidth, height)
            }
        | Dimensions(dimX, dimY) =>
            (
                switch (dimX) {
                | Pixel(pixels) => pixels
                | Scale(scale) => paddedWidth *. scale
                },
                switch (dimY) {
                | Pixel(pixels) => pixels
                | Scale(scale) => paddedHeight *. scale
                }
            )
        };
        node.calcedLayout.pixelWidth = nodeWidth;
        node.calcedLayout.pixelHeight = nodeHeight;
    };
    let rec calcNodeLayout = (node, padding, constrainWidth, constrainHeight, x, y) => {
        let (paddedWidth, paddedHeight) = switch (padding) {
        | None => (constrainWidth, constrainHeight)
        | Some(Pixel(padding)) => 
            (constrainWidth -. padding, constrainHeight -. padding)
        | Some(Scale(padding)) =>
            (constrainWidth -. (constrainWidth *. padding), constrainHeight -. (constrainHeight *. padding))
        };
        List.iter((dep) => calcNodeDimensions(dep, paddedWidth, paddedHeight), node.deps);
        List.iter((child) => calcNodeDimensions(child, paddedWidth, paddedHeight), node.children);
        /* Handle aligns */
        switch (node.layout.childLayout) {
        | Horizontal =>
            let spacing = switch (node.layout.spacing) {
            | Some(Pixel(pixel)) => pixel
            | Some(Scale(scale)) => paddedWidth *. scale
            | None => 0.0
            };
            /* Set xoffset */
            switch (node.layout.hAlign) {
            | AlignLeft =>
                let _ = List.fold_left((xOffset, child) => {
                    child.calcedLayout.pixelXOffset = xOffset;
                    xOffset +. spacing +. child.calcedLayout.pixelWidth
                }, x, node.children);
            | AlignCenter =>
                /* Get total width and start from paddedWidth + ((paddedWidth - totalWidth) / 2) */
                let totalWidth = List.fold_left((totalWidth, child) => {
                    totalWidth +. spacing +. child.calcedLayout.pixelWidth
                }, 0.0, node.children);
                let xOffset = paddedWidth +. ((paddedWidth -. totalWidth) /. 2.0);
                let _ = List.fold_left((xOffset, child) => {
                    child.calcedLayout.pixelXOffset = xOffset;
                    xOffset +. spacing +. child.calcedLayout.pixelWidth
                }, x +. xOffset, node.children);
            | AlignRight =>
                let _ = List.fold_right((child, xOffset) => {
                    child.calcedLayout.pixelXOffset = xOffset -. child.calcedLayout.pixelWidth;
                    child.calcedLayout.pixelXOffset -. spacing
                }, node.children, x +. paddedWidth);
            };
            switch (node.layout.vAlign) {
            | AlignTop => ()
            | AlignMiddle =>
                List.iter((child) => {
                    child.calcedLayout.pixelYOffset = y +. (paddedHeight -. child.calcedLayout.pixelHeight) /. 2.0;
                }, node.children);
            | AlignBottom =>
                List.iter((child) => {
                    child.calcedLayout.pixelYOffset = y +. paddedHeight -. child.calcedLayout.pixelHeight;
                }, node.children);
            };
            /* Get total width */
        | Vertical =>
            let spacing = switch (node.layout.spacing) {
            | Some(Pixel(pixel)) => pixel
            | Some(Scale(scale)) => paddedHeight *. scale
            | None => 0.0
            };
            switch (node.layout.hAlign) {
            | AlignLeft => 
                List.iter((child) => {
                    child.calcedLayout.pixelXOffset = x;
                }, node.children);
            | AlignCenter =>
                List.iter((child) => {
                    child.calcedLayout.pixelXOffset = x +. (paddedWidth -. child.calcedLayout.pixelWidth) /. 2.0;
                }, node.children);
            | AlignRight =>
                List.iter((child) => {
                    child.calcedLayout.pixelXOffset = x +. paddedWidth -. child.calcedLayout.pixelWidth;
                }, node.children);
            };
            switch (node.layout.vAlign) {
            | AlignTop => ()
            | AlignMiddle =>
                List.iter((child) => {
                    child.calcedLayout.pixelYOffset = y +. (paddedHeight -. child.calcedLayout.pixelHeight) /. 2.0;
                }, node.children);
            | AlignBottom =>
                List.iter((child) => {
                    child.calcedLayout.pixelYOffset = y +. paddedHeight -. child.calcedLayout.pixelHeight;
                }, node.children);
            };
        };
        let scale = Coords.Mat3.scale(node.calcedLayout.pixelWidth /. vpWidth, node.calcedLayout.pixelHeight /. vpHeight);
        let translate = Coords.Mat3.trans(node.calcedLayout.pixelXOffset /. vpWidth, node.calcedLayout.pixelYOffset /. vpHeight);
        let layoutMat = Coords.Mat3.matmul(translate, scale);
        if (Hashtbl.mem(node.uniforms, "layout")) {
            Hashtbl.replace(node.uniformVals, "layout", UniformValItem(Gpu.Uniform.UniformMat3f(layoutMat)));
        };
        List.iter((dep) => {
            calcNodeLayout(
                dep,
                node.layout.padding,
                node.calcedLayout.pixelWidth,
                node.calcedLayout.pixelHeight,
                node.calcedLayout.pixelXOffset,
                node.calcedLayout.pixelYOffset
            )
        }, node.deps);
        List.iter((child) => {
            calcNodeLayout(
                child,
                node.layout.padding,
                node.calcedLayout.pixelWidth,
                node.calcedLayout.pixelHeight,
                node.calcedLayout.pixelXOffset,
                node.calcedLayout.pixelYOffset
            )
        }, node.children);
    };
    calcNodeDimensions(self.root, vpWidth, vpHeight);
    calcNodeLayout(self.root, None, vpWidth, vpHeight, 0.0, 0.0);
};

let draw = (self, node) => {
    Js.log("Drawing " ++ node.key);
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
            if (!scene.inited) {
                update(scene, [scene.initFlag]);
                scene.inited = true;
            };
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