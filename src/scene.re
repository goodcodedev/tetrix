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

type childLayout =
  | Horizontal
  | Vertical
  | Stacked;

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

type easing =
  | Linear;

let currNodeId = ref(0);

/* Not thread safe :) */
let nextNodeId = () => {
    let nodeId = currNodeId^;
    currNodeId := nodeId + 1;
    nodeId
};

type fbufferConfig = {
    width: int,
    height: int
};

/* user defined state and update flags */
type t('state, 'flags) = {
    state: 'state,
    canvas: Gpu.Canvas.t,
    root: node('state, 'flags),
    mutable inited: bool,
    initFlag: 'flags,
    frameFlag: 'flags,
    resizeFlag: 'flags,
    updateLists: Hashtbl.t(updateState('flags), list(node('state, 'flags))),
    mutable loadingNodes: list((list('flags), node('state, 'flags))),
    mutable anims: list(anim('state, 'flags)),
    fbuffer: Hashtbl.t(fbufferConfig, Gpu.FrameBuffer.inited)
}
and updateState('flags) = {
    flags: list('flags),
    anims: list(int)
}
and node('state, 'flags) = {
    key: string,
    id: int,
    updateOn: list('flags),
    mutable drawState: option(Gpu.DrawState.t),
    update: option((node('state, 'flags), 'state, list('flags)) => unit),
    layout: layout,
    calcLayout: calcLayout,
    selfDraw: bool,
    transparent: bool,
    mutable loading: bool,
    deps: list(node('state, 'flags)),
    children: list(node('state, 'flags)),
    vertShader: option(Gpu.Shader.t),
    fragShader: option(Gpu.Shader.t),
    textureList: list(string),
    textures: Hashtbl.t(string, Gpu.Texture.t),
    vertices: Gpu.VertexBuffer.t,
    indices: Gpu.IndexBuffer.t,
    uniformList: list(string),
    uniforms: Hashtbl.t(string, Gpu.uniform),
    layoutUniform: option(Gpu.uniform),
    pixelSizeUniform: option(Gpu.uniform),
    textureUniforms: list((node('state, 'flags), string, Gpu.uniform)),
    drawToTexture: option(Gpu.Texture.t),
    mutable parent: option(node('state, 'flags)),
    mutable scene: option(t('state, 'flags))
}
and layout = {
    size: blockSize,
    padding: option(dimension),
    spacing: option(dimension),
    childLayout: childLayout,
    hAlign: hAlign,
    vAlign: vAlign
}
and calcLayout = {
    mutable pWidth: float,
    mutable pHeight: float,
    mutable pXOffset: float,
    mutable pYOffset: float
}
and anim('state, 'flags) = {
    node: node('state, 'flags),
    onFrame: (t('state, 'flags), node('state, 'flags), anim('state, 'flags)) => unit,
    duration: float,
    mutable elapsed: float,
    next: option(anim('state, 'flags))
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
    ~layoutUniform=false,
    ~pixelSizeUniform=false,
    ~textureUniforms=[],
    ~size=Dimensions(Scale(1.0), Scale(1.0)),
    ~padding=?,
    ~spacing=?,
    ~childLayout=Horizontal,
    ~hAlign=AlignCenter,
    ~vAlign=AlignTop,
    ~selfDraw=true,
    ~loading=false,
    ~transparent=false,
    ~deps=[],
    ~drawToTexture=?,
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
        /* System uniforms */
        /* Could also allow stringly typed uniforms to create this,
           There is a slight advantage to not search map
           for each node in layout calculation, so nice to
           have datatype */
        let layoutUniform = if (layoutUniform) {
            Some(Gpu.UniformMat3f(ref(Data.Mat3.id())))
        } else {
            None
        };
        let pixelSizeUniform = if (pixelSizeUniform) {
            Some(Gpu.UniformVec2f(ref(Data.Vec2.zeros())))
        } else {
            None
        };
        let textureUniforms = List.map(((name, node)) => {
            (node, name, Gpu.UniformMat3f(ref(Data.Mat3.id())))
        }, textureUniforms);
        let vertices = switch(vertices) {
        | Some(vertices) => vertices
        | None => quadVertices
        };
        let indices = switch(indices) {
        | Some(indices) => indices
        | None => quadIndices
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
        id: nextNodeId(),
        update,
        updateOn,
        drawState: None,
        layout,
        calcLayout: {
            pWidth: 0.0,
            pHeight: 0.0,
            pXOffset: 0.0,
            pYOffset: 0.0
        },
        selfDraw,
        loading,
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
        layoutUniform,
        pixelSizeUniform,
        textureUniforms,
        drawToTexture,
        parent: None,
        scene: None
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
        updateLists: Hashtbl.create(5),
        loadingNodes: [],
        anims: [],
        fbuffer: Hashtbl.create(1)
    }
};

let makeAnim = (node, onFrame, duration, ~next=?, ()) => {
    {
        node,
        onFrame,
        duration,
        elapsed: 0.0,
        next
    }
};

let setNodeParentsAndScene = (self) => {
    let rec loop = (node, parent) => {
        node.parent = parent;
        node.scene = Some(self);
        List.iter((dep) => loop(dep, Some(node)), node.deps);
        List.iter((child) => loop(child, Some(node)), node.children);
    };
    loop(self.root, None)
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

let getSceneNodesToUpdate = (flags, animIds, root) => {
    let rec loop = (node, list, inAnims) => {
        let inAnims = (inAnims || List.exists((animId) => node.id == animId, animIds));
        let hasAnyFlag = List.exists((updateOn) => List.exists((flag) => flag == updateOn, flags), node.updateOn);
        /* todo: tail recursive? */
        let depsList = List.fold_left((list, dep) => loop(dep, list, inAnims), list, node.deps);
        let childList = List.fold_left((list, child) => loop(child, list, inAnims), depsList, node.children);
        if ((hasAnyFlag || inAnims) && node.selfDraw) {
            [node, ...childList]
        } else {
            childList
        }
    };
    loop(root, [], List.exists((animId) => root.id == animId, animIds))
};

let createNodeDrawState = (self, node) => {
    /* Texture uniforms */
    let uniforms = List.fold_left((uniforms, (_, name, uniform)) => {
        [Gpu.Uniform.make(name, uniform), ...uniforms]
    }, [], node.textureUniforms);
    /* PixelSize uniform */
    let uniforms = switch (node.pixelSizeUniform) {
    | Some(pixelSizeUniform) => [Gpu.Uniform.make("pixelSize", pixelSizeUniform), ...uniforms]
    | None => uniforms
    };
    /* Layout uniform */
    let uniforms = switch (node.layoutUniform) {
    | Some(layoutUniform) => [Gpu.Uniform.make("layout", layoutUniform), ...uniforms]
    | None => uniforms
    };
    let uniforms = Array.of_list(List.append(List.map(
        (key) => {
            Gpu.Uniform.make(key, Hashtbl.find(node.uniforms, key))
        },
        node.uniformList
    ), uniforms));
    let textures = Array.of_list(List.map(
        (key) => {
            let texture = Hashtbl.find(node.textures, key);
            Gpu.ProgramTexture.make(key, texture)
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
        node.vertices,
        node.indices,
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

/* Not really breadth first, which may be most intuitive
   but with a little overhead. OCaml lists should be
   good for it though.
   Breadth first is also confusing wrt deps vs children.
   Should deps be searched completely before children,
   or same level wise, maybe after children altogether.
   For global search, maybe a better solution is to
   maintain a key map maybe with level.
*/
let getNode = (self, key) => {
    let rec searchList = (list) => {
        switch (list) {
        | [] => None
        | [node, ...rest] => if (node.key == key) {
            Some(node)
        } else {
            searchList(rest)
        }
        }
    }
    and traverseList = (list) => {
        switch (list) {
        | [] => None
        | [node, ...rest] =>
            switch (traverse(node)) {
            | Some(node) => Some(node)
            | None => traverseList(rest)
            }
        }
    }
    and traverse = (node) => {
        switch (searchList(node.children)) {
        | Some(node) => Some(node)
        | None =>
            switch (searchList(node.deps)) {
            | Some(node) => Some(node)
            | None =>
                switch (traverseList(node.children)) {
                | Some(node) => Some(node)
                | None =>
                    traverseList(node.deps)
                }
            }
        }
    };
    if (self.root.key == key) {
        Some(self.root)
    } else {
        traverse(self.root)
    }
};

let setUniformFloat = (node, key, value) => {
    let uniform = Hashtbl.find(node.uniforms, key);
    Gpu.Uniform.setFloat(uniform, value);
};

let setUniformVec2f = (node, key, value) => {
    let uniform = Hashtbl.find(node.uniforms, key);
    Gpu.Uniform.setVec2f(uniform, value);
};

let setUniformMat3f = (node, key, value) => {
    let uniform = Hashtbl.find(node.uniforms, key);
    Gpu.Uniform.setMat3f(uniform, value);
};

let calcLayout = (self) => {
    let debug = false;
    let vpWidth = float_of_int(self.canvas.width);
    let vpHeight = float_of_int(self.canvas.height);
    let vpWidthCenter = vpWidth /. 2.0;
    let vpHeightMiddle = vpHeight /. 2.0;
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
        node.calcLayout.pWidth = nodeWidth;
        node.calcLayout.pHeight = nodeHeight;
    };
    let rec calcNodeLayout = (node) => {
        let layout = node.layout;
        let calcLayout = node.calcLayout;
        /* Calc padding */
        let (paddedWidth, paddedHeight, x, y) = switch (layout.padding) {
        | None => (calcLayout.pWidth, calcLayout.pHeight, calcLayout.pXOffset, calcLayout.pYOffset)
        | Some(Pixel(padding)) => 
            (
                calcLayout.pWidth -. padding *. 2.0,
                calcLayout.pHeight -. padding *. 2.0,
                calcLayout.pXOffset +. padding,
                calcLayout.pYOffset +. padding
            )
        | Some(Scale(padding)) =>
            let scaledXPadding = (calcLayout.pWidth *. padding);
            let scaledYPadding = (calcLayout.pHeight *. padding);
            (
                calcLayout.pWidth -. scaledXPadding *. 2.0,
                calcLayout.pHeight -. scaledYPadding *. 2.0,
                calcLayout.pXOffset +. scaledXPadding,
                calcLayout.pYOffset +. scaledYPadding
            )
        };
        /* Set width/height of children and deps */
        List.iter((dep) => calcNodeDimensions(dep, paddedWidth, paddedHeight), node.deps);
        List.iter((child) => calcNodeDimensions(child, paddedWidth, paddedHeight), node.children);
        /* Todo: allow to set a pixel value or something for one child,
           then allow one of the other elements to stretch to available space */
        /* Handle aligns */
        switch (layout.childLayout) {
        | Stacked =>
            List.iter((child) => {
                child.calcLayout.pXOffset = x;
                child.calcLayout.pYOffset = y;
            }, node.children);
        | Horizontal =>
            let spacing = switch (layout.spacing) {
            | Some(Pixel(pixel)) => pixel
            | Some(Scale(scale)) => paddedWidth *. scale
            | None => 0.0
            };
            /* Set xoffset */
            switch (layout.hAlign) {
            | AlignLeft =>
                let _ = List.fold_left((xOffset, child) => {
                    child.calcLayout.pXOffset = xOffset;
                    xOffset +. spacing +. child.calcLayout.pWidth
                }, x, node.children);
            | AlignCenter =>
                /* Get total width and start from (paddedWidth  - totalWidth) / 2) */
                let totalWidth = List.fold_left((totalWidth, child) => {
                    totalWidth +. child.calcLayout.pWidth
                }, 0.0, node.children) +. spacing *. float_of_int(List.length(node.children) - 1);
                let xOffset = (paddedWidth -. totalWidth) /. 2.0;
                let _ = List.fold_left((xOffset, child) => {
                    child.calcLayout.pXOffset = xOffset;
                    xOffset +. spacing +. child.calcLayout.pWidth
                }, x +. xOffset, node.children);
            | AlignRight =>
                let _ = List.fold_right((child, xOffset) => {
                    child.calcLayout.pXOffset = xOffset -. child.calcLayout.pWidth;
                    child.calcLayout.pXOffset -. spacing
                }, node.children, x +. paddedWidth);
            };
            switch (layout.vAlign) {
            | AlignTop =>
                List.iter((child) => {
                    child.calcLayout.pYOffset = y;
                }, node.children);
            | AlignMiddle =>
                List.iter((child) => {
                    child.calcLayout.pYOffset = y +. (paddedHeight -. child.calcLayout.pHeight) /. 2.0;
                }, node.children);
            | AlignBottom =>
                List.iter((child) => {
                    child.calcLayout.pYOffset = y +. paddedHeight -. child.calcLayout.pHeight;
                }, node.children);
            };
            /* Get total width */
        | Vertical =>
            let spacing = switch (layout.spacing) {
            | Some(Pixel(pixel)) => pixel
            | Some(Scale(scale)) => paddedHeight *. scale
            | None => 0.0
            };
            switch (layout.hAlign) {
            | AlignLeft => 
                List.iter((child) => {
                    child.calcLayout.pXOffset = x;
                }, node.children);
            | AlignCenter =>
                List.iter((child) => {
                    child.calcLayout.pXOffset = x +. (paddedWidth -. child.calcLayout.pWidth) /. 2.0;
                }, node.children);
            | AlignRight =>
                List.iter((child) => {
                    child.calcLayout.pXOffset = x +. paddedWidth -. child.calcLayout.pWidth;
                }, node.children);
            };
            switch (layout.vAlign) {
            | AlignTop =>
                let _ = List.fold_left((yOffset, child) => {
                    child.calcLayout.pYOffset = yOffset;
                    yOffset +. child.calcLayout.pHeight +. spacing
                }, y, node.children);
            | AlignMiddle =>
                let totalHeight = List.fold_left((totalHeight, child) => {
                    totalHeight +. child.calcLayout.pHeight
                }, 0.0, node.children) +. spacing *. float_of_int(List.length(node.children) - 1);
                let _ = List.fold_left((yOffset, child) => {
                    child.calcLayout.pYOffset = yOffset;
                    yOffset +. child.calcLayout.pYOffset +. spacing
                }, y +. (paddedHeight -. totalHeight) /. 2.0 ,node.children);
            | AlignBottom =>
                let _ = List.fold_right((child, yOffset) => {
                    child.calcLayout.pYOffset = yOffset -. child.calcLayout.pHeight;
                    child.calcLayout.pYOffset -. spacing
                }, node.children, y +. paddedHeight);
            };
        };
        if (debug) {
            Js.log("Layout for " ++ node.key);
            Js.log2("pWidth: ", calcLayout.pWidth);
            Js.log2("pHeight: ", calcLayout.pHeight);
            Js.log2("xOff: ", calcLayout.pXOffset);
            Js.log2("yOff: ", calcLayout.pYOffset);
        };
        switch (node.layoutUniform) {
        | None => ();
        | Some(layoutUniform) =>
            let scaleX = calcLayout.pWidth /. vpWidth;
            let scaleY = calcLayout.pHeight /. vpHeight;
            let scale = Data.Mat3.scale(scaleX, scaleY);
            switch (node.drawToTexture) {
            | Some(texture) =>
                /* Translate texture to begin from 0,0.
                   The important thing is correspondance with texture
                   uniforms below */
                /* Todo: maintain values for quick lookup? */
                let (texWidth, texHeight) = switch (Gpu.Texture.getSize(texture)) {
                | Some((texWidth, texHeight)) => (float_of_int(texWidth), float_of_int(texHeight))
                | None => failwith("Could not find texture size");
                };
                let translate = Data.Mat3.trans(
                    (texWidth -. scaleX *. texWidth) /. texWidth *. -1.0,
                    (texHeight -. scaleY *. texHeight) /. texHeight
                );
                let layoutMat = Data.Mat3.matmul(translate, scale);
                Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
                if (debug) {
                    Js.log2("Layout: ", layoutMat);
                };
            | None =>
                /* Can this be simplified? */
                let translate = Data.Mat3.trans(
                    ((calcLayout.pXOffset +. (calcLayout.pWidth /. 2.0) -. vpWidthCenter) /. vpWidth *. 2.0),
                    ((calcLayout.pYOffset +. (calcLayout.pHeight /. 2.0) -. vpHeightMiddle) /. vpHeight *. -2.0)
                );
                let layoutMat = Data.Mat3.matmul(translate, scale);
                Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
                if (debug) {
                    Js.log2("Scale: ", scale);
                    Js.log2("Translate: ", translate);
                    Js.log2("Layout: ", layoutMat);
                };
            }
        };
        switch (node.pixelSizeUniform) {
        | None => ();
        | Some(pixelSizeUniform) =>
            Gpu.Uniform.setVec2f(pixelSizeUniform, Data.Vec2.make(
                node.calcLayout.pWidth,
                node.calcLayout.pHeight
            ));
        };
        List.iter((dep) => {
            calcNodeLayout(dep);
        }, node.deps);
        List.iter((child) => {
            calcNodeLayout(child);
        }, node.children);
        /* After deps and children have been processed, set texture matrices */
        List.iter(((texNode, _name, uniform)) => {
            /* Textures should be scaled for right pixel ratio,
               need not be translated (could be packed maybe),
               but should start from beginning or pack position */
            let scaleX = texNode.calcLayout.pWidth /. vpWidth;
            let scaleY = texNode.calcLayout.pHeight /. vpHeight;
            let scale = Data.Mat3.scale(
                scaleX,
                scaleY *. -1.0
            );
            let translate = Data.Mat3.trans(1.0, -1.0);
            let texMat = Data.Mat3.matmul(translate, scale);
            Gpu.Uniform.setMat3f(uniform, texMat);
                Js.log2("texMat: ", texMat);
            if (debug) {
                Js.log2("texMat: ", texMat);
            };
        }, node.textureUniforms);
    };
    if (debug) {
        Js.log2("vpWidth", vpWidth);
        Js.log2("vpHeight", vpHeight);
    };
    calcNodeDimensions(self.root, vpWidth, vpHeight);
    self.root.calcLayout.pXOffset = 0.0;
    self.root.calcLayout.pYOffset = 0.0;
    calcNodeLayout(self.root);
};


let getFBuffer = (self, config : fbufferConfig) => {
    if (Hashtbl.mem(self.fbuffer, config)) {
        Hashtbl.find(self.fbuffer, config)
    } else {
        let fbuffer = Gpu.FrameBuffer.init(
            Gpu.FrameBuffer.make(config.width, config.height),
            self.canvas.context
        );
        Hashtbl.add(self.fbuffer, config, fbuffer);
        fbuffer
    }
};

let draw = (self, node) => {
    /*Js.log("Drawing " ++ node.key);*/
    switch (node.drawState) {
    | Some(drawState) =>
        let drawToTexture = switch (node.drawToTexture) {
        | Some(texture) =>
            let config = {
                width: 1024,
                height: 1024
            };
            let fbuffer = getFBuffer(self, config);
            Gpu.FrameBuffer.bindTexture(fbuffer, self.canvas.context, texture);
            Gpu.Canvas.setFramebuffer(self.canvas, fbuffer);
            true
        | None => false
        };
        if (node.transparent) {
            let context = self.canvas.context;
            Gpu.Gl.enable(~context, Gpu.Constants.blend);
            Gpu.Gl.blendFunc(~context, Gpu.Constants.src_alpha, Gpu.Constants.one_minus_src_alpha);
            Gpu.DrawState.draw(drawState, self.canvas);
            Gpu.Gl.disable(~context, Gpu.Constants.blend);
        } else {
            Gpu.DrawState.draw(drawState, self.canvas);
        };
        if (drawToTexture) {
            Gpu.Canvas.clearFramebuffer(self.canvas);
        };
    | None => failwith("Drawstate not found")
    };
};

let update = (self, updateFlags) => {
    /* Anims */
    let animIds = List.sort((a, b) => (a < b) ? -1 : 1, List.map((anim) => anim.node.id, self.anims));
    let rec doAnims = (anims) => {
        switch (anims) {
        | [] => []
        | [anim, ...rest] =>
            anim.elapsed = anim.elapsed +. self.canvas.deltaTime;
            anim.onFrame(self, anim.node, anim);
            if (anim.elapsed >= anim.duration) {
                doAnims(rest)
            } else {
                [anim, ...doAnims(rest)]
            }
        }
    };
    self.anims = doAnims(self.anims);
    let sortedFlags = List.sort((a, b) => (a < b) ? -1 : 1, updateFlags);
    let updateState = {
        flags: sortedFlags,
        anims: animIds
    };
    if (!Hashtbl.mem(self.updateLists, updateState)) {
        Hashtbl.add(self.updateLists, updateState, getSceneNodesToUpdate(sortedFlags, animIds, self.root));
    };
    /* Check if any node in loadingNodes is loaded */
    let rec checkLoaded = (loadingNodes) => {
        switch (loadingNodes) {
        | [] => []
        | [(_updateList, node) as item, ...rest] =>
            if (!node.loading) {
                /* Todo: Draw with updateList for the area of the node or something */
                switch (node.update) {
                | Some(update) => update(node, self.state, sortedFlags)
                | None => draw(self, node)
                };
                checkLoaded(rest)
            } else {
                [item, ...checkLoaded(rest)]
            }
        }
    };
    self.loadingNodes = checkLoaded(self.loadingNodes);
    /* todo: possibly optimize with a second transformed data structure
       so the drawstate etc is readily available */
    List.iter((node) => {
        if (node.loading) {
            self.loadingNodes = [(updateFlags, node), ...self.loadingNodes]
        } else {
            switch (node.update) {
            | Some(update) => update(node, self.state, sortedFlags)
            | None => draw(self, node)
            };
        };
    }, Hashtbl.find(self.updateLists, updateState));
};

module Gl = Reasongl.Gl;

let run = (width, height, setup, createScene, draw, ~keyPressed=?, ~resize=?, ()) => {
    let canvas = Gpu.Canvas.init(width, height);
    let userState = ref(setup(canvas));
    let scene = createScene(canvas, userState^);
    setNodeParentsAndScene(scene);
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

let doAnim = (scene, anim) => {
    scene.anims = [anim, ...scene.anims];
};

module UFloat {
    let make = (value) => Gpu.UniformFloat(ref(value));
    let zero = () => Gpu.UniformFloat(ref(0.0));
    let one = () => Gpu.UniformFloat(ref(1.0));
};

module UVec2f {
    let zeros = () => Gpu.UniformVec2f(ref(Data.Vec2.zeros()));
    let vals = (a, b, c) => Gpu.UniformVec2f(ref(Data.Vec2.make(a, b)));
    let fromArray = (arr) => Gpu.UniformVec2f(ref(Data.Vec2.fromArray(arr)));
};

module UVec3f {
    let zeros = () => Gpu.UniformVec3f(ref(Data.Vec3.zeros()));
    let vals = (a, b, c) => Gpu.UniformVec3f(ref(Data.Vec3.make(a, b, c)));
    let fromArray = (arr) => Gpu.UniformVec3f(ref(Data.Vec3.fromArray(arr)));
};

module UVec4f {
    let zeros = () => Gpu.UniformVec4f(ref(Data.Vec4.zeros()));
    let vals = (a, b, c, d) => Gpu.UniformVec4f(ref(Data.Vec4.make(a, b, c, d)));
    let fromArray = (arr) => Gpu.UniformVec4f(ref(Data.Vec4.fromArray(arr)));
};

module UMat3f {
    let id = () => Gpu.UniformMat3f(ref(Data.Mat3.id()));
    let mat = (mat) => Gpu.UniformMat3f(ref(mat));
};