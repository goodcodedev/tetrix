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

type margin =
  | Margin(dimension)
  | MarginXY(dimension, dimension)
  | MarginRBLT(dimension, dimension, dimension, dimension);

/* Size by dimensions or aspect with best fit in container */
type blockSize =
| Dimensions(dimension, dimension)
| WidthRatio(dimension, float)
| HeightRatio(dimension, float)
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
    textures: Hashtbl.t(string, nodeTex('state, 'flags)),
    vertices: Gpu.VertexBuffer.t,
    indices: option(Gpu.IndexBuffer.t),
    uniformList: list(string),
    uniforms: Hashtbl.t(string, Gpu.uniform),
    layoutUniform: option(Gpu.uniform),
    pixelSizeUniform: option(Gpu.uniform),
    drawToTexture: option(Gpu.Texture.t),
    clearOnDraw: bool,
    mutable parent: option(node('state, 'flags)),
    mutable scene: option(t('state, 'flags))
}
and layout = {
    size: blockSize,
    padding: option(dimension),
    margin: option(margin),
    childLayout: childLayout,
    spacing: option(dimension),
    hAlign: hAlign,
    vAlign: vAlign
}
and calcLayout = {
    mutable pWidth: float,
    mutable pHeight: float,
    mutable marginX1: float,
    mutable marginX2: float,
    mutable marginY1: float,
    mutable marginY2: float,
    mutable inWidth: float,
    mutable inHeight: float,
    mutable pXOffset: float,
    mutable pYOffset: float
}
and anim('state, 'flags) = {
    node: node('state, 'flags),
    onFrame: (t('state, 'flags), node('state, 'flags), anim('state, 'flags)) => unit,
    duration: float,
    mutable elapsed: float,
    next: option(anim('state, 'flags))
}
and nodeTex('state, 'flags) = {
    texture: Gpu.Texture.t,
    node: option(node('state, 'flags)),
    uniformMat: option(Gpu.uniform),
    offsetX: dimension,
    offsetY: dimension
};

let quadVertices = Gpu.VertexBuffer.makeQuad(());
let quadIndices = Gpu.IndexBuffer.makeQuad();

let makeLayout = (
    ~size=Dimensions(Scale(1.0), Scale(1.0)),
    ~padding=?,
    ~margin=?,
    ~spacing=?,
    ~childLayout=Horizontal,
    ~hAlign=AlignCenter,
    ~vAlign=AlignTop,
    ()
) : layout => {
    {
        size,
        padding,
        margin,
        childLayout,
        spacing,
        hAlign,
        vAlign
    }
};
/* todo: greyscale */
type drawTo =
  | Framebuffer
  | TextureRGBA
  | TextureRGB
  | TextureGreyscale
  | TextureItem(Gpu.Texture.t);

module NodeTex {
    let node = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), node) => {
        let texture = switch (node.drawToTexture) {
        | Some(texture) => texture
        | None => failwith("Provided node does not draw to texture");
        };
        {
            texture,
            node: Some(node),
            uniformMat: Some(UniformMat3f(ref(Data.Mat3.id()))),
            offsetX,
            offsetY
        }
    };

    let tex = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), texture) => {
        {
            texture,
            node: None,
            uniformMat: None,
            offsetX,
            offsetY
        }
    };
};

let makeNode = (
    key,
    ~vertShader=?,
    ~fragShader=?,
    ~updateOn=[],
    ~update=?,
    ~children=[],
    ~textures : list((string, nodeTex('state, 'flags))) =[],
    ~vertices=?,
    ~indices=?,
    ~vo : option(Gpu.VertexObject.t) =?,
    ~uniforms=[],
    ~layoutUniform=true,
    ~pixelSizeUniform=false,
    ~size=Dimensions(Scale(1.0), Scale(1.0)),
    ~padding=?,
    ~margin=?,
    ~childLayout=Horizontal,
    ~spacing=?,
    ~hAlign=AlignCenter,
    ~vAlign=AlignTop,
    ~selfDraw=true,
    ~loading=false,
    ~transparent=false,
    ~deps=[],
    ~drawTo=Framebuffer,
    ~clearOnDraw=false,
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
    /* Provided uniform list */
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
    /* Vertices and indices can be passed as
       separate objects, or as a vertex object */
    let (vertices, indices) = switch (vo) {
    | Some(vo) => (vo.vertices, vo.indices)
    | None => switch (vertices, indices) {
    | (Some(vertices), indices) => (vertices, indices)
    | (None, _) => (quadVertices, Some(quadIndices))
    }
    };
    let layout = {
        size,
        padding,
        margin,
        childLayout,
        spacing,
        hAlign,
        vAlign
    };
    /* Easy way to create texture to draw to, this can
       be used in tandem with ~texNodes.
       Todo: pool of textures to draw to? This would
       require some texture refs to be changed
       when there is no more room and a new
       texture needs to be used
       Or possibly the size of the texture could be changed */
    let drawToTexture = switch (drawTo) {
    | Framebuffer =>
        None
    | TextureRGB =>
        let texture = Gpu.Texture.makeEmptyRgb(());
        Some(texture)
    | TextureRGBA =>
        let texture = Gpu.Texture.makeEmptyRgba(());
        Some(texture)
    | TextureGreyscale =>
        let texture = Gpu.Texture.makeEmptyGreyscale(());
        Some(texture)
    | TextureItem(texture) =>
        Some(texture)
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
            inWidth: 0.0,
            inHeight: 0.0,
            marginX1: 0.0,
            marginX2: 0.0,
            marginY1: 0.0,
            marginY2: 0.0,
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
        drawToTexture,
        clearOnDraw,
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

/* Searches deps and parents (with their deps) */
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

type updateRect('state, 'flags) = {
    rect: Shapes.Rect.t,
    stencils: list(Shapes.Rect.t),
    node: node('state, 'flags)
};

/* A caching strategy could be to keep a list of
   whether to update, rects etc based on a easily indexable key/int
   derived from flags and animations (+ other factors).
   It would also be nice to know where the rects
   come from, and whether that thing is visible
   currently.
   At some point, I guess rects should be reconciliated,
   for example if there is intersection, it might be better
   to draw a bounding box, there may be several instances
   of the same rectangle.
   There are advantages to doing this as early as possible
   in the algorithm to avoid propagation of equal/overlapping
   rects, but this hampers caching.
   The better solution might be dependant on the dynamicism
   of the scene. Many dynamic objects would benefit
   from caching of intermediary results, while a static
   scene wouldn't require this and could be somewhat better
   of with a quick algorithm */
type updateListNode('state, 'flags) = {
    mutable update: bool,
    node: node('state, 'flags),
    mutable rects: list(updateRect('state, 'flags)),
    mutable updDeps: list(updateListNode('state, 'flags)),
    mutable updChildren: list(updateListNode('state, 'flags)),
    parent: option(updateListNode('state, 'flags))
};

/* Can also consider using depth buffer
   and draw from closest down parents when
   drawing layout things */
/* Performance is important for this function,
   so will look for more opportunities to cache etc.
   Possibly we could use an array sized for all
   nodes in the three and jump for children
   and deps.
   Use new collections from bucklescript.
   Consider making one version for root without
   parent, and children (loopChild),
   .. just handle root in body of function */
let getUpdateTree = (flags, animIds, root) => {
    let rec loop = (node, parent, isDep, stacked) => {
        /* Self update check, node may also need update if children or deps does */
        /* Parent check could include logic to ensure only area within parent is
           rendered, given that the child is translated or scaled up or something
           (havent encountered this yet, and would come with some cost) */
        let update = (
            (!isDep && switch (parent) {
            | Some(parent) => parent.update
            | None => false
            })
            || List.exists((animId) => node.id == animId, animIds)
            || List.exists((updateOn) => List.exists((flag) => flag == updateOn, flags), node.updateOn)
        );
        let updateNode = {
            update,
            node,
            rects: [],
            updDeps: [],
            updChildren: [],
            parent: parent
        };
        updateNode.updDeps = List.map((dep) => loop(dep, Some(updateNode), true, []), node.deps);
        switch (node.layout.childLayout) {
        | Stacked =>
            /* Treat stacked layout as flattened children
               where the stack nodes from parent are added as last child
               as far as drawing is concerned */
            switch (node.children) {
            | [] =>
                /* Stacked layout but no children */
                switch (stacked) {
                | [] => ();
                | [first, ...rest] => updateNode.updChildren = [loop(first, Some(updateNode), false, rest)];
                };
            | [first, ...rest] =>
                /* Append stacked from parent after this new list
                   of stacked children */
                updateNode.updChildren = [loop(first, Some(updateNode), false, List.append(rest, stacked))]
            };
        | _ =>
            switch (stacked) {
            | [] =>
                /* Normal processing of children */
                updateNode.updChildren = List.map((child) => loop(child, Some(updateNode), false, []), node.children);
            | [first, ...rest] =>
                /* We are in middle of a stacked layout, collect children,
                   and keep next stacked item as last child */
                updateNode.updChildren = List.rev(List.fold_left((list, child) => {
                    [loop(child, Some(updateNode), false, []), ...list]
                }, [loop(first, Some(updateNode), false, rest)], node.children));
            };
        };
        /* Propagate updates to parent until covered by non-transparent */
        let rec requireRect = (rect, parent) => {
            switch (parent) {
            | None => ();
            | Some(parent) =>
                /* Add rect to parent requesting a draw of it */
                if (parent.node.transparent) {
                    /* Parent also transparent, not covered, can't add to stencil */
                    if (!parent.update) {
                        parent.rects = [rect, ...parent.rects];
                    };
                    requireRect(rect, parent.parent);
                } else {
                    let cl = parent.node.calcLayout;
                    let parentRect = Shapes.Rect.make(cl.pXOffset, cl.pYOffset, cl.inWidth, cl.inHeight);
                    if (!Shapes.Rect.contains(parentRect, rect.rect)) {
                        /* Add stencil */
                        let rect = {
                            ...rect,
                            stencils: [
                                Shapes.Rect.make(cl.pXOffset, cl.pYOffset, cl.inWidth, cl.inHeight),
                                ...rect.stencils
                            ]
                        };
                        requireRect(rect, parent.parent);
                    };
                };
            };
        };
        if (node.transparent) {
            /* Create updateRect */
            let cl = node.calcLayout;
            let uRect = {
                rect: Shapes.Rect.make(cl.pXOffset, cl.pYOffset, cl.inWidth, cl.inHeight),
                stencils: [],
                node
            };
            requireRect(uRect, updateNode.parent);
        };
        updateNode
    };
    loop(root, None, false)
};

let getSceneNodesToUpdate = (flags, animIds, root) => {
    let rec loop = (node, list, parentUpdate) => {
        let depsToUpdate = List.fold_left((list, dep) => loop(dep, list, false), [], node.deps);
        let doUpdate = (
            parentUpdate
            || (List.length(depsToUpdate) > 0)
            || List.exists((animId) => node.id == animId, animIds)
            || List.exists((updateOn) => List.exists((flag) => flag == updateOn, flags), node.updateOn)
        );
        /* todo: tail recursive? */
        let childList = List.fold_left((list, child) => loop(child, list, doUpdate), list, node.children);
        let list = if (doUpdate && node.selfDraw) {
            [node, ...childList]
        } else {
            childList
        };
        /* Deps first */
        List.fold_left((list, dep) => [dep, ...list], list, depsToUpdate);
    };
    loop(root, [], false)
};

let createNodeDrawState = (self, node) => {
/* Texture uniforms */
let uniforms = List.fold_left((uniforms, name) => {
    let nodeTex = Hashtbl.find(node.textures, name);
    switch (nodeTex.uniformMat) {
    | Some(uniform) => [Gpu.Uniform.make(name ++ "Mat", uniform), ...uniforms]
    | None => uniforms
    }
}, [], node.textureList);
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
        let nodeTex = Hashtbl.find(node.textures, key);
        Gpu.ProgramTexture.make(key, nodeTex.texture)
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

let setUniformVec3f = (node, key, value) => {
let uniform = Hashtbl.find(node.uniforms, key);
Gpu.Uniform.setVec3f(uniform, value);
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

let calcMargins = (node, outerWidth, outerHeight) => {
    /* Calc margins. Not sure how best to handle
       all cases. Aspect I guess should be kept
       inside the margins, currently the
       margins are scaled by padded dimensions
       around it */
    let cl = node.calcLayout;
    /* Calc margins and dimensions inside margin */
    let xdim = (dim) => switch (dim) {
    | Scale(scale) => outerWidth *. scale
    | Pixel(pixels) => pixels
    };
    let ydim = (dim) => switch (dim) {
    | Scale(scale) => outerHeight *. scale
    | Pixel(pixels) => pixels;
    };
    switch (node.layout.margin) {
    | None => ()
    | Some(margin) => switch (margin) {
        | Margin(dimension) => {
            switch (dimension) {
            | Scale(scale) =>
                let scaledX = outerWidth *. scale;
                let scaledY = outerHeight *. scale;
                cl.marginX1 = scaledX;
                cl.marginX2 = scaledX;
                cl.marginY1 = scaledY;
                cl.marginY2 = scaledY;
            | Pixel(pixels) =>
                cl.marginX1 = pixels;
                cl.marginX2 = pixels;
                cl.marginY1 = pixels;
                cl.marginY2 = pixels;
            };
        };
        | MarginXY(dimX, dimY) => {
            let dimX = xdim(dimX);
            let dimY = ydim(dimY);
            cl.marginX1 = dimX;
            cl.marginX2 = dimX;
            cl.marginY1 = dimY;
            cl.marginY2 = dimY;
        };
        | MarginRBLT(mRight, mBottom, mLeft, mTop) => {
            let dimX2 = xdim(mRight);
            let dimY2 = ydim(mBottom);
            let dimX1 = xdim(mLeft);
            let dimY1 = ydim(mTop);
            cl.marginX1 = dimX1;
            cl.marginX2 = dimX2;
            cl.marginY1 = dimY1;
            cl.marginY2 = dimY2;
        };
    };
    };
};

let calcNodeDimensions = (node, paddedWidth, paddedHeight) => {
    calcMargins(node, paddedWidth, paddedHeight);
    let cl = node.calcLayout;
    let (nodeWidth, nodeHeight) = switch (node.layout.size) {
    | Aspect(ratio) => {
        switch (node.layout.margin) {
        | Some(_) =>
            /* Keeping aspect inside margins */
            let innerWidth = (paddedWidth -. cl.marginX1 -. cl.marginX2);
            let innerHeight = (paddedHeight -. cl.marginY1 -. cl.marginY2);
            let parentAspect = innerWidth /. innerHeight;
            if (ratio < parentAspect) {
                /* Limit by height */
                let innerWidth = innerHeight *. ratio;
                (innerWidth +. cl.marginX1 +. cl.marginX2, paddedHeight)
            } else {
                /* Limit by width */
                let innerHeight = innerWidth /. ratio;
                (paddedWidth, innerHeight +. cl.marginY1 +. cl.marginY2)
            };
        | None =>
            let parentAspect = paddedWidth /. paddedHeight;
            if (ratio < parentAspect) {
                /* Limit by height */
                let width = paddedHeight *. ratio;
                (width, paddedHeight)
            } else {
                /* Limit by width */
                let height = paddedWidth /. ratio;
                (paddedWidth, height)
            };
        };
    }
    | Dimensions(dimX, dimY) =>
        /* Get in pixel or ratio form */
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
    | WidthRatio(dimX, ratio) =>
        let width = switch (dimX) {
        | Pixel(pixels) => pixels
        | Scale(scale) => paddedWidth *. scale
        };
        (
            width,
            width /. ratio
        )
    | HeightRatio(dimY, ratio) =>
        let height = switch (dimY) {
        | Pixel(pixels) => pixels
        | Scale(scale) => paddedHeight *. scale
        };
        (
            height *. ratio,
            height
        )
    };
    cl.pWidth = nodeWidth;
    cl.pHeight = nodeHeight;
    cl.inWidth = cl.pWidth -. cl.marginX1 -. cl.marginX2;
    cl.inHeight = cl.pHeight -. cl.marginY1 -. cl.marginY2;
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
                let cl = child.calcLayout;
                cl.pXOffset = x +. cl.marginX1;
                cl.pYOffset = y +. cl.marginY1;
            }, node.children);
        | Horizontal =>
            /* Use ratio as scale. Not sure if it makes total sense,
               possibly restrict to width, height dimensions */
            let spacing = switch (layout.spacing) {
            | Some(Pixel(pixel)) => pixel
            | Some(Scale(scale)) => paddedWidth *. scale
            | None => 0.0
            };
            /* Set xoffset */
            switch (layout.hAlign) {
            | AlignLeft =>
                let _ = List.fold_left((xOffset, child) => {
                    child.calcLayout.pXOffset = xOffset +. child.calcLayout.marginX1;
                    xOffset +. spacing +. child.calcLayout.pWidth
                }, x, node.children);
            | AlignCenter =>
                /* Get total width and start from (paddedWidth  - totalWidth) / 2) */
                let totalWidth = List.fold_left((totalWidth, child) => {
                    totalWidth +. child.calcLayout.pWidth
                }, 0.0, node.children) +. spacing *. float_of_int(List.length(node.children) - 1);
                let xOffset = (paddedWidth -. totalWidth) /. 2.0;
                let _ = List.fold_left((xOffset, child) => {
                    child.calcLayout.pXOffset = xOffset +. child.calcLayout.marginX1;
                    xOffset +. spacing +. child.calcLayout.pWidth
                }, x +. xOffset, node.children);
            | AlignRight =>
                let _ = List.fold_right((child, xOffset) => {
                    let childOffset = xOffset -. child.calcLayout.pWidth;
                    child.calcLayout.pXOffset = childOffset +. child.calcLayout.marginX1;
                    childOffset -. spacing
                }, node.children, x +. paddedWidth);
            };
            switch (layout.vAlign) {
            | AlignTop =>
                List.iter((child) => {
                    child.calcLayout.pYOffset = y +. child.calcLayout.marginY1;
                }, node.children);
            | AlignMiddle =>
                List.iter((child) => {
                    child.calcLayout.pYOffset = y +. ((paddedHeight -. child.calcLayout.pHeight) /. 2.0) +. child.calcLayout.marginY1;
                }, node.children);
            | AlignBottom =>
                List.iter((child) => {
                    child.calcLayout.pYOffset = y +. paddedHeight -. child.calcLayout.pHeight +. child.calcLayout.marginY1;
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
                    child.calcLayout.pXOffset = x +. child.calcLayout.marginX1;
                }, node.children);
            | AlignCenter =>
                List.iter((child) => {
                    child.calcLayout.pXOffset = x +. ((paddedWidth -. child.calcLayout.pWidth) /. 2.0) +. child.calcLayout.marginX1;
                }, node.children);
            | AlignRight =>
                List.iter((child) => {
                    child.calcLayout.pXOffset = x +. paddedWidth -. child.calcLayout.pWidth +. child.calcLayout.marginX1;
                }, node.children);
            };
            switch (layout.vAlign) {
            | AlignTop =>
                let _ = List.fold_left((yOffset, child) => {
                    child.calcLayout.pYOffset = yOffset +. child.calcLayout.marginY1;
                    yOffset +. child.calcLayout.pHeight +. spacing
                }, y, node.children);
            | AlignMiddle =>
                let totalHeight = List.fold_left((totalHeight, child) => {
                    totalHeight +. child.calcLayout.pHeight
                }, 0.0, node.children) +. spacing *. float_of_int(List.length(node.children) - 1);
                let _ = List.fold_left((yOffset, child) => {
                    child.calcLayout.pYOffset = yOffset +. child.calcLayout.marginY1;
                    yOffset +. child.calcLayout.pHeight +. spacing
                }, y +. (paddedHeight -. totalHeight) /. 2.0 ,node.children);
            | AlignBottom =>
                let _ = List.fold_right((child, yOffset) => {
                    let childOffset = yOffset -. child.calcLayout.pHeight;
                    child.calcLayout.pYOffset = childOffset +. child.calcLayout.marginY1;
                    childOffset -. spacing
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
            let scaleX = calcLayout.inWidth /. vpWidth;
            let scaleY = calcLayout.inHeight /. vpHeight;
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
                    ((calcLayout.pXOffset +. (calcLayout.inWidth /. 2.0) -. vpWidthCenter) /. vpWidth *. 2.0),
                    ((calcLayout.pYOffset +. (calcLayout.inHeight /. 2.0) -. vpHeightMiddle) /. vpHeight *. -2.0)
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
                node.calcLayout.inWidth,
                node.calcLayout.inHeight
            ));
        };
        List.iter((dep) => {
            calcNodeLayout(dep);
        }, node.deps);
        List.iter((child) => {
            calcNodeLayout(child);
        }, node.children);
        /* After deps and children have been processed, set texture matrices */
        Hashtbl.iter((_name, nodeTex : nodeTex('state, 'flags)) => {
            /* Textures should be scaled for right pixel ratio,
               need not be translated (could be packed maybe),
               but should start from beginning or pack position */
            /* Understand this as texture size goes to 1.0 on viewport size
               so it will depend on setting viewport before
               render to texture */
            switch (nodeTex.node, nodeTex.uniformMat) {
            | (Some(texNode), Some(uniform)) =>
                let scaleX = texNode.calcLayout.pWidth /. vpWidth /. 2.0;
                let scaleY = texNode.calcLayout.pHeight /. vpHeight /. 2.0;
                let scale = Data.Mat3.scale(
                    scaleX,
                    scaleY
                );
                let translate = Data.Mat3.trans(scaleX, 1.0 -. scaleY);
                let texMat = Data.Mat3.matmul(translate, scale);
                Gpu.Uniform.setMat3f(uniform, texMat);
            | _ => ()
            };
        }, node.textures);
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

let debugNodes = [];

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
            if (node.clearOnDraw) {
                Gpu.Canvas.clear(self.canvas, 0.0, 0.0, 0.0);
            };
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
            if (List.exists((debug) => node.key == debug, debugNodes)) {
                Gpu.DrawState.draw(drawState, self.canvas);
                Gpu.Canvas.clearFramebuffer(self.canvas);
                Gpu.DrawState.draw(drawState, self.canvas);
                [%debugger];
            } else {
                Gpu.DrawState.draw(drawState, self.canvas);
            };
        };
        if (drawToTexture) {
            Gpu.Canvas.clearFramebuffer(self.canvas);
        };
    | None => failwith("Drawstate not found")
    };
};

let update = (self, updateFlags) => {
    /* Anims */
    let animIds = List.sort((a, b) => (a < b) ? -1 : 1, List.map((anim : anim('state, 'flags)) => anim.node.id, self.anims));
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
    /* todo: is this ok to sort variant types?
       use some hashset type? */
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
    let vals = (a, b) => Gpu.UniformVec2f(ref(Data.Vec2.make(a, b)));
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