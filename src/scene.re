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
    drawLists: Hashtbl.t(updateState('flags), list(drawListItem('state, 'flags))),
    mutable loadingNodes: list((list('flags), node('state, 'flags))),
    mutable anims: list(anim('state, 'flags)),
    fbuffer: Hashtbl.t(fbufferConfig, Gpu.FrameBuffer.inited),
    stencilDraw: StencilDraw.t,
    /* List of nodes that needs update on flags */
    onFlags: Hashtbl.t('flags, list(updateListNode('state, 'flags))),
    /* Update nodes keyed by node id/number,
       array is sized to cover all nodes */
    updateNodes: ArrayB.t(option(updateListNode('state, 'flags))),
    mutable updateRoot: option(updateListNode('state, 'flags))
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
    onUpdate: option((node('state, 'flags), 'state, list('flags)) => unit),
    layout: layout,
    calcLayout: calcLayout,
    /* Rect in pixels */
    rect: Shapes.Rect.t,
    scissorRect: Shapes.Rect.t,
    /* Rect in screen -1.0 to 1.0 */
    screenRect: Shapes.Rect.t,
    selfDraw: bool,
    transparent: bool,
    partialDraw: bool,
    hidable: bool,
    mutable hidden: bool,
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
    elapsedUniform: option(Gpu.uniform),
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
    animNode: node('state, 'flags),
    onFrame: (t('state, 'flags), node('state, 'flags), anim('state, 'flags)) => unit,
    duration: float,
    mutable elapsed: float,
    next: option(anim('state, 'flags))
}
and nodeTex('state, 'flags) = {
    texture: Gpu.Texture.t,
    texNode: option(node('state, 'flags)),
    uniformMat: option(Gpu.uniform),
    offsetX: dimension,
    offsetY: dimension
}
and updateStencil = {
    mutable rect: Shapes.Rect.t,
    mutable active: bool
}
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
and updateListNode('state, 'flags) = {
    /* Whether there is a full update triggered
       on the node, currently through flag or animation */
    mutable update: bool,
    /* Whether any child is registered for update,
       to help traversal */
    mutable childUpdate: bool,
    /* Whether this is part of a dependency node */
    isDep: bool,
    /* Whether this is a node in the deps list */
    isDepRoot: bool,
    /* Reference to the node */
    updNode: node('state, 'flags),
    /* Rectangle of the node, this is referenced in
       updateRects and stencils, and the data may
       be updated on resize
       todo: Not sure whether this should be option or
       not, maybe not. If checks are maybe a little more
       jit */
    mutable rect: option(Shapes.Rect.t),
    /* Stencil rects/(shapes) from children.
       A childs stencil is referenced and flagged as active
       as the child is marked for update */
    mutable stencils: list(updateStencil),
    /* When visible, not transparent and not partialDraw, this is
       stencil rect that can be used by parents.
       This is referenced upwards in the tree
       so when toggling active, it will
       propagate */
    stencil: option(updateStencil),
    /* Rectangles coming from child drawings,
       here is a list of possible child rects,
       where the ones needed should be activated,
       then on traversal all active rects
       should be reconciled */
    mutable childRects: list(updateRect('state, 'flags)),
    /* Rects upwards in the tree that are needed
       to redraw this node, the list will be
       expanded as needed */
    mutable parentRects: list(updateRect('state, 'flags)),
    mutable updDeps: list(updateListNode('state, 'flags)),
    mutable updChildren: list(updateListNode('state, 'flags)),
    parent: option(updateListNode('state, 'flags))
}
/* Update rect that will be listen on the node
   that needs these rects to be drawn below. */
and updateRect('state, 'flags) = {
    /* Rectangle mutable for resize,
       maybe this is not sufficient if nodes
       will be responsively rearranged */
    rect: Shapes.Rect.t,
    /* Rect with scissor coords */
    scisRect: Shapes.Rect.t,
    /* Node that may draw this rect, it will
       be checked for hidden and update state
       when setting active rects */
    rNode: updateListNode('state, 'flags),
    /* List of stencils that are among child nodes,
       they will have active state determined by
       whether nodes are visible */
    stencils: list(updateStencil),
    /* Whether this rect covers the node it is drawing
       for. In which case, when node is visible, it is suffucient
       to stop at the current position in the list. */
    covers: bool,
    /* Whether the child requiring this rect needs to be drawn,
       also includes check whether this node is to be drawn */
    mutable active: bool
}
/* It would be nice to directly update these on resize etc */
and stencilBuffers = {
    mutable stencilVertices: Gpu.VertexBuffer.inited,
    mutable stencilIndices: Gpu.IndexBuffer.inited,
}
and  drawListItem('state, 'flags) =
  | DrawNode(node('state, 'flags))
  | SetRect(Shapes.Rect.t)
  | ClearRect
  | DrawStencil(stencilBuffers)
  | ClearStencil;

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
            texNode: Some(node),
            uniformMat: Some(UniformMat3f(ref(Data.Mat3.id()))),
            offsetX,
            offsetY
        }
    };

    let tex = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), texture) => {
        {
            texture,
            texNode: None,
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
    ~elapsedUniform=false,
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
    ~partialDraw=false,
    ~hidden=false,
    ~hidable=false,
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
    let elapsedUniform = if (elapsedUniform) {
        Some(Gpu.UniformFloat(ref(0.0)))
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
        onUpdate: update,
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
        rect: Shapes.Rect.zeros(),
        scissorRect: Shapes.Rect.zeros(),
        screenRect: Shapes.Rect.zeros(),
        selfDraw,
        loading,
        transparent,
        partialDraw,
        hidden,
        hidable: (hidden || hidable),
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
        elapsedUniform,
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
    drawLists: Hashtbl.create(5),
    loadingNodes: [],
    anims: [],
    fbuffer: Hashtbl.create(1),
    stencilDraw: StencilDraw.make(canvas),
    onFlags: Hashtbl.create(50),
    updateNodes: ArrayB.make(None),
    updateRoot: None
}
};

let makeAnim = (node, onFrame, duration, ~next=?, ()) => {
{
    animNode: node,
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
let createUpdateTree = (scene, root) => {
    let rec loop = (node, parent, isDep, isDepRoot, stacked) => {
        let stencil = if (node.transparent || node.partialDraw) {
            None
        } else {
            /* Translate to screen coordinates */
            let updStencil = {
                rect: node.screenRect,
                active: false,
            };
            /* There may not be much point to
               add stencil when isDepRoot */
            if (!isDepRoot) {
                /* Add reference to parent */
                switch (parent) {
                | Some(parent) => parent.stencils = [updStencil, ...parent.stencils];
                | None => ();
                };
            };
            Some(updStencil)
        };
        /* Possibly, this could be created after
           collecting deps and children, in which
           case parent field would need to be
           filled in. Not sure if the difference
           is very big, see if there is some use
           to getting the parent handy first */
        let updateNode = {
            update: false,
            childUpdate: false,
            isDep,
            isDepRoot,
            updNode : node,
            rect: None,
            stencils: [],
            stencil,
            childRects: [],
            parentRects: [],
            updDeps: [],
            updChildren: [],
            parent: parent
        };
        /* Add to indexes for node id/number and flag */
        scene.updateNodes.data[node.id] = Some(updateNode);
        List.iter((flag) => {
            if (!Hashtbl.mem(scene.onFlags, flag)) {
                Hashtbl.add(scene.onFlags, flag, [updateNode]);
            } else {
                let nodes = Hashtbl.find(scene.onFlags, flag);
                Hashtbl.replace(scene.onFlags, flag, [updateNode, ...nodes]);
            };
        }, node.updateOn);
        updateNode.updDeps = List.map((dep) => loop(dep, Some(updateNode), true, true, []), node.deps);
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
                | [first, ...rest] => updateNode.updChildren = [loop(first, Some(updateNode), isDep, false, rest)];
                };
            | [first, ...rest] =>
                /* Append stacked from parent after this new list
                   of stacked children */
                updateNode.updChildren = [loop(first, Some(updateNode), isDep, false, List.append(rest, stacked))]
            };
        | _ =>
            switch (stacked) {
            | [] =>
                /* Normal processing of children */
                updateNode.updChildren = List.map((child) => loop(child, Some(updateNode), isDep, false, []), node.children);
            | [first, ...rest] =>
                /* We are in middle of a stacked layout, collect children,
                   and keep next stacked item as last child */
                updateNode.updChildren = List.rev(List.fold_left((list, child) => {
                    [loop(child, Some(updateNode), isDep, false, []), ...list]
                }, [loop(first, Some(updateNode), isDep, false, rest)], node.children));
            };
        };
        /* All children have added their stencils,
           propagate them to parent */
        if (!updateNode.isDepRoot) {
            switch (updateNode.parent) {
            | Some(parent) =>
                List.iter((updStencil) => {
                    parent.stencils = [updStencil, ...parent.stencils];
                }, updateNode.stencils);
            | None => ();
            };
        };
        updateNode
    };
    ArrayB.ensureSize(scene.updateNodes, currNodeId^);
    loop(root, None, false, false, [])
};

let rec setChildToUpdate = (updNode) => {
    updNode.update = true;
    List.iter((updChild) => {
        if (!updChild.update) {
            setChildToUpdate(updChild);
        };
    }, updNode.updChildren);
};

let rec setDepParentToUpdate = (updNode) => {
    /* todo: more fine grained update
       when coming from deps. Currently
       just turning on update
       There should be a rect propagated up
       to node with this dependency, then
       to it's children */
    updNode.update = true;
    /* This might be uneccesary if we do more
       fine grained update */
    List.iter((updChild) => {
        if (!updChild.update) {
            setChildToUpdate(updChild);
        };
    }, updNode.updChildren);
    switch (updNode.parent) {
    | Some(parent) =>
        if (parent.isDep) {
            if (!parent.update) {
                setDepParentToUpdate(parent);
            };
        } else {
            /* Propagate to regular update of node that
               has this as dependency */
            if (!parent.update) {
                setToUpdate(parent);
            };
        };
    | None => ();
    };
}
and setToUpdate = (updNode) => {
    updNode.update = true;
    List.iter((updChild) => {
        if (!updChild.update) {
            setChildToUpdate(updChild);
        };
    }, updNode.updChildren);
    if (updNode.isDep) {
        switch (updNode.parent) {
        | Some(parent) =>
            if (!parent.update) {
                if (parent.isDep) {
                    setDepParentToUpdate(parent);
                } else {
                    setToUpdate(parent);
                };
            };
        | None => failwith("Dependency without parent");
        };
    };
    /* Mark upwards in the tree that a child
        needs update */
    let rec markChildUpdate = (updNode) => {
        updNode.childUpdate = true;
        switch (updNode.parent) {
        | Some(parent) when (parent.childUpdate == false) => markChildUpdate(parent);
        | _ => ();
        };
    };
    switch (updNode.parent) {
    | Some(parent) when (parent.childUpdate == false) => markChildUpdate(parent);
    | _ => ();
    };
};

/* Traverses parents to ensure nodes that
   are set to update, and are transparent
   or partialDraw, are covered by a parent */
let actRectUntilCovered = (updNode) => {
    /* Repeating some checks a few places here
       percievely in the interest of a little
       performance as well as slightly
       different cases */

    let actStencil = (updNode) => {
        /* Stencil should be present
           when node is not transparent
           or partialDraw */
        /* Todo:(!) If we had rendered from
           child upwards when not needing
           to draw transparent nodes,
           the stencil mask could
           come from those renders, and the
           stencil mask could be increased as
           more is drawn, should be a nice
           optimization */
        switch (updNode.stencil) {
        | Some(stencil) => stencil.active = true;
        | None => ()
        };
    };

    /* This creates new updRects for unexplored
       parents, sets their active status and return
       a list of newly created updateRects until
       a covering node is found */
    let rec loopNewRects = (parent) => {
        switch (parent) {
        | Some(parent) =>
            if (!parent.updNode.selfDraw) {
                /* Skipping layout nodes now,
                   possibly there could be use of
                   having rects there if they propagate
                   to their children */
                loopNewRects(parent.parent)
            } else {
                let covers = (
                    !(parent.updNode.transparent || parent.updNode.partialDraw)
                    && Shapes.Rect.contains(parent.updNode.rect, updNode.updNode.rect)
                );
                let updRect = {
                    rect: updNode.updNode.rect,
                    scisRect: updNode.updNode.scissorRect,
                    rNode: parent,
                    stencils: [],
                    covers,
                    active: !(parent.updNode.hidden || parent.update)
                };
                parent.childRects = [updRect, ...parent.childRects];
                /* For normal childs they are hidden when parent
                   are hidden, but stacked layouts are
                   rearranged so a parent may be hidden while
                   a child is not */
                if (parent.updNode.hidden) {
                    /* Pass on rect for possible later use, but it will not be active */
                    [updRect, ...loopNewRects(updRect.rNode.parent)]
                } else if (parent.update) {
                    if (updRect.covers) {
                        /* Update and covers, done */
                        [updRect]
                    } else {
                        actStencil(updRect.rNode);
                        [updRect, ...loopNewRects(updRect.rNode.parent)]
                    }
                } else {
                    if (updRect.covers) {
                        [updRect]
                    } else {
                        actStencil(updRect.rNode);
                        [updRect, ...loopNewRects(updRect.rNode.parent)];
                    }
                }
            }
        | None => failwith("Could not find covering parent for: " ++ updNode.updNode.key
                           ++ ". Maybe add a background node?");
        }
    };
    let rec loopRects = (rects : list(updateRect('state, 'flags))) => {
        switch (rects) {
        | [updRect] =>
            /* Check if we need to continue
               or have found a covering node */
            if (updRect.rNode.updNode.hidden) {
                updNode.parentRects = List.append(
                    updNode.parentRects,
                    loopNewRects(updRect.rNode.parent)
                );
            } else if (updRect.rNode.update) {
                if (!updRect.covers) {
                    actStencil(updRect.rNode);
                    updNode.parentRects = List.append(
                        updNode.parentRects,
                        loopNewRects(updRect.rNode.parent)
                    );
                };
            } else {
                updRect.active = true;
                if (!updRect.covers) {
                    actStencil(updRect.rNode);
                    updNode.parentRects = List.append(
                        updNode.parentRects,
                        loopNewRects(updRect.rNode.parent)
                    );
                }
            };
        | [updRect, ...rest] =>
            if (updRect.rNode.updNode.hidden) {
                loopRects(rest);
            } else if (updRect.rNode.update) {
                if (!updRect.covers) {
                    actStencil(updRect.rNode);
                    loopRects(rest);
                };
            } else {
                updRect.active = true;
                if (!updRect.covers) {
                    actStencil(updRect.rNode);
                    loopRects(rest);
                }
            };
        | [] =>
            /* This should only be case for initial empty list,
               or actual empty list for rootish node */
            updNode.parentRects = loopNewRects(updNode.parent);
        };
    };
    loopRects(updNode.parentRects);
};

let createDrawList = (scene, flags, animIds, root) => {
    /* Set nodes to update */
    let updNodes = List.fold_left((updNodes, flag) => {
        if (Hashtbl.mem(scene.onFlags, flag)) {
            let toUpdate = Hashtbl.find(scene.onFlags, flag);
            List.iter((toUpdate) => {
                setToUpdate(toUpdate);
            }, toUpdate);
            [toUpdate, ...updNodes]
        } else {
            updNodes
        };
    }, [], flags);
    let updNodes = [List.fold_left((nodes, nodeId) => {
        switch (scene.updateNodes.data[nodeId]) {
        | Some(updNode) =>
            setToUpdate(updNode);
            [updNode, ...nodes]
        | None => failwith("Could not find update node with id: " ++ string_of_int(nodeId));
        };
    }, [], animIds), ...updNodes];
    /* Do second pass on nodes to update
       to check for transparent nodes and
       ensure they are covered by a parent */
    List.iter((nodes) => {
        List.iter((updNode) => {
            if (updNode.updNode.transparent || updNode.updNode.partialDraw) {
                actRectUntilCovered(updNode);
            };
        }, nodes);
    }, updNodes);
    /* State of stencils and rect to determine
       when to clear them */
    let activeStencils = ref(None);
    let activeRect = ref(None);
    let equalsActiveStencils = (stencils : list(updateStencil)) => {
        let rec checkStencils = (stencils : list(updateStencil), activeStencils : list(updateStencil)) => switch (stencils, activeStencils) {
        | ([], []) => true
        | ([st1, ...rest1], [st2, ...rest2]) => {
            if (Shapes.Rect.equals(st1.rect, st2.rect)) {
                false
            } else {
                checkStencils(rest1, rest2)
            }
        }
        | _ => false
        };
        switch (activeStencils^) {
        | None => false
        | Some(activeStencils) => checkStencils(stencils, activeStencils)
        }
    };
    let equalsActiveRect = (rect) => {
        switch (activeRect^) {
        | None => false
        | Some(activeRect) => Shapes.Rect.equals(activeRect, rect)
        }
    };
    /* Generate draw list
       The returned list will be reversed */
    let rec drawListLoop = (updNode, drawList) => {
        /* First collect from dependencies */
        let drawList = List.fold_left((drawList, dep) => drawListLoop(dep, drawList), drawList, updNode.updDeps);
        /* Check for selfDraw.
           Not sure where to branch on this,
           doing after deps now,
           especially if we render from child to parent
           when not transparent, it could make sense
           to update stencils */
        if (!updNode.updNode.selfDraw) {
            updNode.update = false; /* in case */
            /* Recurse to children if they have update or childUpdate flagged */
            let drawList = List.fold_left((drawList, updChild) => {
                if (updChild.update) {
                    drawListLoop(updChild, drawList)
                } else if (updChild.childUpdate) {
                    updChild.childUpdate = false;
                    drawListLoop(updChild, drawList)
                } else {
                    drawList
                }
            }, drawList, updNode.updChildren);
            drawList
        } else {
            /* todo: possibly some microoptimizations if either lists are empty */
            /* Check for active stencils */
            let stencils = List.filter((stencil : updateStencil) => stencil.active, updNode.stencils);
            /* Filter duplicate stencils/stencils contained by other stencils */
            let rec nonDupStencils = (list : list(updateStencil)) => switch (list) {
            | [] => []
            | [stencil, ...rest] =>
                if (List.exists((stencil2 : updateStencil) => Shapes.Rect.contains(stencil2.rect, stencil.rect), rest)) {
                    nonDupStencils(rest)
                } else {
                    [stencil, ...nonDupStencils(rest)]
                };
            };
            let stencils = nonDupStencils(stencils);
            /* Possibly a slight optimization
            would be to, in the case of one rect, draw
            stencil after rect. In the case of several rects,
            the stencils might take part in several of them
            and more logic would be needed.
            Or maybe better a bounding box over rects or node could
            be used to shape the stencils on the cpu */
            let drawList = switch (stencils) {
            | [] =>
                if (activeStencils^ == None) {
                    drawList
                } else {
                    activeStencils := None;
                    [ClearStencil, ...drawList]
                }
            | stencils =>
                if (activeStencils^ != None && equalsActiveStencils(stencils)) {
                    drawList
                } else {
                    /* Create buffers */
                    let data = Array.concat(List.map((stencil : updateStencil) => {
                        let rect = stencil.rect;
                        [|
                            rect.x, rect.y -. rect.h, /* Bottom left */
                            rect.x, rect.y, /* Top left */
                            rect.x +. rect.w, rect.y, /* Top right */
                            rect.x +. rect.w, rect.y -. rect.h /* Bottom right */
                        |]
                    }, stencils));
                    let vb = Gpu.VertexBuffer.makeQuad(
                        ~data,
                        ~usage=Gpu.DynamicDraw,
                        ()
                    );
                    let ib = Gpu.IndexBuffer.make(
                        Gpu.IndexBuffer.makeQuadsData(List.length(stencils)),
                        Gpu.DynamicDraw
                    );
                    let stencilBuffers = {
                        stencilVertices: Gpu.VertexBuffer.init(vb, scene.canvas.context, scene.stencilDraw.program.programRef),
                        stencilIndices: Gpu.IndexBuffer.init(ib, scene.canvas.context),
                    };
                    activeStencils := Some(stencils);
                    [DrawStencil(stencilBuffers), ...drawList]
                }
            };
            if (updNode.update) {
                /* Clean up update flag */
                updNode.update = false;
                /* Clear active rect if any */
                let drawList = if (activeRect^ != None) {
                    activeRect := None;
                    [ClearRect, ...drawList]
                } else {
                    drawList
                };
                let drawList = [DrawNode(updNode.updNode), ...drawList];
                /* Since this node is flagged for update, assume all children
                will also be updated */
                let drawList = List.fold_left((drawList, updChild) => {
                    drawListLoop(updChild, drawList)
                }, drawList, updNode.updChildren);
                /* Clean up active stencils */
                List.iter((stencil) => stencil.active = false, stencils);
                drawList
            } else {
                /* Check for active rects */
                let rects = List.filter((updRect : updateRect('state, 'flags)) => updRect.active, updNode.childRects);
                /* Filter duplicate/contained  */
                let rec nonDupRects = (list : list(updateRect('state, 'flags))) => switch (list) {
                | [] => []
                | [rect, ...rest] =>
                    /* !! Also negates active flag !! */
                    rect.active = false;
                    if (List.exists((rect2 : updateRect('state, 'flags)) => Shapes.Rect.contains(rect2.rect, rect.rect), rest)) {
                        nonDupRects(rest)
                    } else {
                        [rect, ...nonDupRects(rest)]
                    };
                };
                let rects = nonDupRects(rects);
                /* When there are multiple rects, another strategy would
                   be to set up a stencil buffer with all of them
                   and regular stencils subtracted.
                   Though I assume in some cases it may not be
                   that much better as a scissor rect should be easier
                   to deal with for the gpu? So it would be nice to
                   see benchmarks */
                let drawList = List.fold_left((drawList, rect: updateRect('a, 'b)) => {
                    if (activeRect^ == None) {
                        activeRect := Some(rect.scisRect);
                        [
                            DrawNode(updNode.updNode),
                            SetRect(rect.scisRect),
                            ...drawList
                        ]
                    } else if (equalsActiveRect(rect.scisRect)) {
                        [DrawNode(updNode.updNode)]
                    } else {
                        /* Clear and set new rect */
                        activeRect := Some(rect.scisRect);
                        [
                            DrawNode(updNode.updNode),
                            SetRect(rect.scisRect),
                            ClearRect,
                            ...drawList
                        ]
                    }
                }, drawList, rects);
                /* Recurse to children if they have update or childUpdate flagged */
                let drawList = List.fold_left((drawList, updChild) => {
                    if (updChild.update) {
                        drawListLoop(updChild, drawList)
                    } else if (updChild.childUpdate) {
                        updChild.childUpdate = false;
                        drawListLoop(updChild, drawList)
                    } else {
                        drawList
                    }
                }, drawList, updNode.updChildren);
                List.iter((stencil) => stencil.active = false, stencils);
                drawList
            }
        }
    };
    /* Not sure if we need to useProgram for stencil buffers */
    Reasongl.Gl.useProgram(~context=scene.canvas.context, scene.stencilDraw.program.programRef);
    switch (scene.updateNodes.data[root.id]) {
    | Some(updRoot) =>
        let drawList = drawListLoop(updRoot, []);
        /* Clear active rect if any */
        let drawList = if (activeRect^ != None) {
            [ClearRect, ...drawList]
        } else {
            drawList
        };
        /* Clear active stencils if any */
        let drawList = if (activeStencils^ != None) {
            [ClearStencil, ...drawList]
        } else {
            drawList
        };
        List.rev(drawList)
    | None => failwith("Root update node not found");
    }
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
        switch (node.elapsedUniform) {
        | Some(elapsedUniform) => Gpu.Uniform.setFloat(elapsedUniform, self.canvas.elapsed);
        | None => ()
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

module Gl = Reasongl.Gl;

let processDrawList = (scene, drawList, flags) => {
    let context = scene.canvas.context;
    open Gpu;
    module Gl = Reasongl.Gl;
    let rec processDrawEl = (list) => switch (list) {
    | [el, ...rest] =>
        switch (el) {
        | DrawNode(node) =>
            if (node.loading) {
                if (!List.exists(((_flags, loading)) => loading === node, scene.loadingNodes)) {
                    /* todo: flags.. are they needed? */
                    scene.loadingNodes = [(flags, node), ...scene.loadingNodes];
                };
            } else {
                switch (node.onUpdate) {
                | Some(update) => update(node, scene.state, flags);
                | None =>
                    draw(scene, node);
                };
            };
        | DrawStencil(stencilBuffers) =>
            Gl.enable(~context, Stencil.stencilTest);
            Canvas.clearStencil(scene.canvas);
            /* Always write 1s */
            Stencil.stencilFunc(context, Stencil.always, 1, 0xFF);
            Stencil.stencilOp(context, Stencil.keep, Stencil.keep, Stencil.replace);
            Stencil.stencilMask(context, 0xFF);
            /* Disable color and depth writing when writing
               stencil rects */
            Canvas.colorMask(context, false, false, false, false);
            Canvas.depthMask(context, false);
            StencilDraw.draw(scene.stencilDraw, stencilBuffers.stencilVertices, stencilBuffers.stencilIndices);
            Canvas.colorMask(context, true, true, true, true);
            Canvas.depthMask(context, true);
            /* Set stencil test function to check for 1's
               when drawing nodes that should be affected
               by the stencil */
            Stencil.stencilFunc(context, Stencil.notEqual, 1, 0xFF);
            /* We can also disable stencil writing */
            Stencil.stencilMask(context, 0x00);
        | ClearStencil =>
            Gl.disable(~context, Stencil.stencilTest);
        | SetRect(rect) =>
            Gl.enable(~context, Scissor.scissorTest);
            /* todo: Int rect */
            Scissor.scissor(
                context,
                int_of_float(rect.x),
                int_of_float(rect.y),
                int_of_float(rect.w),
                int_of_float(rect.h)
            );
        | ClearRect =>
            Gl.disable(~context, Scissor.scissorTest);
        };
        processDrawEl(rest);
    | [] => ()
    };
    processDrawEl(drawList);
};

let logDrawList = (scene, updateState, drawList) => {
    Js.log2("Drawlist", updateState.flags);
    List.iter((animId) => {
        let n = scene.updateNodes.data[animId];
        let key = switch (n) {
        | Some(n) => n.updNode.key
        | None => "No key"
        };
        Js.log2("Anim: ", key);
    }, updateState.anims);
    let rec processDrawEl = (list) => switch (list) {
    | [el, ...rest] =>
        switch (el) {
        | DrawNode(node) =>
            Js.log("Draw node: " ++ node.key);
        | DrawStencil(stencilBuffers) =>
            Js.log2("Draw stencil: ", stencilBuffers.stencilVertices.data);
        | ClearStencil =>
            Js.log("Clear stencil");
        | SetRect(rect) =>
            Js.log2("Set rect: ", rect);
        | ClearRect =>
            Js.log("Clear rect");
        };
        processDrawEl(rest);
    | [] => ()
    };
    processDrawEl(drawList);
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
/* Elapsed uniform */
let uniforms = switch (node.elapsedUniform) {
| Some(elapsedUniform) => [Gpu.Uniform.make("elapsedScene", elapsedUniform), ...uniforms]
| None => uniforms
};
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
        node.rect.x = calcLayout.pXOffset;
        node.rect.y = calcLayout.pYOffset;
        node.rect.w = calcLayout.inWidth;
        node.rect.h = calcLayout.inHeight;
        node.scissorRect.x = floor(calcLayout.pXOffset);
        node.scissorRect.y = floor(vpHeight -. calcLayout.pYOffset -. calcLayout.inHeight);
        node.scissorRect.w = ceil(calcLayout.inWidth +. (calcLayout.pXOffset -. floor(calcLayout.pXOffset))); /* Couldnt get modf to work */
        node.scissorRect.h = ceil(calcLayout.inHeight +. (calcLayout.pYOffset -. floor(calcLayout.pYOffset)));
        /* Bit torn on whether to do this for all nodes,
           currently used for stencils, possibly there
           are other uses, but generally layout matrix is also available.
           We have vpWidth available, if calculations
           are moved, vpWidth etc might beneficially
           be stored. */
        node.screenRect.x = (calcLayout.pXOffset /. vpWidth *. 2.0) -. 1.0;
        node.screenRect.y = 1.0 -. (calcLayout.pYOffset /. vpHeight *. 2.0);
        node.screenRect.w = calcLayout.inWidth /. vpWidth *. 2.0;
        node.screenRect.h = calcLayout.inHeight /. vpHeight *. 2.0;
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
            switch (nodeTex.texNode, nodeTex.uniformMat) {
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

let update2 = (self, updateFlags) => {
    /* Anims */
    let animIds = List.sort((a, b) => (a < b) ? -1 : 1, List.map((anim : anim('state, 'flags)) => anim.animNode.id, self.anims));
    let rec doAnims = (anims) => {
        switch (anims) {
        | [] => []
        | [anim, ...rest] =>
            anim.elapsed = anim.elapsed +. self.canvas.deltaTime;
            anim.onFrame(self, anim.animNode, anim);
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
                switch (node.onUpdate) {
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
            switch (node.onUpdate) {
            | Some(update) => update(node, self.state, sortedFlags)
            | None => draw(self, node)
            };
        };
    }, Hashtbl.find(self.updateLists, updateState));
};

let update = (self, updateFlags) => {
    /* Anims */
    let animIds = List.sort((a, b) => (a < b) ? -1 : 1, List.map((anim : anim('state, 'flags)) => anim.animNode.id, self.anims));
    let rec doAnims = (anims) => {
        switch (anims) {
        | [] => []
        | [anim, ...rest] =>
            anim.elapsed = anim.elapsed +. self.canvas.deltaTime;
            anim.onFrame(self, anim.animNode, anim);
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
    if (!Hashtbl.mem(self.drawLists, updateState)) {
        Hashtbl.add(self.drawLists, updateState, createDrawList(self, sortedFlags, animIds, self.root));
        logDrawList(self, updateState, Hashtbl.find(self.drawLists, updateState));
    };
    /* Check if any node in loadingNodes is loaded */
    let (newList, loaded) = List.partition((loading) => {
        let (_updateList, node) = loading;
        node.loading
    }, self.loadingNodes);
    self.loadingNodes = newList;
    if (List.length(loaded) > 0) {
        let loadedIds = List.map((loaded) => {
            let (_updateList, node) = loaded;
            switch (node.onUpdate) {
            | Some(update) => update(node, self.state, sortedFlags) 
            | None => ()
            };
            node.id
        }, loaded);
        let drawList = createDrawList(self, [], loadedIds, self.root);
        /* Drawing loaded nodes in a special drawlist
           The loaded node will redraw if they are
           in the main drawlist anyway, todo */
        processDrawList(self, drawList, sortedFlags);
    };
    /* todo: possibly optimize with a second transformed data structure
       so the drawstate etc is readily available */
    processDrawList(self, Hashtbl.find(self.drawLists, updateState), sortedFlags);
};

let cleanUpDrawList = (scene, drawList) => {
    let rec loop = (list) => {
        switch (list) {
        | [el, ...rest] =>
            switch (el) {
            | DrawNode(_node) => ()
            | DrawStencil(stencilBuffers) =>
                /* Clear gpu memory */
                Gpu.VertexBuffer.deleteBuffer(scene.canvas.context, stencilBuffers.stencilVertices);
                Gpu.IndexBuffer.deleteBuffer(scene.canvas.context, stencilBuffers.stencilIndices);
            | ClearStencil => ()
            | SetRect(_rect) => ()
            | ClearRect => ()
            };
            loop(rest);
        | [] => ()
        };
    };
    loop(drawList);
};

let doResize = (scene) => {
    let (width, height) = Gpu.Canvas.getViewportSize();
    /* Todo: There is some bugs here, maybe we need some time after resize
        before we should redraw background */
    Gpu.Canvas.resize(scene.canvas, width, height);
    /* Although rects are updated through calcLayout, we
       simply reset the drawList cache for some time.
       Stencil vertices would need to be updated/reset,
       otherwise it would work I think to keep cache */
    Hashtbl.iter((_, drawList) => {
        cleanUpDrawList(scene, drawList);
    }, scene.drawLists);
    Hashtbl.clear(scene.drawLists);
    calcLayout(scene);
    update(scene, [scene.initFlag]);
};

let run = (width, height, setup, createScene, draw, ~keyPressed=?, ~resize=?, ()) => {
    let canvas = Gpu.Canvas.init(width, height);
    let userState = ref(setup(canvas));
    let scene = createScene(canvas, userState^);
    setNodeParentsAndScene(scene);
    calcLayout(scene);
    createDrawStates(scene);
    /* Time for resize requested, this is throttled */
    let resizeRequested = ref(None);
    let resizeThrottle = 0.5;
    /* Start render loop */
    Gl.render(
        ~window = canvas.window,
        ~displayFunc = (f) => {
            canvas.deltaTime = f /. 1000.;
            canvas.elapsed = canvas.elapsed +. canvas.deltaTime;
            if (!scene.inited) {
                /* Create updateNodes */
                scene.updateRoot = Some(createUpdateTree(scene, scene.root));
                update(scene, [scene.initFlag]);
                scene.inited = true;
            };
            userState := draw(userState^, scene, canvas);
            switch (resizeRequested^) {
            | None => ()
            | Some(resizeTime) =>
                if (resizeTime > canvas.elapsed -. resizeThrottle) {
                    resizeRequested := None;
                    doResize(scene);
                };
            };
        },
        ~windowResize = () => {
            resizeRequested := Some(canvas.elapsed);
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