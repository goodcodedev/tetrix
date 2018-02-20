/* Scene nodes maps roughly to "draw states", a program draw with
   uniforms and textures.
   They also provide layout capabilities.

   The nodes will have some layout uniforms available.
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
  | Linear
  | SineOut
  | SineIn
  | SineInOut;

let currNodeId = ref(0);

/* Not thread safe :) */
let nextNodeId = () => {
  let nodeId = currNodeId^;
  currNodeId := nodeId + 1;
  nodeId;
};

type fbufferConfig = {
  width: int,
  height: int
};

/* user defined state and update flags */
type t('s) = {
  state: 's,
  mutable queuedUpdates: list(int),
  nodesByKey: Hashtbl.t(string, node('s)),
  nodesByCls: Hashtbl.t(string, list(node('s))),
  canvas: Gpu.Canvas.t,
  root: node('s),
  mutable inited: bool,
  drawLists: Hashtbl.t(updateState, list(drawListItem('s))),
  drawListsDebug: option(drawListDebug('s)),
  initedLists: Hashtbl.t(updateState, bool),
  initedDeps: Hashtbl.t(int, bool),
  mutable loadingNodes: list(node('s)),
  mutable anims: list(anim('s)),
  fbuffer: Hashtbl.t(fbufferConfig, Gpu.FrameBuffer.inited),
  stencilDraw: StencilDraw.t,
  /* Update nodes keyed by node id/number,
     array is sized to cover all nodes */
  updateNodes: ArrayB.t(option(updateListNode('s))),
  mutable updateRoot: option(updateListNode('s)),
  mutable hiddenNodes: list(int)
}
and drawListDebug('s) = {draw: DrawListDebug.t}
and updateState = {
  updateNodes: list(int),
  hiddenNodes: list(int)
}
and node('s) = {
  key: option(string),
  cls: option(string),
  id: int,
  mutable drawState: option(Gpu.DrawState.t),
  onUpdate: option([@bs] ((node('s), 's) => unit)),
  layout,
  calcLayout,
  /* Rect in pixels */
  rect: Geom2d.Rect.t,
  scissorRect: Geom2d.Rect.t,
  /* Rect in screen -1.0 to 1.0 */
  screenRect: Geom2d.Rect.t,
  selfDraw: bool,
  transparent: bool,
  partialDraw: bool,
  mutable hidden: bool,
  mutable loading: bool,
  deps: list(node('s)),
  children: list(node('s)),
  vertShader: option(Gpu.Shader.t),
  fragShader: option(Gpu.Shader.t),
  textureList: list(string),
  textures: Hashtbl.t(string, sceneTexture),
  vo: sceneVertexObject,
  uniformList: list(string),
  uniforms: Hashtbl.t(string, sceneUniform),
  layoutUniform: option(Gpu.uniform),
  pixelSizeUniform: option(Gpu.uniform),
  elapsedUniform: option(Gpu.uniform),
  drawToTexture: option(Gpu.Texture.t),
  texTransUniform: option(Gpu.uniform),
  clearOnDraw: bool,
  mutable parent: option(node('s)),
  mutable scene: option(t('s))
}
/* Also consider assigning id's to these
   objects, then connecting id's to nodes */
and sceneUniform = {
  mutable nodes: list(int),
  uniform: Gpu.uniform
}
and sceneVertexObject = {
  mutable nodes: list(int),
  vertexBuffer: Gpu.VertexBuffer.t,
  indexBuffer: option(Gpu.IndexBuffer.t)
}
and sceneTexture = {
  mutable nodes: list(int),
  texture: Gpu.Texture.t,
  texNode: option(int),
  uniformMat: option(Gpu.uniform),
  offsetX: dimension,
  offsetY: dimension
}
and layout = {
  size: blockSize,
  padding: option(dimension),
  margin: option(margin),
  childLayout,
  spacing: option(dimension),
  hAlign,
  vAlign
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
and anim('s) = {
  animKey: option(string),
  onFrame: [@bs] ((t('s), anim('s)) => unit),
  setLast: [@bs] (t('s) => unit),
  duration: float,
  mutable elapsed: float,
  frameInterval: int,
  mutable numFrames: int,
  next: option(anim('s))
}
and updateStencil = {
  mutable rect: Geom2d.Rect.t,
  mutable active: bool
}
and updateListNode('s) = {
  /* Whether there is a full update triggered
     on the node */
  mutable update: bool,
  /* Whether any child is registered for update,
     to help traversal */
  mutable childUpdate: bool,
  /* Whether this is part of a dependency node */
  isDep: bool,
  /* Whether this is a node directly in the deps list aka dep root */
  isDepRoot: bool,
  /* If node is part of a stacked layout, previous stacked nodes */
  prevStacked: list(updateListNode('s)),
  /* Partitioned to prev and next stacked */
  mutable nextStacked: list(updateListNode('s)),
  /* Reference to the node */
  updNode: node('s),
  /* Rectangle of the node, this is referenced in
     updateRects and stencils, and the data may possibly
     be updated on resize
     todo: Not sure whether this should be option or
     not, maybe not. If checks are maybe a little more
     jit */
  mutable rect: option(Geom2d.Rect.t),
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
  mutable childRects: list(updateRect('s)),
  /* Rects upwards in the tree that are needed
     to redraw this node, the list will be
     expanded as needed */
  mutable parentRects: list(updateRect('s)),
  mutable updDeps: list(updateListNode('s)),
  mutable updChildren: list(updateListNode('s)),
  parent: option(updateListNode('s))
}
/* Update rect that will be listen on the node
   that needs these rects to be drawn below. */
and updateRect('s) = {
  /* Rectangle mutable for resize,
     maybe this is not sufficient if nodes
     will be responsively rearranged */
  rect: Geom2d.Rect.t,
  /* Rect with scissor coords */
  scisRect: Geom2d.Rect.t,
  /* Node that may draw this rect, it will
     be checked for hidden and update state
     when setting active rects */
  rNode: updateListNode('s),
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
  mutable stencilIndices: Gpu.IndexBuffer.inited
}
and drawListItem('s) =
  | DrawNode(node('s))
  | SetRect(Geom2d.Rect.t)
  | ClearRect
  | DrawStencil(stencilBuffers)
  | ClearStencil
  | BindDrawTexture(node('s))
  | UnBindDrawTexture;

let quadVertices = Gpu.VertexBuffer.makeQuad();

let quadIndices = Gpu.IndexBuffer.makeQuad();

let makeLayout =
    (
      ~size=Dimensions(Scale(1.0), Scale(1.0)),
      ~padding=?,
      ~margin=?,
      ~spacing=?,
      ~childLayout=Horizontal,
      ~hAlign=AlignCenter,
      ~vAlign=AlignTop,
      ()
    )
    : layout => {
  size,
  padding,
  margin,
  childLayout,
  spacing,
  hAlign,
  vAlign
};

module SceneVO = {
  let make = (vertexBuffer, indexBuffer) => {
    nodes: [],
    vertexBuffer,
    indexBuffer
  };
};

type drawTo =
  | Framebuffer
  | TextureRGBA
  | TextureRGBADim(int)
  | TextureRGB
  | TextureRGBDim(int)
  | TextureGreyscale /* This is not usable, havent gotten greyscale to work yet, errors on not renderable */
  | TextureItem(Gpu.Texture.t);

module SceneTex = {
  let node = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), node) => {
    let texture =
      switch node.drawToTexture {
      | Some(texture) => texture
      | None => failwith("Provided node does not draw to texture")
      };
    {
      nodes: [],
      texture,
      texNode: Some(node.id),
      uniformMat: Some(UniformMat3f(ref(Data.Mat3.id()))),
      offsetX,
      offsetY
    };
  };
  let tex = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), texture) => {
    nodes: [],
    texture,
    texNode: None,
    uniformMat: None,
    offsetX,
    offsetY
  };
};

let makeNode =
    (
      ~key=?,
      ~cls=?,
      ~vertShader=?,
      ~fragShader=?,
      ~update=?,
      ~children=[],
      ~textures: list((string, sceneTexture))=[],
      ~vo: option(sceneVertexObject)=?,
      ~uniforms: list((string, sceneUniform))=[],
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
      ~deps=[],
      ~drawTo=Framebuffer,
      ~texTransUniform=false,
      ~clearOnDraw=false,
      ()
    ) => {
  let listToTbl = list => {
    let listLen = List.length(list);
    let tbl = Hashtbl.create(listLen > 0 ? listLen : 1);
    List.iter(((key, item)) => Hashtbl.add(tbl, key, item), list);
    tbl;
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
  let layoutUniform =
    if (layoutUniform) {
      Some(Gpu.UniformMat3f(ref(Data.Mat3.id())));
    } else {
      None;
    };
  let pixelSizeUniform =
    if (pixelSizeUniform) {
      Some(Gpu.UniformVec2f(ref(Data.Vec2.zeros())));
    } else {
      None;
    };
  let elapsedUniform =
    if (elapsedUniform) {
      Some(Gpu.UniformFloat(ref(0.0)));
    } else {
      None;
    };
  let vo =
    switch vo {
    | Some(vo) => vo
    | None => {
        nodes: [],
        vertexBuffer: quadVertices,
        indexBuffer: Some(quadIndices)
      }
    };
  let layout = {size, padding, margin, childLayout, spacing, hAlign, vAlign};
  /* Easy way to create texture to draw to, this can
     be used in tandem with ~texNodes.
     Todo: pool of textures to draw to? This would
     require some texture refs to be changed
     when there is no more room and a new
     texture needs to be used
     Or possibly the size of the texture could be changed */
  let drawToTexture =
    switch drawTo {
    | Framebuffer => None
    | TextureRGB =>
      let texture = Gpu.Texture.makeEmptyRgb();
      Some(texture);
    | TextureRGBDim(dim) =>
      let texture = Gpu.Texture.makeEmptyRgb(~width=dim, ~height=dim, ());
      Some(texture);
    | TextureRGBA =>
      let texture = Gpu.Texture.makeEmptyRgba();
      Some(texture);
    | TextureRGBADim(dim) =>
      let texture = Gpu.Texture.makeEmptyRgba(~width=dim, ~height=dim, ());
      Some(texture);
    | TextureGreyscale =>
      let texture = Gpu.Texture.makeEmptyGreyscale();
      Some(texture);
    | TextureItem(texture) => Some(texture)
    };
  /* Texture transformation could either
     come with layout uniform, or in a separate
     uniform for example if the shader wants
     to do lighting based on layout coords */
  let texTransUniform =
    switch (drawTo, texTransUniform) {
    | (Framebuffer, _) => None
    | (_, false) => None
    | (_, true) => Some(Gpu.UniformMat3f(ref(Data.Mat3.id())))
    };
  {
    key,
    cls,
    id: nextNodeId(),
    onUpdate: update,
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
    rect: Geom2d.Rect.zeros(),
    scissorRect: Geom2d.Rect.zeros(),
    screenRect: Geom2d.Rect.zeros(),
    selfDraw,
    loading,
    transparent,
    partialDraw,
    hidden,
    children,
    deps,
    vertShader,
    fragShader,
    textureList,
    textures,
    vo,
    uniformList,
    uniforms,
    layoutUniform,
    pixelSizeUniform,
    elapsedUniform,
    drawToTexture,
    clearOnDraw,
    texTransUniform,
    parent: None,
    scene: None
  };
};

let setNodeParentsSceneKeyCls = self => {
  let rec loop = (node, parent) => {
    node.parent = parent;
    node.scene = Some(self);
    switch node.key {
    | None => ()
    | Some(key) =>
      if (Hashtbl.mem(self.nodesByKey, key)) {
        failwith("Node with key already exist: " ++ key);
      } else {
        Hashtbl.add(self.nodesByKey, key, node);
      }
    };
    switch node.cls {
    | None => ()
    | Some(cls) =>
      if (Hashtbl.mem(self.nodesByCls, cls)) {
        let current = Hashtbl.find(self.nodesByCls, cls);
        Hashtbl.replace(self.nodesByCls, cls, [node, ...current]);
      } else {
        Hashtbl.add(self.nodesByCls, cls, [node]);
      }
    };
    List.iter(dep => loop(dep, Some(node)), node.deps);
    List.iter(child => loop(child, Some(node)), node.children);
  };
  loop(self.root, None);
};

let make = (canvas, state, root, ~drawListDebug=false, ()) => {
  let drawListsDebug =
    switch drawListDebug {
    | false => None
    | true => Some({draw: DrawListDebug.make(canvas)})
    };
  let scene = {
    state,
    canvas,
    queuedUpdates: [],
    nodesByKey: Hashtbl.create(30),
    nodesByCls: Hashtbl.create(30),
    root,
    inited: false,
    drawLists: Hashtbl.create(30),
    drawListsDebug,
    initedLists: Hashtbl.create(30),
    initedDeps: Hashtbl.create(10),
    loadingNodes: [],
    anims: [],
    fbuffer: Hashtbl.create(1),
    stencilDraw: StencilDraw.make(canvas),
    updateNodes: ArrayB.make(None),
    updateRoot: None,
    hiddenNodes: []
  };
  setNodeParentsSceneKeyCls(scene);
  scene;
};

let queueUpdates = (scene, nodes) =>
  scene.queuedUpdates =
    List.fold_left(
      (updates, id) => [id, ...updates],
      scene.queuedUpdates,
      nodes
    );

let makeAnim =
    (onFrame, setLast, duration, ~key=?, ~next=?, ~frameInterval=1, ()) => {
  animKey: key,
  onFrame,
  setLast,
  duration,
  elapsed: 0.0,
  frameInterval,
  numFrames: 0,
  next
};

/* Removes any active anims with given key from anim list */
let clearAnim = (scene, animKey) =>
  scene.anims =
    List.filter(
      a =>
        switch a.animKey {
        | Some(key) => key != animKey
        | None => true
        },
      scene.anims
    );

/* Can also consider using depth buffer
   and draw from closest down parents when
   drawing layout things */
/* Performance is important for this function,
   so will look for more opportunities to cache etc.
   Possibly we could use an array sized for all
   nodes in the three and jump for children
   and deps.
   Use new collections from bucklescript. */
let buildUpdateTree = (scene, root) => {
  /* Also adds nodes to uniforms, vertexobjects and textures objects */
  let rec loop = (node, parent, isDep, isDepRoot, prevStacked) => {
    /* Feels maybe better to keep lists on scene instead of on these objects */
    Hashtbl.iter(
      (_key, uniform) => uniform.nodes = [node.id, ...uniform.nodes],
      node.uniforms
    );
    node.vo.nodes = [node.id, ...node.vo.nodes];
    Hashtbl.iter(
      (_key, tex: sceneTexture) => tex.nodes = [node.id, ...tex.nodes],
      node.textures
    );
    /* Check for hidden */
    if (node.hidden) {
      scene.hiddenNodes = [node.id, ...scene.hiddenNodes];
    };
    let stencil =
      if (node.transparent || node.partialDraw) {
        None;
      } else {
        /* Translate to screen coordinates */
        let updStencil = {rect: node.screenRect, active: false};
        /* There may not be much point to
           add stencil when isDepRoot */
        if (! isDepRoot) {
          /* Add reference to parent */
          switch parent {
          | Some(parent) => parent.stencils = [updStencil, ...parent.stencils]
          | None => ()
          };
        };
        Some(updStencil);
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
      updNode: node,
      rect: None,
      stencils: [],
      stencil,
      childRects: [],
      parentRects: [],
      updDeps: [],
      updChildren: [],
      parent,
      prevStacked,
      nextStacked: []
    };
    /* Add to indexes for node id/number and flag */
    scene.updateNodes.data[node.id] = Some(updateNode);
    updateNode.updDeps =
      List.map(dep => loop(dep, Some(updateNode), true, true, []), node.deps);
    switch node.layout.childLayout {
    | Stacked =>
      /* Loop stacked children, and pass on accumulated children as prevStacked */
      let updChildren =
        List.fold_left(
          (updChildren, stackedChild) => {
            let updChild =
              loop(stackedChild, Some(updateNode), isDep, false, updChildren);
            [updChild, ...updChildren];
          },
          [],
          node.children
        );
      /* Set nextStacked, updChildren is in reverse order from original child order */
      let _ =
        List.fold_left(
          (nextStacked, updChild) => {
            updChild.nextStacked = nextStacked;
            [updChild, ...nextStacked];
          },
          [],
          updChildren
        );
      updateNode.updChildren = List.rev(updChildren);
    | _ =>
      updateNode.updChildren =
        List.map(
          child => loop(child, Some(updateNode), isDep, false, []),
          node.children
        )
    };
    /* All children have added their stencils,
       propagate them to prevStacked and parent */
    let rec addStencilsToNodeAndChildren = updNode => {
      /* Add to updNode */
      List.iter(
        updStencil => updNode.stencils = [updStencil, ...updNode.stencils],
        updateNode.stencils
      );
      /* And updNodes children */
      List.iter(
        updNodeChild => addStencilsToNodeAndChildren(updNodeChild),
        updNode.updChildren
      );
    };
    List.iter(
      prevStacked => addStencilsToNodeAndChildren(prevStacked),
      prevStacked
    );
    if (! updateNode.isDepRoot) {
      switch updateNode.parent {
      | Some(parent) =>
        List.iter(
          updStencil => parent.stencils = [updStencil, ...parent.stencils],
          updateNode.stencils
        )
      | None => ()
      };
    };
    updateNode;
  };
  ArrayB.ensureSize(scene.updateNodes, currNodeId^);
  let tree = loop(root, None, false, false, []);
  /* Sort added hidden nodes */
  scene.hiddenNodes = List.sort((a, b) => a > b ? 1 : (-1), scene.hiddenNodes);
  tree;
};

let rec setChildToUpdate = updNode =>
  if (! updNode.updNode.hidden) {
    updNode.update = true;
    List.iter(
      updChild =>
        if (! updChild.update) {
          setChildToUpdate(updChild);
        },
      updNode.updChildren
    );
  };

let nodeDescrString = node =>
  switch node.key {
  | Some(key) => key ++ " id:" ++ string_of_int(node.id)
  | None =>
    switch node.cls {
    | Some(cls) => "cls: " ++ cls ++ " id:" ++ string_of_int(node.id)
    | None => "id:" ++ string_of_int(node.id)
    }
  };

let nodeIdDescr = (scene, nodeId) =>
  switch scene.updateNodes.data[nodeId] {
  | Some(n) => nodeDescrString(n.updNode)
  | None => failwith("Could not find node with id: " ++ string_of_int(nodeId))
  };

let rec setDepParentToUpdate = updNode =>
  if (! updNode.updNode.hidden) {
    /* todo: more fine grained update
       when coming from deps. Currently
       just turning on update
       There should be a rect propagated up
       to node with this dependency, then
       to it's children */
    updNode.update = true;
    /* This might be uneccesary if we do more
       fine grained update */
    List.iter(
      updChild =>
        if (! updChild.update) {
          setChildToUpdate(updChild);
        },
      updNode.updChildren
    );
    switch updNode.parent {
    | Some(parent) =>
      if (parent.isDep) {
        if (! parent.update) {
          setDepParentToUpdate(parent);
        };
      } else if
        /* Propagate to regular update of node that
           has this as dependency */
        (! parent.update) {
        setToUpdate(parent);
      }
    | None => ()
    };
  }
and setToUpdate = updNode =>
  if (! updNode.updNode.hidden && ! updNode.update) {
    updNode.update = true;
    /* If this is stacked layout, set
       previous children to update
       todo: Could possibly be rect/stencil */
    List.iter(
      prevStacked =>
        if (! prevStacked.update) {
          setToUpdate(prevStacked);
        },
      updNode.prevStacked
    );
    /* Set children to update */
    List.iter(
      updChild =>
        if (! updChild.update) {
          setChildToUpdate(updChild);
        },
      updNode.updChildren
    );
    /* Then next stacked if this is in the middle
       of a stacked layout */
    List.iter(
      nextStacked =>
        if (! nextStacked.update) {
          setToUpdate(nextStacked);
        },
      updNode.nextStacked
    );
    /* If this is a dep, set dependant parent also to update */
    if (updNode.isDep) {
      switch updNode.parent {
      | Some(parent) =>
        if (! parent.update) {
          if (parent.isDep) {
            setDepParentToUpdate(parent);
          } else {
            setToUpdate(parent);
          };
        }
      | None => failwith("Dependency without parent")
      };
    };
    /* Mark upwards in the tree that a child
       needs update */
    let rec markChildUpdate = updNode => {
      updNode.childUpdate = true;
      switch updNode.parent {
      | Some(parent) when parent.childUpdate == false =>
        markChildUpdate(parent)
      | _ => ()
      };
    };
    switch updNode.parent {
    | Some(parent) when parent.childUpdate == false => markChildUpdate(parent)
    | _ => ()
    };
  };

/* Traverses parents to ensure nodes that
   are set to update, and are transparent
   or partialDraw, are covered by a parent */
let actRectUntilCovered = updNode => {
  /* Repeating some checks a few places here
     percievely in the interest of a little
     performance as well as slightly
     different cases */
  let actStencil = updNode =>
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
    switch updNode.stencil {
    | Some(stencil) => stencil.active = true
    | None => ()
    };
  /* This creates new updRects for unexplored
     parents, sets their active status and return
     a list of newly created updateRects until
     a covering node is found */
  let rec loopNewRects = (parNode: updateListNode('s)) => {
    /* First process any prevStacked */
    let next =
      switch parNode.prevStacked {
      | [] => parNode.parent
      | [prev, ..._rest] => Some(prev)
      };
    switch next {
    | Some(next) =>
      if (! next.updNode.selfDraw) {
        /* Skipping layout nodes now,
           possibly there could be use of
           having rects there if they propagate
           to their children */
        loopNewRects(
          next
        );
      } else {
        let covers =
          ! (next.updNode.transparent || next.updNode.partialDraw)
          && Geom2d.Rect.contains(next.updNode.rect, updNode.updNode.rect);
        let updRect = {
          rect: updNode.updNode.rect,
          scisRect: updNode.updNode.scissorRect,
          rNode: next,
          stencils: [],
          covers,
          active: ! (next.updNode.hidden || next.update)
        };
        next.childRects = [updRect, ...next.childRects];
        /* For normal childs they are hidden when parent
           are hidden, but stacked layouts are
           rearranged so a parent may be hidden while
           a child is not */
        if (next.updNode.hidden) {
          [
            /* Pass on rect for possible later use, but it will not be active */
            updRect,
            ...loopNewRects(updRect.rNode)
          ];
        } else if (next.update) {
          if (updRect.covers) {
            [
              /* Update and covers, done */
              updRect
            ];
          } else {
            actStencil(updRect.rNode);
            [updRect, ...loopNewRects(updRect.rNode)];
          };
        } else if (updRect.covers) {
          [updRect];
        } else {
          actStencil(updRect.rNode);
          [updRect, ...loopNewRects(updRect.rNode)];
        };
      }
    | None =>
      failwith(
        "Could not find covering parent for: "
        ++ nodeDescrString(updNode.updNode)
        ++ ". Maybe add a background node?"
      )
    };
  };
  let rec loopRects = (rects: list(updateRect('s))) =>
    switch rects {
    | [updRect] =>
      /* Check if we need to continue
         or have found a covering node */
      if (updRect.rNode.updNode.hidden) {
        updNode.parentRects =
          List.append(updNode.parentRects, loopNewRects(updRect.rNode));
      } else if (updRect.rNode.update) {
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          updNode.parentRects =
            List.append(updNode.parentRects, loopNewRects(updRect.rNode));
        };
      } else {
        updRect.active = true;
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          updNode.parentRects =
            List.append(updNode.parentRects, loopNewRects(updRect.rNode));
        };
      }
    | [updRect, ...rest] =>
      if (updRect.rNode.updNode.hidden) {
        loopRects(rest);
      } else if (updRect.rNode.update) {
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          loopRects(rest);
        };
      } else {
        updRect.active = true;
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          loopRects(rest);
        };
      }
    | [] =>
      /* This should only be case for initial empty list,
         or actual empty list for rootish node */
      updNode.parentRects = loopNewRects(updNode)
    };
  loopRects(updNode.parentRects);
};

let rec setAdjecentStackedToUpdate = (updNode: option(updateListNode('s))) =>
  switch updNode {
  | Some(updNode) =>
    setAdjecentStackedToUpdate(updNode.parent);
    /* todo:(!) improve handling of adjecent stacked with stencil/rects */
    let numAdded =
      List.fold_left(
        (num, stacked) =>
          if (! stacked.updNode.hidden) {
            if (! stacked.update) {
              setToUpdate(stacked);
            };
            num + 1;
          } else {
            num;
          },
        0,
        List.append(updNode.prevStacked, updNode.nextStacked)
      );
    /* We also need to update parent if adjecent
       stack is updated (see todo) */
    if (numAdded > 0) {
      if (! updNode.updNode.hidden) {
        if (! updNode.update) {
          setToUpdate(updNode);
        };
      };
    };
  | None => ()
  };

/* UpdateNodes should be checked for visibility up front */
let createDrawList = (scene, updateNodes, updRoot) => {
  let updRoot =
    switch scene.updateNodes.data[updRoot.id] {
    | Some(updRoot) => updRoot
    | None => failwith("Root update node not found")
    };
  /* Set nodes to update */
  let updNodes = [
    List.fold_left(
      (nodes, nodeId) =>
        switch scene.updateNodes.data[nodeId] {
        | Some(updNode) =>
          setToUpdate(updNode);
          [updNode, ...nodes];
        | None =>
          failwith(
            "Could not find update node with id: " ++ string_of_int(nodeId)
          )
        },
      [],
      updateNodes
    ),
    []
  ];
  List.iter(
    nodes =>
      List.iter(
        (updNode: updateListNode('s)) =>
          /* Nodes could be part of a stacked layout
             here or further up in the scene. Traverse
             node and parents and for now, simply flag
             update if finding stacked nodes. (could be stencil/rect) */
          setAdjecentStackedToUpdate(
            Some(updNode)
          ),
        nodes
      ),
    updNodes
  );
  /* Assuming nodes that need update are flagged,
     do second pass on nodes to update
     to check for transparent nodes and
     ensure they are covered by a parent */
  let rec actRecLoop = (updNode: updateListNode('s)) =>
    if (updNode.update) {
      if (updNode.updNode.transparent || updNode.updNode.partialDraw) {
        actRectUntilCovered(updNode);
      };
      List.iter(child => actRecLoop(child), updNode.updChildren);
    } else if (updNode.childUpdate) {
      List.iter(child => actRecLoop(child), updNode.updChildren);
    };
  actRecLoop(updRoot);
  /* State of stencils and rect to determine
     when to clear them */
  let activeStencils = ref(None);
  let activeRect = ref(None);
  let equalsActiveStencils = (stencils: list(updateStencil)) => {
    let rec checkStencils =
            (
              stencils: list(updateStencil),
              activeStencils: list(updateStencil)
            ) =>
      switch (stencils, activeStencils) {
      | ([], []) => true
      | ([st1, ...rest1], [st2, ...rest2]) =>
        if (Geom2d.Rect.equals(st1.rect, st2.rect)) {
          false;
        } else {
          checkStencils(rest1, rest2);
        }
      | _ => false
      };
    switch activeStencils^ {
    | None => false
    | Some(activeStencils) => checkStencils(stencils, activeStencils)
    };
  };
  let equalsActiveRect = rect =>
    switch activeRect^ {
    | None => false
    | Some(activeRect) => Geom2d.Rect.equals(activeRect, rect)
    };
  /* Generate draw list
     Here we look at activated stencils and
     rects, and create a drawlist with instructions
     on how to draw those and the nodes themselves.
     The returned list will be reversed */
  let rec drawListLoop = (updNode, drawList, hidden) => {
    /* Propagating hidden variable. It is not ideal
       to do all else, but for now atleast.
       Need also to make sure whole tree is reset,
       So otherwise should ensure there are no hidden
       parents when setting a node to update.
       Possibly we might also have some logic
       to cancel hidden status in children */
    let hidden = hidden || updNode.updNode.hidden;
    /* Clean up childUpdate flag */
    if (updNode.childUpdate) {
      updNode.childUpdate = false;
    };
    /* First collect from dependencies */
    let drawList =
      List.fold_left(
        (drawList, dep) => drawListLoop(dep, drawList, hidden),
        drawList,
        updNode.updDeps
      );
    /* Check for selfDraw.
       Not sure where to branch on this,
       doing after deps now,
       especially if we render from child to parent
       when not transparent, it could make sense
       to update stencils */
    if (! updNode.updNode.selfDraw) {
      /* Allow draw texture also for nodes that don't self draw */
      let (drawList, drawToTexture) =
        if (updNode.update) {
          updNode.update = false;
          switch updNode.updNode.drawToTexture {
          | Some(_tex) =>
            ([BindDrawTexture(updNode.updNode), ...drawList], true);
          | None => (drawList, false)
          };
        } else {
          (drawList, false);
        };
      /* Recurse to children if they have update or childUpdate flagged */
      let drawList =
        List.fold_left(
          (drawList, updChild) =>
            if (updChild.update) {
              drawListLoop(updChild, drawList, hidden);
            } else if (updChild.childUpdate) {
              drawListLoop(updChild, drawList, hidden);
            } else {
              drawList;
            },
          drawList,
          updNode.updChildren
        );
      let drawList =
        if (drawToTexture) {
          [UnBindDrawTexture, ...drawList];
        } else {
          drawList;
        };
      drawList;
    } else if (hidden) {
      updNode.update = false;
      /* Clean up active flag on rects */
      List.iter(
        (updRect: updateRect('s)) => updRect.active = false,
        updNode.childRects
      );
      /* Recurse children to clean up if they
         have update or childUpdate flagged */
      let drawList =
        List.fold_left(
          (drawList, updChild) =>
            if (updChild.childUpdate) {
              drawListLoop(updChild, drawList, hidden);
            } else if (updChild.update) {
              drawListLoop(updChild, drawList, hidden);
            } else {
              drawList;
            },
          drawList,
          updNode.updChildren
        );
      /* Clean up active stencils */
      List.iter(stencil => stencil.active = false, updNode.stencils);
      drawList;
    } else {
      /* todo: possibly some microoptimizations if either lists are empty */
      /* Check for active stencils */
      let stencils =
        List.filter(
          (stencil: updateStencil) => stencil.active,
          updNode.stencils
        );
      /* Filter duplicate stencils/stencils contained by other stencils */
      let rec nonDupStencils = (list: list(updateStencil)) =>
        switch list {
        | [] => []
        | [stencil, ...rest] =>
          if (List.exists(
                (stencil2: updateStencil) =>
                  Geom2d.Rect.contains(stencil2.rect, stencil.rect),
                rest
              )) {
            nonDupStencils(rest);
          } else {
            [stencil, ...nonDupStencils(rest)];
          }
        };
      let stencils = nonDupStencils(stencils);
      /* Possibly a slight optimization
         would be to, in the case of one rect, draw
         stencil after rect. In the case of several rects,
         the stencils might take part in several of them
         and more logic would be needed.
         Or maybe better a bounding box over rects or node could
         be used to shape the stencils on the cpu */
      let drawList =
        switch stencils {
        | [] =>
          if (activeStencils^ == None) {
            drawList;
          } else {
            activeStencils := None;
            [ClearStencil, ...drawList];
          }
        | stencils =>
          if (activeStencils^ != None && equalsActiveStencils(stencils)) {
            drawList;
          } else {
            /* Create buffers */
            let data =
              Array.concat(
                List.map(
                  (stencil: updateStencil) => {
                    let rect = stencil.rect;
                    [|
                      rect.x,
                      rect.y -. rect.h, /* Bottom left */
                      rect.x,
                      rect.y, /* Top left */
                      rect.x +. rect.w,
                      rect.y, /* Top right */
                      rect.x +. rect.w,
                      rect.y
                      -. rect.h /* Bottom right */
                    |];
                  },
                  stencils
                )
              );
            let vb =
              Gpu.VertexBuffer.makeQuad(~data, ~usage=Gpu.DynamicDraw, ());
            let ib =
              Gpu.IndexBuffer.make(
                Gpu.IndexBuffer.makeQuadsData(List.length(stencils)),
                Gpu.DynamicDraw
              );
            let stencilBuffers = {
              stencilVertices:
                Gpu.VertexBuffer.init(
                  vb,
                  scene.canvas.context,
                  scene.stencilDraw.program.programRef
                ),
              stencilIndices: Gpu.IndexBuffer.init(ib, scene.canvas.context)
            };
            activeStencils := Some(stencils);
            [DrawStencil(stencilBuffers), ...drawList];
          }
        };
      if (updNode.update) {
        /* Clean up update flag */
        updNode.update = false;
        /* Clear active rect if any */
        let drawList =
          if (activeRect^ != None) {
            activeRect := None;
            [ClearRect, ...drawList];
          } else {
            drawList;
          };
        let (drawList, drawToTexture) =
          switch updNode.updNode.drawToTexture {
          | Some(tex) =>
            ([BindDrawTexture(updNode.updNode), ...drawList], true);
          | None => (drawList, false)
          };
        let drawList = [DrawNode(updNode.updNode), ...drawList];
        /* Since this node is flagged for update, assume all children
           will also be updated */
        let drawList =
          List.fold_left(
            (drawList, updChild) => drawListLoop(updChild, drawList, hidden),
            drawList,
            updNode.updChildren
          );
        let drawList =
          if (drawToTexture) {
            [UnBindDrawTexture, ...drawList];
          } else {
            drawList;
          };
        /* Clean up active stencils */
        List.iter(stencil => stencil.active = false, stencils);
        /* Clean up active rects */
        List.iter(
          (rect: updateRect('s)) => rect.active = false,
          updNode.childRects
        );
        drawList;
      } else {
        /* Check for active rects */
        let rects =
          List.filter(
            (updRect: updateRect('s)) => updRect.active,
            updNode.childRects
          );
        /* Filter duplicate/contained  */
        let rec nonDupRects = (list: list(updateRect('s))) =>
          switch list {
          | [] => []
          | [rect, ...rest] =>
            /* !! Also negates active flag !! */
            rect.active = false;
            if (List.exists(
                  (rect2: updateRect('s)) =>
                    Geom2d.Rect.contains(rect2.rect, rect.rect),
                  rest
                )) {
              nonDupRects(rest);
            } else {
              [rect, ...nonDupRects(rest)];
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
        let drawList =
          List.fold_left(
            (drawList, rect: updateRect('s)) =>
              if (activeRect^ == None) {
                activeRect := Some(rect.scisRect);
                [
                  DrawNode(updNode.updNode),
                  SetRect(rect.scisRect),
                  ...drawList
                ];
              } else if (equalsActiveRect(rect.scisRect)) {
                [DrawNode(updNode.updNode)];
              } else {
                /* Clear and set new rect */
                activeRect := Some(rect.scisRect);
                [
                  DrawNode(updNode.updNode),
                  SetRect(rect.scisRect),
                  ClearRect,
                  ...drawList
                ];
              },
            drawList,
            rects
          );
        /* Recurse to children if they have update or childUpdate flagged */
        let drawList =
          List.fold_left(
            (drawList, updChild) =>
              if (updChild.update) {
                drawListLoop(updChild, drawList, hidden);
              } else if (updChild.childUpdate) {
                drawListLoop(updChild, drawList, hidden);
              } else {
                drawList;
              },
            drawList,
            updNode.updChildren
          );
        List.iter(stencil => stencil.active = false, stencils);
        drawList;
      };
    };
  };
  /* Not sure if we need to useProgram for stencil buffers */
  Reasongl.Gl.useProgram(
    ~context=scene.canvas.context,
    scene.stencilDraw.program.programRef
  );
  scene.canvas.currProgram = Some(scene.stencilDraw.program);
  let drawList = drawListLoop(updRoot, [], false);
  /* Clear active rect if any */
  let drawList =
    if (activeRect^ != None) {
      [ClearRect, ...drawList];
    } else {
      drawList;
    };
  /* Clear active stencils if any */
  let drawList =
    if (activeStencils^ != None) {
      [ClearStencil, ...drawList];
    } else {
      drawList;
    };
  List.rev(drawList);
};

let getFBuffer = (self, config: fbufferConfig) =>
  if (Hashtbl.mem(self.fbuffer, config)) {
    Hashtbl.find(self.fbuffer, config);
  } else {
    let fbuffer =
      Gpu.FrameBuffer.init(
        Gpu.FrameBuffer.make(config.width, config.height),
        self.canvas.context
      );
    Hashtbl.add(self.fbuffer, config, fbuffer);
    fbuffer;
  };

let debugNodes = [];

type drawListDrawTo = {
    dtFbuffer: Gpu.FrameBuffer.inited,
    dtTexture: Gpu.Texture.t,
    dtWidth: int,
    dtHeight: int
};

type drawListState = {
    mutable activeDrawTo: option(drawListDrawTo),
    mutable drawToStack: list(drawListDrawTo)
};

let bindDrawTexture = (self, node, dlState) =>
  switch node.drawToTexture {
  | Some(texture) =>
    /* If any active drawTo, put in stack */
    switch dlState.activeDrawTo {
    | None => ()
    | Some(drawTo) =>
        dlState.drawToStack = [drawTo, ...dlState.drawToStack];
    };
    let config = {width: 1024, height: 1024};
    let fbuffer = getFBuffer(self, config);
    Gpu.FrameBuffer._bindFramebuffer(
      ~context=self.canvas.context,
      Gpu.FrameBuffer.frameBufferC,
      Js.Nullable.return(fbuffer.frameBufferRef)
    );
    /* Let pixels go from offset of node */
    /* Possibly use for this, would need to adjust layouts */
    let dtWidth = int_of_float(node.rect.w);
    let dtHeight = int_of_float(node.rect.h);
     Reasongl.Gl.viewport(
         ~context=self.canvas.context,
         ~x=0,
         ~y=0,
         ~width=dtWidth,
         ~height=dtHeight
     );
    Gpu.FrameBuffer.bindTexture(fbuffer, self.canvas.context, texture);
    if (node.clearOnDraw) {
      /* Alpha channel should maybe only cleared on rgba */
      Gpu.Canvas.clear(self.canvas, 0.0, 0.0, 0.0, 0.0);
    };
    dlState.activeDrawTo = Some({
        dtFbuffer: fbuffer,
        dtTexture: texture,
        dtWidth,
        dtHeight
    });
    let debugBuffered = Some("lightBase");
    let debugBuffered = None;
    switch (debugBuffered, node.key, node.cls) {
    | (Some(d), Some(k), _) when k == d =>
      [%debugger];
      Gpu.Canvas.clearFramebuffer(self.canvas);
    | (Some(d), _, Some(k)) when k == d =>
      [%debugger];
      Gpu.Canvas.clearFramebuffer(self.canvas);
    | _ => ()
    };
  | None => failwith("Node is not drawToTexture")
  };

let unbindDrawTexture = (self, dlState) => {
  switch dlState.drawToStack {
  | [] =>
    dlState.activeDrawTo = None;
    Gpu.Canvas.clearFramebuffer
      (self.canvas);
    /* Possibly unneccesary */
    Reasongl.Gl.viewport(
        ~context=self.canvas.context,
        ~x=0,
        ~y=0,
        ~width=self.canvas.width,
        ~height=self.canvas.height
    );
  | [{dtFbuffer, dtTexture, dtWidth, dtHeight} as prevActive, ...rest] =>
    Gpu.FrameBuffer.bindTexture(dtFbuffer, self.canvas.context, dtTexture);
    Gpu.Canvas.setFramebuffer(self.canvas, dtFbuffer);
    dlState.drawToStack = rest;
    dlState.activeDrawTo = Some(prevActive);
    Reasongl.Gl.viewport(
        ~context=self.canvas.context,
        ~x=0,
        ~y=0,
        ~width=dtWidth,
        ~height=dtHeight
    );
  };
};

let draw = (self, node) =>
  switch node.drawState {
  | Some(drawState) =>
    switch node.elapsedUniform {
    | Some(elapsedUniform) =>
      Gpu.Uniform.setFloat(elapsedUniform, self.canvas.elapsed)
    | None => ()
    };
    let isDebug =
      switch (node.key, node.cls) {
      | (Some(key), _) => List.exists(debug => key == debug, debugNodes)
      | (_, Some(cls)) => List.exists(debug => cls == debug, debugNodes)
      | _ => false
      };
    if (isDebug) {
      [%debugger];
    };
    if (node.transparent) {
      let context = self.canvas.context;
      Gpu.glEnable(context, Gpu.Constants.blend);
      Gpu.glBlendFunc(
        context,
        Gpu.Constants.src_alpha,
        Gpu.Constants.one_minus_src_alpha
      );
      Gpu.DrawState.draw(drawState, self.canvas);
      Gpu.glDisable(context, Gpu.Constants.blend);
    } else if (isDebug) {
      Gpu.DrawState.draw(drawState, self.canvas);
      Gpu.Canvas.clearFramebuffer(self.canvas);
      Gpu.DrawState.draw(drawState, self.canvas);
    } else {
      Gpu.DrawState.draw(drawState, self.canvas);
    };
  | None => failwith("Drawstate not found")
  };

module Gl = Reasongl.Gl;

/* Debug parameter passed, it would be a slight optimization
   to create own function for debug.. */
let processDrawList = (scene, drawList, debug) => {
  let context = scene.canvas.context;
  let elapsed = scene.canvas.elapsed;
  let dlState = {
    activeDrawTo: None,
    drawToStack: []
  };
  open Gpu;
  module Gl = Reasongl.Gl;
  let rec processDrawEl = list =>
    switch list {
    | [el, ...rest] =>
      switch el {
      | DrawNode(node) =>
        if (debug) {
          switch (scene.drawListsDebug, node.layoutUniform, node.drawToTexture) {
          | (Some(drawDebug), Some(UniformMat3f(uniformMat)), None) =>
            DrawListDebug.draw(
              drawDebug.draw,
              uniformMat^,
              elapsed +. float_of_int(node.id)
            )
          | _ => ()
          };
        } else if (node.loading) {
          if (! List.exists(loading => loading === node, scene.loadingNodes)) {
            scene.loadingNodes = [node, ...scene.loadingNodes];
          };
        } else {
          switch node.onUpdate {
          | Some(update) => [@bs] update(node, scene.state)
          | None => draw(scene, node)
          };
        }
      | DrawStencil(stencilBuffers) =>
        glEnable(context, Stencil.stencilTest);
        Canvas.clearStencil(scene.canvas);
        /* Always write 1s */
        Stencil.stencilFunc(context, Stencil.always, 1, 0xFF);
        Stencil.stencilOp(
          context,
          Stencil.keep,
          Stencil.keep,
          Stencil.replace
        );
        Stencil.stencilMask(context, 0xFF);
        /* Disable color and depth writing when writing
           stencil rects */
        Canvas.colorMask(context, false, false, false, false);
        Canvas.depthMask(context, false);
        StencilDraw.draw(
          scene.stencilDraw,
          stencilBuffers.stencilVertices,
          stencilBuffers.stencilIndices
        );
        Canvas.colorMask(context, true, true, true, true);
        Canvas.depthMask(context, true);
        /* Set stencil test function to check for 1's
           when drawing nodes that should be affected
           by the stencil */
        Stencil.stencilFunc(context, Stencil.notEqual, 1, 0xFF);
        /* We can also disable stencil writing */
        Stencil.stencilMask(context, 0x00);
      | ClearStencil => glDisable(context, Stencil.stencilTest)
      | SetRect(rect) =>
        glEnable(context, Scissor.scissorTest);
        /* todo: Int rect */
        Scissor.scissor(
          context,
          int_of_float(rect.x),
          int_of_float(rect.y),
          int_of_float(rect.w),
          int_of_float(rect.h)
        );
      | ClearRect => glDisable(context, Scissor.scissorTest)
      | BindDrawTexture(n) =>
        if (! debug) {
          bindDrawTexture(scene, n, dlState);
        }
      | UnBindDrawTexture =>
        if (! debug) {
          unbindDrawTexture(scene, dlState);
        }
      };
      processDrawEl(rest);
    | [] => ()
    };
  processDrawEl(drawList);
};

let logDrawList = (scene, updateState: updateState, drawList) => {
  Js.log2(
    "====\nDrawlist",
    List.fold_left(
      (str, id) =>
        switch scene.updateNodes.data[id] {
        | Some(node) => str ++ ", " ++ nodeDescrString(node.updNode)
        | None => str
        },
      "",
      updateState.updateNodes
    )
  );
  let rec processDrawEl = list =>
    switch list {
    | [el, ...rest] =>
      switch el {
      | DrawNode(node) => Js.log("Draw node: " ++ nodeDescrString(node))
      | DrawStencil(stencilBuffers) =>
        Js.log2("Draw stencil: ", stencilBuffers.stencilVertices.data)
      | ClearStencil => Js.log("Clear stencil")
      | SetRect(rect) => Js.log2("Set rect: ", rect)
      | ClearRect => Js.log("Clear rect")
      | BindDrawTexture(n) =>
        Js.log("Bind draw texture: " ++ nodeDescrString(n))
      | UnBindDrawTexture => Js.log("Unbind draw texture")
      };
      processDrawEl(rest);
    | [] => ()
    };
  processDrawEl(drawList);
};

let getSceneNodesToUpdate = (animIds, root) => {
  let rec loop = (node, list, parentUpdate) => {
    let depsToUpdate =
      List.fold_left((list, dep) => loop(dep, list, false), [], node.deps);
    let doUpdate =
      parentUpdate
      || List.length(depsToUpdate) > 0
      || List.exists(animId => node.id == animId, animIds);
    /* todo: tail recursive? */
    let childList =
      List.fold_left(
        (list, child) => loop(child, list, doUpdate),
        list,
        node.children
      );
    let list =
      if (doUpdate && node.selfDraw) {
        [node, ...childList];
      } else {
        childList;
      };
    /* Deps first */
    List.fold_left((list, dep) => [dep, ...list], list, depsToUpdate);
  };
  loop(root, [], false);
};

let createNodeDrawState = (self, node) => {
  /* Texture uniforms */
  let uniforms =
    List.fold_left(
      (uniforms, name) => {
        let nodeTex = Hashtbl.find(node.textures, name);
        switch nodeTex.uniformMat {
        | Some(uniform) => [
            Gpu.Uniform.make(name ++ "Mat", uniform),
            ...uniforms
          ]
        | None => uniforms
        };
      },
      [],
      node.textureList
    );
  /* Elapsed uniform */
  let uniforms =
    switch node.elapsedUniform {
    | Some(elapsedUniform) => [
        Gpu.Uniform.make("elapsedScene", elapsedUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  /* PixelSize uniform */
  let uniforms =
    switch node.pixelSizeUniform {
    | Some(pixelSizeUniform) => [
        Gpu.Uniform.make("pixelSize", pixelSizeUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  /* Layout uniform */
  let uniforms =
    switch node.layoutUniform {
    | Some(layoutUniform) => [
        Gpu.Uniform.make("layout", layoutUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  /* TexTrans uniform */
  let uniforms =
    switch node.texTransUniform {
    | Some(texTransUniform) => [
        Gpu.Uniform.make("texTrans", texTransUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  let uniforms =
    Array.of_list(
      List.append(
        List.map(
          key => {
            let sUniform = Hashtbl.find(node.uniforms, key);
            Gpu.Uniform.make(key, sUniform.uniform);
          },
          node.uniformList
        ),
        uniforms
      )
    );
  let textures =
    Array.of_list(
      List.map(
        key => {
          let nodeTex = Hashtbl.find(node.textures, key);
          Gpu.ProgramTexture.make(key, nodeTex.texture);
        },
        node.textureList
      )
    );
  let vertShader =
    switch node.vertShader {
    | Some(vertShader) => vertShader
    | None => failwith("Vertex shader not found on: " ++ nodeDescrString(node))
    };
  let fragShader =
    switch node.fragShader {
    | Some(fragShader) => fragShader
    | None =>
      failwith("Fragment shader not found on: " ++ nodeDescrString(node))
    };
  node.drawState =
    Some(
      Gpu.DrawState.init(
        self.canvas.context,
        Gpu.Program.make(vertShader, fragShader, uniforms),
        node.vo.vertexBuffer,
        node.vo.indexBuffer,
        textures
      )
    );
};

let createDrawStates = self => {
  let rec loop = node => {
    if (node.selfDraw) {
      createNodeDrawState(self, node);
    };
    List.iter(dep => loop(dep), node.deps);
    List.iter(child => loop(child), node.children);
  };
  loop(self.root);
};

let getNode = (self, key) =>
  if (Hashtbl.mem(self.nodesByKey, key)) {
    Some(Hashtbl.find(self.nodesByKey, key));
  } else {
    None;
  };

let getNodeUnsafe = (scene, nodeKey) =>
  switch (getNode(scene, nodeKey)) {
  | Some(node) => node
  | None => failwith("Could not find node: " ++ nodeKey)
  };

let hideNode = (scene, node) => {
  node.hidden = true;
  scene.hiddenNodes =
    List.sort_uniq(
      (a, b) => a > b ? 1 : (-1),
      [node.id, ...scene.hiddenNodes]
    );
  /* todo: Queuing root for now to cover updated area,
     but this could be more precise. Optimally find covering
     area and redraw */
  queueUpdates(scene, [scene.root.id]);
};

let hideNodeByKey = (scene, nodeKey) =>
  switch (getNode(scene, nodeKey)) {
  | Some(node) => hideNode(scene, node)
  | None => failwith("Could not find node: " ++ nodeKey)
  };

let showNode = (scene, node) => {
  node.hidden = false;
  scene.hiddenNodes = List.filter(n => n != node.id, scene.hiddenNodes);
  queueUpdates(scene, [node.id]);
};

let showNodeByKey = (scene, nodeKey) =>
  switch (getNode(scene, nodeKey)) {
  | Some(node) => showNode(scene, node)
  | None => failwith("Could not find node: " ++ nodeKey)
  };

type parentDrawToTex = {
  nOffX: float,
  nOffY: float,
  texVpWidth: float,
  texVpHeight: float
};

/* Todo: More just in time, and fine grained updates */
let calcLayout = self => {
  let debug = false;
  /* These are assumed "ints"/rounded */
  let vpWidth = float_of_int(self.canvas.width);
  let vpHeight = float_of_int(self.canvas.height);
  let calcMargins = (node, outerWidth, outerHeight) => {
    /* Calc margins. Not sure how best to handle
       all cases. Aspect I guess should be kept
       inside the margins, currently the
       margins are scaled by padded dimensions
       around it */
    let cl = node.calcLayout;
    /* Calc margins and dimensions inside margin */
    let xdim = dim =>
      switch dim {
      | Scale(scale) => outerWidth *. scale
      | Pixel(pixels) => pixels
      };
    let ydim = dim =>
      switch dim {
      | Scale(scale) => outerHeight *. scale
      | Pixel(pixels) => pixels
      };
    switch node.layout.margin {
    | None => ()
    | Some(margin) =>
      switch margin {
      | Margin(dimension) =>
        switch dimension {
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
        }
      | MarginXY(dimX, dimY) =>
        let dimX = xdim(dimX);
        let dimY = ydim(dimY);
        cl.marginX1 = dimX;
        cl.marginX2 = dimX;
        cl.marginY1 = dimY;
        cl.marginY2 = dimY;
      | MarginRBLT(mRight, mBottom, mLeft, mTop) =>
        let dimX2 = xdim(mRight);
        let dimY2 = ydim(mBottom);
        let dimX1 = xdim(mLeft);
        let dimY1 = ydim(mTop);
        cl.marginX1 = dimX1;
        cl.marginX2 = dimX2;
        cl.marginY1 = dimY1;
        cl.marginY2 = dimY2;
      }
    };
  };
  let calcNodeDimensions = (node, paddedWidth, paddedHeight) => {
    calcMargins(node, paddedWidth, paddedHeight);
    let cl = node.calcLayout;
    let (nodeWidth, nodeHeight) =
      switch node.layout.size {
      | Aspect(ratio) =>
        switch node.layout.margin {
        | Some(_) =>
          /* Keeping aspect inside margins */
          let innerWidth = paddedWidth -. cl.marginX1 -. cl.marginX2;
          let innerHeight = paddedHeight -. cl.marginY1 -. cl.marginY2;
          let parentAspect = innerWidth /. innerHeight;
          if (ratio < parentAspect) {
            /* Limit by height */
            let innerWidth = innerHeight *. ratio;
            (innerWidth +. cl.marginX1 +. cl.marginX2, paddedHeight);
          } else {
            /* Limit by width */
            let innerHeight = innerWidth /. ratio;
            (paddedWidth, innerHeight +. cl.marginY1 +. cl.marginY2);
          };
        | None =>
          let parentAspect = paddedWidth /. paddedHeight;
          if (ratio < parentAspect) {
            /* Limit by height */
            let width = paddedHeight *. ratio;
            (width, paddedHeight);
          } else {
            /* Limit by width */
            let height = paddedWidth /. ratio;
            (paddedWidth, height);
          };
        }
      | Dimensions(dimX, dimY) =>
        /* Get in pixel or ratio form */
        (
          switch dimX {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedWidth *. scale
          },
          switch dimY {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedHeight *. scale
          }
        )
      | WidthRatio(dimX, ratio) =>
        let width =
          switch dimX {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedWidth *. scale
          };
        (width, width /. ratio);
      | HeightRatio(dimY, ratio) =>
        let height =
          switch dimY {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedHeight *. scale
          };
        (height *. ratio, height);
      };
    cl.pWidth = nodeWidth;
    cl.pHeight = nodeHeight;
    cl.inWidth = cl.pWidth -. cl.marginX1 -. cl.marginX2;
    cl.inHeight = cl.pHeight -. cl.marginY1 -. cl.marginY2;
  };
  let rec calcNodeLayout = (node, parentDrawToTex) => {
    let layout = node.layout;
    let calcLayout = node.calcLayout;
    /* Calc padding */
    let (paddedWidth, paddedHeight, x, y) =
      switch layout.padding {
      | None => (
          calcLayout.pWidth,
          calcLayout.pHeight,
          calcLayout.pXOffset,
          calcLayout.pYOffset
        )
      | Some(Pixel(padding)) => (
          calcLayout.pWidth -. padding *. 2.0,
          calcLayout.pHeight -. padding *. 2.0,
          calcLayout.pXOffset +. padding,
          calcLayout.pYOffset +. padding
        )
      | Some(Scale(padding)) =>
        let scaledXPadding = calcLayout.pWidth *. padding;
        let scaledYPadding = calcLayout.pHeight *. padding;
        (
          calcLayout.pWidth -. scaledXPadding *. 2.0,
          calcLayout.pHeight -. scaledYPadding *. 2.0,
          calcLayout.pXOffset +. scaledXPadding,
          calcLayout.pYOffset +. scaledYPadding
        );
      };
    /* Set width/height of children and deps */
    List.iter(
      dep => calcNodeDimensions(dep, paddedWidth, paddedHeight),
      node.deps
    );
    List.iter(
      child => calcNodeDimensions(child, paddedWidth, paddedHeight),
      node.children
    );
    /* Set x and y offset for deps */
    List.iter(
      dep => {
        dep.calcLayout.pXOffset = x;
        dep.calcLayout.pYOffset = y;
      },
      node.deps
    );
    /* Todo: allow to set a pixel value or something for one child,
       then allow one of the other elements to stretch to available space */
    /* Handle aligns */
    switch layout.childLayout {
    | Stacked =>
      switch layout.hAlign {
      | AlignLeft =>
        List.iter(
          child => child.calcLayout.pXOffset = x +. child.calcLayout.marginX1,
          node.children
        )
      | AlignCenter =>
        List.iter(
          child =>
            child.calcLayout.pXOffset =
              x
              +. (paddedWidth -. child.calcLayout.pWidth)
              /. 2.0
              +. child.calcLayout.marginX1,
          node.children
        )
      | AlignRight =>
        List.iter(
          child =>
            child.calcLayout.pXOffset =
              x
              +. paddedWidth
              -. child.calcLayout.pWidth
              +. child.calcLayout.marginX1,
          node.children
        )
      };
      switch layout.vAlign {
      | AlignTop =>
        List.iter(
          child => child.calcLayout.pYOffset = y +. child.calcLayout.marginY1,
          node.children
        )
      | AlignMiddle =>
        List.iter(
          child =>
            child.calcLayout.pYOffset =
              y
              +. (paddedHeight -. child.calcLayout.pHeight)
              /. 2.0
              +. child.calcLayout.marginY1,
          node.children
        )
      | AlignBottom =>
        List.iter(
          child =>
            child.calcLayout.pYOffset =
              y
              +. paddedHeight
              -. child.calcLayout.pHeight
              +. child.calcLayout.marginY1,
          node.children
        )
      };
    | Horizontal =>
      /* Use ratio as scale. Not sure if it makes total sense,
         possibly restrict to width, height dimensions */
      let spacing =
        switch layout.spacing {
        | Some(Pixel(pixel)) => pixel
        | Some(Scale(scale)) => paddedWidth *. scale
        | None => 0.0
        };
      /* Set xoffset */
      switch layout.hAlign {
      | AlignLeft =>
        let _ =
          List.fold_left(
            (xOffset, child) => {
              child.calcLayout.pXOffset = xOffset +. child.calcLayout.marginX1;
              xOffset +. spacing +. child.calcLayout.pWidth;
            },
            x,
            node.children
          );
        ();
      | AlignCenter =>
        /* Get total width and start from (paddedWidth  - totalWidth) / 2) */
        let totalWidth =
          List.fold_left(
            (totalWidth, child) => totalWidth +. child.calcLayout.pWidth,
            0.0,
            node.children
          )
          +. spacing
          *. float_of_int(List.length(node.children) - 1);
        let xOffset = (paddedWidth -. totalWidth) /. 2.0;
        let _ =
          List.fold_left(
            (xOffset, child) => {
              child.calcLayout.pXOffset = xOffset +. child.calcLayout.marginX1;
              xOffset +. spacing +. child.calcLayout.pWidth;
            },
            x +. xOffset,
            node.children
          );
        ();
      | AlignRight =>
        let _ =
          List.fold_right(
            (child, xOffset) => {
              let childOffset = xOffset -. child.calcLayout.pWidth;
              child.calcLayout.pXOffset =
                childOffset +. child.calcLayout.marginX1;
              childOffset -. spacing;
            },
            node.children,
            x +. paddedWidth
          );
        ();
      };
      switch layout.vAlign {
      | AlignTop =>
        List.iter(
          child => child.calcLayout.pYOffset = y +. child.calcLayout.marginY1,
          node.children
        )
      | AlignMiddle =>
        List.iter(
          child =>
            child.calcLayout.pYOffset =
              y
              +. (paddedHeight -. child.calcLayout.pHeight)
              /. 2.0
              +. child.calcLayout.marginY1,
          node.children
        )
      | AlignBottom =>
        List.iter(
          child =>
            child.calcLayout.pYOffset =
              y
              +. paddedHeight
              -. child.calcLayout.pHeight
              +. child.calcLayout.marginY1,
          node.children
        )
      };
    /* Get total width */
    | Vertical =>
      let spacing =
        switch layout.spacing {
        | Some(Pixel(pixel)) => pixel
        | Some(Scale(scale)) => paddedHeight *. scale
        | None => 0.0
        };
      switch layout.hAlign {
      | AlignLeft =>
        List.iter(
          child => child.calcLayout.pXOffset = x +. child.calcLayout.marginX1,
          node.children
        )
      | AlignCenter =>
        List.iter(
          child =>
            child.calcLayout.pXOffset =
              x
              +. (paddedWidth -. child.calcLayout.pWidth)
              /. 2.0
              +. child.calcLayout.marginX1,
          node.children
        )
      | AlignRight =>
        List.iter(
          child =>
            child.calcLayout.pXOffset =
              x
              +. paddedWidth
              -. child.calcLayout.pWidth
              +. child.calcLayout.marginX1,
          node.children
        )
      };
      switch layout.vAlign {
      | AlignTop =>
        let _ =
          List.fold_left(
            (yOffset, child) => {
              child.calcLayout.pYOffset = yOffset +. child.calcLayout.marginY1;
              yOffset +. child.calcLayout.pHeight +. spacing;
            },
            y,
            node.children
          );
        ();
      | AlignMiddle =>
        let totalHeight =
          List.fold_left(
            (totalHeight, child) => totalHeight +. child.calcLayout.pHeight,
            0.0,
            node.children
          )
          +. spacing
          *. float_of_int(List.length(node.children) - 1);
        let _ =
          List.fold_left(
            (yOffset, child) => {
              child.calcLayout.pYOffset = yOffset +. child.calcLayout.marginY1;
              yOffset +. child.calcLayout.pHeight +. spacing;
            },
            y +. (paddedHeight -. totalHeight) /. 2.0,
            node.children
          );
        ();
      | AlignBottom =>
        let _ =
          List.fold_right(
            (child, yOffset) => {
              let childOffset = yOffset -. child.calcLayout.pHeight;
              child.calcLayout.pYOffset =
                childOffset +. child.calcLayout.marginY1;
              childOffset -. spacing;
            },
            node.children,
            y +. paddedHeight
          );
        ();
      };
    };
    /* Round numbers to make them work as pixel values
       since they are used with rects etc and nodes
       are drawn independently */
    let pXOff = floor(calcLayout.pXOffset +. 0.5);
    let pYOff = floor(calcLayout.pYOffset +. 0.5);
    let inW =
      floor(calcLayout.inWidth +. (calcLayout.pXOffset -. pXOff) +. 0.5);
    let inH =
      floor(calcLayout.inHeight +. (calcLayout.pYOffset -. pYOff) +. 0.5);
    let pW =
      floor(calcLayout.pWidth +. (calcLayout.pXOffset -. pXOff) +. 0.5);
    let pH =
      floor(calcLayout.pHeight +. (calcLayout.pYOffset -. pYOff) +. 0.5);
    node.rect.x = pXOff;
    node.rect.y = pYOff;
    node.rect.w = inW;
    node.rect.h = inH;
    node.scissorRect.x = pXOff;
    node.scissorRect.y = vpHeight -. pYOff -. inH;
    node.scissorRect.w = inW;
    node.scissorRect.h = inH;
    /* Bit torn on whether to do this for all nodes,
       currently used for stencils, possibly there
       are other uses, but generally layout matrix is also available.
       We have vpWidth available, if calculations
       are moved, vpWidth etc might beneficially
       be stored. */
    node.screenRect.x = pXOff /. vpWidth *. 2.0 -. 1.0;
    node.screenRect.y = 1.0 -. pYOff /. vpHeight *. 2.0;
    node.screenRect.w = inW /. vpWidth *. 2.0;
    node.screenRect.h = inH /. vpHeight *. 2.0;
    if (debug) {
      Js.log("Layout for " ++ nodeDescrString(node));
      Js.log2("pWidth: ", calcLayout.pWidth);
      Js.log2("pHeight: ", calcLayout.pHeight);
      Js.log2("xOff: ", calcLayout.pXOffset);
      Js.log2("yOff: ", calcLayout.pYOffset);
    };
    let parentDrawToTex =
      switch node.layoutUniform {
      | None => parentDrawToTex
      | Some(layoutUniform) =>
        let scaleX = inW /. vpWidth;
        let scaleY = inH /. vpHeight;
        let scale = Data.Mat3.scale(scaleX, scaleY);
        switch (node.drawToTexture, node.texTransUniform) {
        | (Some(texture), None) =>
          /* Ensuring texture has size to hold result */
          Gpu.Texture.ensureSize(
            texture,
            self.canvas.context,
            int_of_float(node.rect.w),
            int_of_float(node.rect.h)
          );
          /* Translate texture to begin from 0,0.
             The important thing is correspondance with texture
             uniforms below */
          /* Todo: maintain values for quick lookup? */
          /* Todo: Pack texture into available space,
             resize texture on need? */
          /*
           let (texWidth, texHeight) = switch (Gpu.Texture.getSize(texture)) {
           | Some((texWidth, texHeight)) => (float_of_int(texWidth), float_of_int(texHeight))
           | None => failwith("Could not find texture size");
           };
           */
          let layoutMat = Data.Mat3.scaleTrans(1.0, 1.0, 0.0, 0.0);
          Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
          if (debug) {
            Js.log2("Layout: ", layoutMat);
          };
          /* Initiate parentTexTrans */
          Some({nOffX: pXOff, nOffY: pYOff, texVpWidth: inW, texVpHeight: inH});
        | (None, _) =>
          /* todo: case of None + texTransform */
          let transX = (pXOff *. 2.0 +. inW) /. vpWidth -. 1.0;
          let transY = (pYOff *. (-2.0) -. inH) /. vpHeight +. 1.0;
          let layoutMat =
            switch parentDrawToTex {
            | None => Data.Mat3.scaleTrans(scaleX, scaleY, transX, transY)
            | Some(pt) =>
              Data.Mat3.scaleTrans(
                inW /. pt.texVpWidth,
                inH /. pt.texVpHeight,
                ((pXOff -. pt.nOffX) *. 2.0 +. inW) /. pt.texVpWidth -. 1.0,
                ((pYOff -. pt.nOffY) *. (-2.0) -. inH) /. pt.texVpHeight +. 1.0
              )
            };
          Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
          if (debug) {
            Js.log2("Layout: ", layoutMat);
          };
          parentDrawToTex;
        | (Some(texture), Some(texTransUniform)) =>
          /* Ensuring texture has size to hold result */
          Gpu.Texture.ensureSize(
            texture,
            self.canvas.context,
            int_of_float(node.rect.w),
            int_of_float(node.rect.h)
          );
          /* First regular layout uniform */
          let transX = (pXOff *. 2.0 +. inW) /. vpWidth -. 1.0;
          let transY = (pYOff *. (-2.0) -. inH) /. vpHeight +. 1.0;
          let layoutMat = Data.Mat3.scaleTrans(scaleX, scaleY, transX, transY);
          Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
          /* texTransUniform */
          let texTrans = Data.Mat3.scaleTrans(1.0, 1.0, 0.0, 0.0);
          Gpu.Uniform.setMat3f(texTransUniform, texTrans);
          Js.log3("Textrans", layoutMat, texTrans);
          /* Initiate parent tex transform */
          Some({nOffX: pXOff, nOffY: pYOff, texVpWidth: inW, texVpHeight: inH});
        };
      };
    switch node.pixelSizeUniform {
    | None => ()
    | Some(pixelSizeUniform) =>
      Gpu.Uniform.setVec2f(pixelSizeUniform, Data.Vec2.make(inW, inH))
    };
    List.iter(dep => calcNodeLayout(dep, None), node.deps);
    List.iter(child => calcNodeLayout(child, parentDrawToTex), node.children);
    /* After deps and children have been processed, set texture matrices */
    Hashtbl.iter(
      (_name, nodeTex: sceneTexture) =>
        /* Textures should be scaled for right pixel ratio,
           need not be translated (could be packed maybe),
           but should start from beginning or pack position */
        /* Understand this as texture size goes to 1.0 on viewport size
           so it will depend on setting viewport before
           render to texture */
        switch (nodeTex.texNode, nodeTex.uniformMat) {
        | (Some(texNode), Some(uniform)) =>
          let texNode =
            switch self.updateNodes.data[texNode] {
            | Some(texNode) => texNode.updNode
            | None => failwith("Could not find texNode")
            };
          /* todo: Can we be sure this is available?
             It is currently resized in this traversal.
             Alternatively loop through these
             after layout + ensure layout is done */
          let (texW, texH) =
            switch (Gpu.Texture.getSize(nodeTex.texture)) {
            | Some(wh) => wh
            | None => failwith("Could not get size of texture")
            };
          let scaleX = texNode.rect.w /. vpWidth /. 2.0;
          let scaleY = texNode.rect.h /. vpHeight /. 2.0;
          let scale = Data.Mat3.scale(scaleX, scaleY);
          let translate = Data.Mat3.trans(scaleX, 1.0 -. scaleY);
          let texMat = Data.Mat3.matmul(translate, scale);
          let scaleX = texNode.rect.w /. float_of_int(texW) /. 2.0;
          let scaleY = texNode.rect.h /. float_of_int(texH) /. 2.0;
          let texMat = Data.Mat3.scaleTrans(scaleX, scaleY, scaleX, scaleY);
          Gpu.Uniform.setMat3f(uniform, texMat);
        | _ => ()
        },
      node.textures
    );
  };
  if (debug) {
    Js.log2("vpWidth", vpWidth);
    Js.log2("vpHeight", vpHeight);
  };
  calcNodeDimensions(self.root, vpWidth, vpHeight);
  self.root.calcLayout.pXOffset = 0.0;
  self.root.calcLayout.pYOffset = 0.0;
  calcNodeLayout(self.root, None);
};

/* Returns list of dep node ids */
let collectDeps = (scene, nodeIds, hiddenNodes) => {
  let rec loop = (node, list) =>
    if (List.exists(hiddenId => hiddenId == node.id, hiddenNodes)) {
      list;
    } else {
      /* Add deps */
      let list =
        List.fold_left(
          (list, dep) => [dep.id, ...list],
          list,
          node.deps
        );
      /* Traverse children */
      List.fold_left((list, child) => loop(child, list), list, node.children);
    };
  List.rev(List.fold_left(
    (list, nodeId) => {
      let node =
        switch scene.updateNodes.data[nodeId] {
        | Some(updNode) => updNode.updNode
        | None => failwith("Could not find node")
        };
      loop(node, list);
    },
    [],
    nodeIds
  ))
};

/* Checks node and parents for hidden flag
   Would feel a little better to take in
   a list of hidden nodes, on the other
   hand this is probably more efficient */
let isNodeHidden = (scene, nodeId) => {
  let rec loop = node =>
    if (node.hidden) {
      true;
    } else {
      switch node.parent {
      | Some(parent) => loop(parent)
      | None => false
      };
    };
  switch scene.updateNodes.data[nodeId] {
  | Some(node) => loop(node.updNode)
  | None => failwith("Could not find node")
  };
};

/* Creates a drawList that shows which
   areas are updating */
let update = self => {
  /* Anims */
  let rec doAnims = anims =>
    switch anims {
    | [] => []
    | [anim, ...rest] =>
      anim.elapsed = anim.elapsed +. self.canvas.deltaTime;
      if (anim.frameInterval == 1) {
        [@bs] anim.onFrame(self, anim);
      } else if (anim.numFrames mod anim.frameInterval == 0) {
        [@bs] anim.onFrame(self, anim);
      };
      anim.numFrames = anim.numFrames + 1;
      if (anim.elapsed >= anim.duration) {
        [@bs] anim.setLast(self);
        doAnims(rest);
      } else {
        [anim, ...doAnims(rest)];
      };
    };
  self.anims = doAnims(self.anims);
  /* Queued nodes, these are queued because of
     updates in uniforms, vertices or textures */
  let updateNodes =
    List.sort_uniq((a, b) => a < b ? (-1) : a > b ? 1 : 0, self.queuedUpdates);
  /* todo: Consider filtering hidden node ids.
     Those hidden may be hidden below another hidden node,
     requiring some traversal. Maybe a cache could be used,
     maybe it's not so bad as the list should be similar
     when things update anyway, and drawlist is cached */
  self.queuedUpdates = [];
  let updateState = {updateNodes, hiddenNodes: self.hiddenNodes};
  /* Debug function to ensure state is reset after processing of tree */
  let rec checkForCleanState = updNode => {
    if (updNode.update || updNode.childUpdate) {
      [%debugger];
    };
    List.iter(
      stencil =>
        if (stencil.active) {
          [%debugger];
        },
      updNode.stencils
    );
    List.iter(
      (rect: updateRect('s)) =>
        if (rect.active) {
          [%debugger];
        },
      updNode.childRects
    );
    List.iter(dep => checkForCleanState(dep), updNode.updDeps);
    List.iter(child => checkForCleanState(child), updNode.updChildren);
  };
  /* Ensure state is inited */
  let rec initDeps = (nodeId, blackList) => {
    let depIds = collectDeps(self, [nodeId], self.hiddenNodes);
    List.iter(
    depId =>
        if (! Hashtbl.mem(self.initedDeps, depId)) {
            Hashtbl.add(self.initedDeps, depId, true);
            /* Recurse for deps of this dep */
            initDeps(depId, []);
            /* If dep is in list of update nodes, let other
                drawlist include dep.
                However, this will not work for deps of deps,
                as they need to be loaded first */
            if (! List.exists(nodeId => nodeId == depId, blackList)) {
                switch self.updateNodes.data[depId] {
                | Some(depNode) =>
                let depDraws =
                    createDrawList(self, [depId], depNode.updNode);
                processDrawList(self, depDraws, false);
                switch self.drawListsDebug {
                | None => ()
                | Some(_listsDebug) =>
                    Js.log(
                    "==\nDep drawlist: "
                    ++ nodeDescrString(depNode.updNode)
                    );
                    logDrawList(self, updateState, depDraws);
                    checkForCleanState(depNode);
                };
                | None => failwith("Could not find dep node")
                };
            };
        },
    depIds
    );
  };
  /* Both here and when creating drawList, we could
     use filtered list of visible nodes,
     but this is called 60 times a second so we don't want to
     do it when uneccesary, so we keep an option around */
  let visibleNodes =
    if (! Hashtbl.mem(self.initedLists, updateState)) {
      let visibleNodes =
        List.filter(nodeId => ! isNodeHidden(self, nodeId), updateNodes);
      Hashtbl.add(self.initedLists, updateState, true);
      List.iter(nodeId => initDeps(nodeId, updateNodes), visibleNodes);
      Some(visibleNodes);
    } else {
      None;
    };
  /*
   Js.log2("== Drawing", List.fold_left((str, id) => {
       switch (self.updateNodes.data[id]) {
       | Some(node) => str ++ ", " ++ node.updNode.key
       | None => str
       }
   }, "", updateState.updateNodes));*/
  if (! Hashtbl.mem(self.drawLists, updateState)) {
    let visibleNodes =
      switch visibleNodes {
      | None =>
        List.filter(nodeId => ! isNodeHidden(self, nodeId), updateNodes)
      | Some(visibleNodes) => visibleNodes
      };
    let drawList = createDrawList(self, visibleNodes, self.root);
    Hashtbl.add(self.drawLists, updateState, drawList);
    switch self.drawListsDebug {
    | None => ()
    | Some(_listsDebug) =>
      logDrawList(self, updateState, Hashtbl.find(self.drawLists, updateState))
    };
  };
  /* Check if any node in loadingNodes is loaded */
  let (newList, loaded) =
    List.partition(node => node.loading, self.loadingNodes);
  self.loadingNodes = newList;
  if (List.length(loaded) > 0) {
    let loadedIds =
      List.map(
        node => {
          switch node.onUpdate {
          | Some(update) => [@bs] update(node, self.state)
          | None => ()
          };
          node.id;
        },
        loaded
      );
    let drawList = createDrawList(self, loadedIds, self.root);
    /* Drawing loaded nodes in a special drawlist
       The loaded node will redraw if they are
       in the main drawlist anyway, todo */
    processDrawList(self, drawList, false);
  };
  /* todo: possibly optimize with a second transformed data structure
     so the drawstate etc is readily available */
  processDrawList(self, Hashtbl.find(self.drawLists, updateState), false);
  switch self.drawListsDebug {
  | None => ()
  | Some(_) =>
    switch self.updateNodes.data[self.root.id] {
    | None => ()
    | Some(updRoot) => checkForCleanState(updRoot)
    };
    /* When debugging, we could go through list of previous
       draws and repaint with each debugNode knowing how long
       since last paint, and maybe setting alpha
       based on it */
    let context = self.canvas.context;
    Gpu.glEnable(context, Gpu.Constants.blend);
    Gpu.glBlendFunc(
      context,
      Gpu.Constants.src_alpha,
      Gpu.Constants.one_minus_src_alpha
    );
    processDrawList(self, Hashtbl.find(self.drawLists, updateState), true);
    Gpu.glDisable(context, Gpu.Constants.blend);
  };
};

let cleanUpDrawList = (scene, drawList) => {
  let rec loop = list =>
    switch list {
    | [el, ...rest] =>
      switch el {
      | DrawNode(_node) => ()
      | DrawStencil(stencilBuffers) =>
        /* Clear gpu memory */
        Gpu.VertexBuffer.deleteBuffer(
          scene.canvas.context,
          stencilBuffers.stencilVertices
        );
        Gpu.IndexBuffer.deleteBuffer(
          scene.canvas.context,
          stencilBuffers.stencilIndices
        );
      | ClearStencil => ()
      | SetRect(_rect) => ()
      | ClearRect => ()
      | BindDrawTexture(_) => ()
      | UnBindDrawTexture => ()
      };
      loop(rest);
    | [] => ()
    };
  loop(drawList);
};

let doResize = scene => {
  let (width, height) = Gpu.Canvas.getViewportSize();
  Gpu.Canvas.resize(scene.canvas, width, height);
  /* Although rects are updated through calcLayout, we
     simply reset the drawList cache for some time.
     Stencil vertices would need to be updated/reset,
     otherwise it would work I think to keep cache */
  Hashtbl.iter(
    (_, drawList) => cleanUpDrawList(scene, drawList),
    scene.drawLists
  );
  Hashtbl.clear(scene.drawLists);
  Hashtbl.clear(scene.initedLists);
  Hashtbl.clear(scene.initedDeps);
  /* Doing full layout, it would be nice to optimize
     as time goes by */
  calcLayout(scene);
  queueUpdates(scene, [scene.root.id]);
  /* Not sure how well it fits to do update, but it
     might make cleaner draw lists */
  update(scene);
};

let run =
    (width, height, setup, createScene, draw, ~keyPressed=?, ~resize=?, ()) => {
  let canvas = Gpu.Canvas.init(width, height);
  let userState = ref(setup(canvas));
  let scene = createScene(canvas, userState^);
  /* There are possibly other options for where to put this,
     if there is any need for stuff earlier, like scenes make() */
  scene.updateRoot = Some(buildUpdateTree(scene, scene.root));
  calcLayout(scene);
  createDrawStates(scene);
  /* Time for resize requested, this is throttled */
  let resizeRequested = ref(None);
  let resizeThrottle = 0.7;
  /* Start render loop */
  Gl.render(
    ~window=canvas.window,
    ~displayFunc=
      f => {
        canvas.deltaTime = f /. 1000.;
        canvas.elapsed = canvas.elapsed +. canvas.deltaTime;
        if (! scene.inited) {
          /* Create updateNodes */
          queueUpdates(scene, [scene.root.id]);
          update(scene);
          scene.inited = true;
        };
        userState := draw(userState^, scene, canvas);
        switch resizeRequested^ {
        | None => ()
        | Some(resizeTime) =>
          if (resizeTime > canvas.elapsed -. resizeThrottle) {
            resizeRequested := None;
            switch resize {
            | Some(resize) => resize(userState^)
            | None => ()
            };
            doResize(scene);
          }
        };
      },
    ~windowResize=() => resizeRequested := Some(canvas.elapsed),
    ~keyDown=
      (~keycode, ~repeat) => {
        canvas.keyboard.keyCode = keycode;
        if (! repeat) {
          switch keyPressed {
          | Some(keyPressed) => userState := keyPressed(userState^, canvas)
          | None => ()
          };
        };
      },
    ~keyUp=
      (~keycode) =>
        (),
          /* Need this to trigger cleaning of keyes pressed
             and repeat marked */
    ()
  );
};

let doAnim = (scene, anim) => scene.anims = [anim, ...scene.anims];

module SVertexObject = {
  let make = (vertexBuffer, indexBuffer) : sceneVertexObject => {
    nodes: [],
    vertexBuffer,
    indexBuffer
  };
  let makeQuad = (~usage=Gpu.StaticDraw, ()) => {
    let vBuffer = Gpu.VertexBuffer.makeQuad(~usage, ());
    let iBuffer =
      Some(Gpu.IndexBuffer.make(Gpu.IndexBuffer.makeQuadsData(1), usage));
    make(vBuffer, iBuffer);
  };
  let updateQuads = (scene, self, vertices) => {
    Gpu.VertexBuffer.setDataT(self.vertexBuffer, vertices);
    let perElement = self.vertexBuffer.perElement * 4;
    switch self.indexBuffer {
    | Some(indices) =>
      Gpu.IndexBuffer.setDataT(
        indices,
        Gpu.IndexBuffer.makeQuadsData(Array.length(vertices) / perElement)
      )
    | None => ()
    };
    queueUpdates(scene, self.nodes);
  };
};

let makeSceneUniform = uniform : sceneUniform => {nodes: [], uniform};

module UFloat = {
  let make = value => makeSceneUniform(Gpu.UniformFloat(ref(value)));
  let zero = () => makeSceneUniform(Gpu.UniformFloat(ref(0.0)));
  let one = () => makeSceneUniform(Gpu.UniformFloat(ref(1.0)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setFloat(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
};

module UVec2f = {
  let zeros = () => makeSceneUniform(Gpu.UniformVec2f(ref(Data.Vec2.zeros())));
  let vals = (a, b) =>
    makeSceneUniform(Gpu.UniformVec2f(ref(Data.Vec2.make(a, b))));
  let fromArray = arr =>
    makeSceneUniform(Gpu.UniformVec2f(ref(Data.Vec2.fromArray(arr))));
  let set = (scene, self, v) => {
    Gpu.Uniform.setVec2f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
  let setArr = (scene, self, arr) => {
    Gpu.Uniform.setVec2f(self.uniform, Data.Vec2.fromArray(arr));
    queueUpdates(scene, self.nodes);
  };
  let get = self =>
    switch self.uniform {
    | Gpu.UniformVec2f(v) => v^
    | _ => failwith("Could not get vec2 uniform")
    };
};

module UVec3f = {
  let zeros = () => makeSceneUniform(Gpu.UniformVec3f(ref(Data.Vec3.zeros())));
  let vals = (a, b, c) =>
    makeSceneUniform(Gpu.UniformVec3f(ref(Data.Vec3.make(a, b, c))));
  let fromArray = arr =>
    makeSceneUniform(Gpu.UniformVec3f(ref(Data.Vec3.fromArray(arr))));
  let vec = v => makeSceneUniform(Gpu.UniformVec3f(ref(v)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setVec3f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
  let setArr = (scene, self, arr) => {
    Gpu.Uniform.setVec3f(self.uniform, Data.Vec3.fromArray(arr));
    queueUpdates(scene, self.nodes);
  };
  let setQuiet = (self, v) => Gpu.Uniform.setVec3f(self.uniform, v);
  let get = self =>
    switch self.uniform {
    | Gpu.UniformVec3f(v) => v^
    | _ => failwith("Could not get vec3 uniform")
    };
};

module UVec4f = {
  let zeros = () => makeSceneUniform(Gpu.UniformVec4f(ref(Data.Vec4.zeros())));
  let vals = (a, b, c, d) =>
    makeSceneUniform(Gpu.UniformVec4f(ref(Data.Vec4.make(a, b, c, d))));
  let fromArray = arr =>
    makeSceneUniform(Gpu.UniformVec4f(ref(Data.Vec4.fromArray(arr))));
  let set = (scene, self, v) => {
    Gpu.Uniform.setVec4f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
  let setArr = (scene, self, arr) => {
    Gpu.Uniform.setVec4f(self.uniform, Data.Vec4.fromArray(arr));
    queueUpdates(scene, self.nodes);
  };
};

module UMat3f = {
  let id = () => makeSceneUniform(Gpu.UniformMat3f(ref(Data.Mat3.id())));
  let mat = mat => makeSceneUniform(Gpu.UniformMat3f(ref(mat)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setMat3f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
};

/* These finds the uniform in node first,
   maybe reorganize someway */
let setUniformFloat = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UFloat.set(scene, uniform, value);
};

let setUniformVec2f = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UVec2f.set(scene, uniform, value);
};

let setUniformVec3f = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UVec3f.set(scene, uniform, value);
};

let setUniformMat3f = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UMat3f.set(scene, uniform, value);
};
