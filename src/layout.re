/* Creates a layout node */
let vertical = (
    ~key=?,
    ~cls="verticalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    ~margin=?,
    ~padding=?,
    ~hidden=false,
    ~hAlign=Scene.AlignCenter,
    ~vAlign=Scene.AlignMiddle,
    children
) => {
    Scene.makeNode(
        ~key=?key,
        ~cls,
        ~size,
        ~spacing=?spacing,
        ~margin=?margin,
        ~padding=?padding,
        ~childLayout=Scene.Vertical,
        ~hAlign,
        ~vAlign,
        ~selfDraw=false,
        ~children,
        ~hidden,
        ()
    )
};

let horizontal = (
    ~key=?,
    ~cls="horizontalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    ~hidden=false,
    children
) => {
    Scene.makeNode(
        ~key=?key,
        ~cls,
        ~size,
        ~spacing=?spacing,
        ~childLayout=Scene.Horizontal,
        ~selfDraw=false,
        ~hidden,
        ~children,
        ()
    )
};

let stacked = (
    ~key=?,
    ~cls="stackedLayout",
    ~size=Scene.Aspect(1.0),
    ~hidden=false,
    ~hAlign=Scene.AlignCenter,
    ~vAlign=Scene.AlignTop,
    children
) => {
    Scene.makeNode(
        ~key=?key,
        ~cls,
        ~size,
        ~childLayout=Scene.Stacked,
        ~selfDraw=false,
        ~hidden,
        ~hAlign,
        ~vAlign,
        ~children,
        ()
    )
};