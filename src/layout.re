/* Creates a layout node */
let vertical = (
    ~key="verticalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    ~margin=?,
    ~hidden=false,
    ~hAlign=Scene.AlignCenter,
    ~vAlign=Scene.AlignMiddle,
    children
) => {
    Scene.makeNode(
        key,
        ~size,
        ~spacing=?spacing,
        ~margin=?margin,
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
    ~key="horizontalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    ~hidden=false,
    children
) => {
    Scene.makeNode(
        key,
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
    ~key="stackedLayout",
    ~size=Scene.Aspect(1.0),
    ~hidden=false,
    ~hAlign=Scene.AlignCenter,
    ~vAlign=Scene.AlignTop,
    children
) => {
    Scene.makeNode(
        key,
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