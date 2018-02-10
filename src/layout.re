/* Creates a layout node */
let vertical = (
    ~key="verticalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    ~margin=?,
    children
) => {
    Scene.makeNode(
        key,
        ~size,
        ~spacing=?spacing,
        ~margin=?margin,
        ~childLayout=Scene.Vertical,
        ~selfDraw=false,
        ~children,
        ()
    )
};

let horizontal = (
    ~key="horizontalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    children
) => {
    Scene.makeNode(
        key,
        ~size,
        ~spacing=?spacing,
        ~childLayout=Scene.Horizontal,
        ~selfDraw=false,
        ~children,
        ()
    )
};

let stacked = (
    ~key="stackedLayout",
    ~size=Scene.Aspect(1.0),
    children
) => {
    Scene.makeNode(
        key,
        ~size,
        ~childLayout=Scene.Stacked,
        ~selfDraw=false,
        ~children,
        ()
    )
};