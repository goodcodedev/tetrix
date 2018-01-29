/* Creates a layout node */
let vertical = (
    ~key="verticalLayout",
    ~size=Scene.Aspect(1.0),
    ~spacing=?,
    children
) => {
    Scene.makeNode(
        key,
        ~size,
        ~spacing=?spacing,
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