/* Creates a layout node */
let vertical = (
    ~key="verticalLayout",
    ~width=1.0,
    ~height=1.0,
    ~spacing=?,
    children
) => {
    Scene.makeNode(
        key,
        ~size=Dimensions(Scale(width), Scale(height)),
        ~spacing=?spacing,
        ~childLayout=Scene.Vertical,
        ~selfDraw=false,
        ~children,
        ()
    )
};