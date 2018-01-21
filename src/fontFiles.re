type t = {
    bin: array(int),
    image: Reasongl.Gl.imageT
};

let request = (fontName, callback) => {
    open AjaxLoader;
    loadFiles(
        [
            BinaryAsset("fonts/" ++ fontName ++ "-sdf.bin"),
            ImageAsset("fonts/" ++ fontName ++ "-sdf.png")
        ],
        (results) => {
            switch (results[0], results[1]) {
            | (AjaxBinaryResult(buffer), AjaxImageResult(image)) =>
                let fontFiles = {
                    bin: buffer,
                    image
                };
                callback(fontFiles);
            | _ => failwith("Font loading failed");
            }
        }
    );
};