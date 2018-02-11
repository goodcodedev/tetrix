type lightColor =
  | StaticColor(Color.t)
  | DynamicColor(Gpu.uniform);

type lightPos =
  | StaticPos(Data.Vec3.t)
  | DynamicPos(Gpu.uniform);

type lightDir =
  | StaticDir(Data.Vec3.t)
  | DynamicDir(Gpu.uniform);

type coordSystem =
  | ScreenCoords
  | LocalCoords;

module Specular = {
    type t = {
        power: int,
        camera: Camera.t
    };

    let make = (~power=16, camera) => {
        {
            power,
            camera
        }
    };
};

module PointLight = {
    type t = {
        pos: lightPos,
        color: lightColor,
        coords: coordSystem,
        factor: float
    };

    let make = (
        ~pos,
        ~color=StaticColor(Color.white()),
        ~coords=ScreenCoords,
        ~factor=0.6,
        ()
    ) => {
        {
            pos,
            color,
            coords,
            factor
        }
    };

    let getUniforms = (self, i) => {
        let istr = string_of_int(i);
        let uniforms = switch (self.pos) {
        | DynamicPos(u) => [("uPointPos" ++ istr, u)]
        | _ => []
        };
        let uniforms = switch (self.color) {
        | DynamicColor(u) => [("uPointColor" ++ istr, u), ...uniforms]
        | _ => uniforms
        };
        uniforms
    };

    let getFragVarDecls = (self, i) => {
        let istr = string_of_int(i);
        let dir = switch (self.pos) {
        | DynamicPos(_) => "uniform vec3 uPointPos" ++ istr ++ ";\n"
        | _ => ""
        };
        let color = switch (self.color) {
        | DynamicColor(_) => "uniform vec3 uPointColor" ++ istr ++ ";\n"
        | StaticColor(_) => ""
        };
        dir ++ color
    };

    let getLightFuncSource = (self, i) => {
        let istr = string_of_int(i);
        let lightPoint = switch (self.pos) {
        | StaticPos(v) => Data.Vec3.toGlsl(v)
        | DynamicPos(_) => "uPointPos" ++ istr
        };
        let point = switch (self.coords) {
        | ScreenCoords => "screenP"
        | LocalCoords => "localP"
        };
        let pointDir = "normalize(" ++ lightPoint ++ " - " ++ point ++ ")";
        let dot = "max(dot(" ++ pointDir ++ ", normal), 0.0) * "
                        ++ string_of_float(self.factor);
        dot
    };
};

module Directional = {
    type t = {
        dir: lightDir,
        color: lightColor,
        coords: coordSystem,
        factor: float
    };

    let make = (
        ~dir,
        ~color=StaticColor(Color.white()),
        ~coords=ScreenCoords,
        ~factor=0.4,
        ()
    ) => {
        {
            dir,
            color,
            coords,
            factor
        }
    };

    let getUniforms = (self) => {
        let uniforms = switch (self.dir) {
        | DynamicDir(u) => [("uDir", u)]
        | StaticDir(_) => []
        };
        let uniforms = switch (self.color) {
        | DynamicColor(u) => [("uDirColor", u)]
        | StaticColor(_) => uniforms
        };
        uniforms
    };

    let getFragVarDecls = (self) => {
        let dir = switch (self.dir) {
        | DynamicDir(_) => "uniform vec3 uDir;\n"
        | StaticDir(_) => ""
        };
        let color = switch (self.color) {
        | DynamicColor(_) => "uniform vec3 uDirColor;\n"
        | StaticColor(_) => ""
        };
        dir ++ color
    };

    let getLightFuncSource = (self) => {
        let dir = switch (self.dir) {
        | StaticDir(v) => Data.Vec3.toGlsl(v)
        | DynamicDir(_) => "uDir"
        };
        let dot = "max(dot(" ++ dir ++ ", normal), 0.0) * "
                        ++ string_of_float(self.factor);
        dot
    };
};

module ProgramLight = {
    type t = {
        dir: Directional.t,
        points: list(PointLight.t),
        specular: Specular.t
    };

    let make = (dir, points, specular) => {
        {
            dir,
            points,
            specular
        }
    };

    let default = () => {
        let dirLight = Directional.make(~dir=StaticDir(Data.Vec3.make(0.4, 0.3, 0.3)), ());
        let pointLight = PointLight.make(~pos=StaticPos(Data.Vec3.make(0.0, -0.4, 2.0)), ());
        let camera = Camera.make(Data.Vec3.make(0.0, 0.4, 4.0));
        make(
            dirLight,
            [pointLight],
            Specular.make(camera)
        )
    };

    let getUniforms = (self) => {
        let uniforms = List.concat(List.mapi((i, p) => {
            PointLight.getUniforms(p, i)
        }, self.points));
        let uniforms = List.append(Directional.getUniforms(self.dir), uniforms);
        uniforms
    };

    let getVertVarDecls = (self) => {
        ()
    };

    let getFragVarDecls = (self) => {
        let (_, decls) = List.fold_left(((i, decls), p) => {
            (i + 1, decls ++ PointLight.getFragVarDecls(p, i))
        }, (0, ""), self.points);
        Directional.getFragVarDecls(self.dir) ++ decls;
    };

    let getLightFunction = (self) => {
        /* Bit simplistic now, expecting expressions
           from the parts */
        let (_, addends) = List.fold_left(((i, addends), p) => {
            (i + 1, [PointLight.getLightFuncSource(p, i), ...addends])
        }, (0, []), self.points);
        let addends = [Directional.getLightFuncSource(self.dir), ...addends];
        "float lighting(vec3 localP, vec3 screenP, vec3 normal) {\n"
        ++ "float c = " ++ String.concat(" + ", addends) ++ ";\n"
        ++ "return c;\n"
        ++ "}\n"
    };
};