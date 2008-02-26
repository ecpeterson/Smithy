let movestep = 0.1
let rotstep  = 1.0

class visualmode (ar: GlGtk.area) = object (self)
    val mutable map = new MapFormat.map
    method set_map x = map <- x

    val mutable pos = (0.0, 0.0, 0.0)
    val mutable orientation = (0.0, 0.0, 0.0)

    method init () =
        GlMat.mode `projection;
        GlMat.load_identity ();
        GluMat.perspective ~fovy:60.0 ~aspect:(4.0 /. 3.0) ~z:(0.25, 20.0);
        GlMat.mode `modelview;
        GlMat.load_identity ();
        (*GlLight.light ~num:0 (`position (1.0, 1.0, 3.0, 0.0));*)
        (*List.iter Gl.enable [`depth_test; `lighting; `light0];*)
        (*GlLight.material ~face:`front (`ambient_and_diffuse (0.8, 0.8, 0.8, 1.0));*)
        (*Gl.enable `cull_face;*)
        (*GlDraw.cull_face `front;*)
        ()

    method set_camera () =
        let (x, y, z) = pos in
        let (pitch, yaw, roll) = orientation in
        GlMat.load_identity ();
        GlMat.rotate ~x:0.0 ~y:0.0 ~z:1.0 ~angle:roll ();
        GlMat.rotate ~x:0.0 ~y:1.0 ~z:0.0 ~angle:yaw ();
        GlMat.rotate ~x:1.0 ~y:0.0 ~z:0.0 ~angle:pitch ();
        GlMat.translate ~x ~y ~z ()

    method display () =
        (* clear the display *)
        GlClear.color (0.0, 0.0, 0.0);
        GlClear.clear [`color; `depth];
        (* set up the camera *)
        self#set_camera ();
        (* draw the polys *)
        GlDraw.begins `triangles;
            GlDraw.color (1.0, 0.0, 0.0);
            GlDraw.vertex3 (1.0, 0.0, -1.0);
            GlDraw.color (0.0, 1.0, 0.0);
            GlDraw.vertex3 (0.0, 1.0, -1.0);
            GlDraw.color (0.0, 0.0, 1.0);
            GlDraw.vertex3 (0.0, 0.0, -1.0);
        GlDraw.ends ();
        (* swap the buffers *)
        Gl.flush ();
        ar#swap_buffers ()
        
    method send_key in_key =
        let (x, y, z) = pos in
        let (pitch, yaw, roll) = orientation in
        let pi = atan(1.0) *. 4.0 in
        let syaw, cyaw = sin(yaw *. pi /. 180.0), cos(yaw *. pi /. 180.0) in
        begin match in_key with
            |119 -> pos <- (x -. movestep *. syaw, y, z +. movestep *. cyaw)
            |100 -> pos <- (x -. movestep *. cyaw, y, z -. movestep *. syaw)
            |115 -> pos <- (x +. movestep *. syaw, y, z -. movestep *. cyaw)
            |97  -> pos <- (x +. movestep *. cyaw, y, z +. movestep *. syaw)
            |65362 -> orientation <- (pitch -. rotstep, yaw, roll)
            |65363 -> orientation <- (pitch, yaw +. rotstep, roll)
            |65364 -> orientation <- (pitch +. rotstep, yaw, roll)
            |65361 -> orientation <- (pitch, yaw -. rotstep, roll)
            |_ -> () end;
        self#display ();
        false
end
