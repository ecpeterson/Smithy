class visualmode (ar: GlGtk.area) = object (self)
    val mutable pos = (0.0, 0.0, 0.0)
    val mutable orientation = (0.0, 0.0, 0.0)

    method init () =
        GlMat.mode `projection;
        GlMat.load_identity ();
        GluMat.perspective ~fovy:60.0 ~aspect:(4.0/.3.0) ~z:(0.24, 20.0);
        GlMat.mode `modelview;
        GlMat.load_identity ();
        GlLight.light ~num:0 (`position (1.0, 1.0, 3.0, 0.0));
        List.iter Gl.enable [`depth_test; `lighting; `light0];
        GlLight.material ~face:`front (`ambient_and_diffuse (0.8, 0.8, 0.8, 1.0));
        (* Gl.enable `cull_face;
         * GlDraw.cull_face `front *)

    method set_camera () =
        let (x, y, z) = pos in
        let (pitch, yaw, roll) = orientation in
        GlMat.load_identity ();
        GlMat.translate ~x ~y ~z ();
        GlMat.rotate ~x:1.0 ~y:0.0 ~z:0.0 ~angle:pitch ();
        GlMat.rotate ~x:0.0 ~y:1.0 ~z:0.0 ~angle:yaw ();
        GlMat.rotate ~x:0.0 ~y:0.0 ~z:1.0 ~angle:roll ()

    method display () =
        (* clear the display *)
        GlClear.color (0.0, 0.0, 0.0);
        GlClear.clear [`color; `depth];
        (* set up the camera *)
        self#set_camera ();
        (* draw the polys *)
        GlDraw.begins `triangles;
            GlDraw.vertex3 (1.0, 0.0, 1.0);
            GlDraw.vertex3 (0.0, 0.0, 1.0);
            GlDraw.vertex3 (0.0, 0.0, 0.0);
        GlDraw.ends ();
        (* swap the buffers *)
        Gl.flush ();
        ar#swap_buffers ()
        
    method send_key in_key =
        let (x, y, z) = pos in
        let (pitch, yaw, roll) = orientation in
        begin match in_key with
            |65362 -> pos <- (x +. 1.0, y, z)
            |65363 -> pos <- (x, y +. 1.0, z)
            |65364 -> pos <- (x -. 1.0, y, z)
            |65361 -> pos <- (x, y -. 1.0, z)
            |119 -> orientation <- (pitch +. 1.0, yaw, roll)
            |100 -> orientation <- (pitch, yaw +. 1.0, roll)
            |115 -> orientation <- (pitch -. 1.0, yaw, roll)
            |97  -> orientation <- (pitch, yaw -. 1.0, roll)
            |_ -> () end;
        Printf.printf "%f %f %f, %f %f %f" x y z pitch yaw roll
end
