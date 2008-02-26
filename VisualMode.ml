let movestep = 100.0
let rotstep  = 1.0

class visualmode (ar: GlGtk.area) = object (self)
    val mutable map = new MapFormat.map
    method set_map x = map <- x

    val mutable pos = (0.0, -512.0, 0.0)
    val mutable orientation = (0.0, 0.0, 0.0)

    method init () =
        GlMat.mode `projection;
        GlMat.load_identity ();
        GluMat.perspective ~fovy:60.0 ~aspect:(4.0 /. 3.0) ~z:(100.0, 30000.0);
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
        GlMat.rotate ~x:1.0 ~y:0.0 ~z:0.0 ~angle:pitch ();
        GlMat.rotate ~x:0.0 ~y:1.0 ~z:0.0 ~angle:yaw ();
        GlMat.translate ~x ~y ~z ()

    method display () =
        (* clear the display *)
        GlClear.color (0.0, 0.0, 0.0);
        GlClear.clear [`color; `depth];
        (* set up the camera *)
        self#set_camera ();
        (* draw the polys *)
        let points = map#get_points_array () in
        Array.iter (fun poly ->
            (* draw floor *)
            GlDraw.color Colors.floor_color;
            GlDraw.begins `line_loop;
            Array.iter (fun x ->
                let (x, y) = points.(x)#vertex () in
                GlDraw.vertex3 (float_of_int x,
                                poly#floor_height () *. 1024.0,
                                float_of_int y))
                    (Array.sub (poly#endpoint_indices ()) 0 (poly#vertex_count ()));
            GlDraw.ends ();
            (* draw ceiling *)
            GlDraw.color Colors.ceiling_color;
            GlDraw.begins `line_loop;
            Array.iter (fun x ->
                let (x, y) = points.(x)#vertex () in
                GlDraw.vertex3 (float_of_int x,
                                poly#ceiling_height () *. 1024.0,
                                float_of_int y))
                    (Array.sub (poly#endpoint_indices ()) 0 (poly#vertex_count ()));
            GlDraw.ends ();
            (* draw walls *)
            GlDraw.color Colors.wall_color;
            GlDraw.begins `lines;
            Array.iter (fun x ->
                let (p0, p1) = (map#get_lines_array ()).(x)#endpoints () in
                let (p0x, p0y), (p1x, p1y) = points.(p0)#vertex (),
                                             points.(p1)#vertex () in
                GlDraw.vertex3 (float_of_int p0x,
                                poly#floor_height () *. 1024.0,
                                float_of_int p0y);
                GlDraw.vertex3 (float_of_int p0x,
                                poly#ceiling_height () *. 1024.0,
                                float_of_int p0y);
                GlDraw.vertex3 (float_of_int p1x,
                                poly#ceiling_height () *. 1024.0,
                                float_of_int p1y);
                GlDraw.vertex3 (float_of_int p1x,
                                poly#floor_height () *. 1024.0,
                                float_of_int p1y))
                (Array.sub (poly#line_indices ()) 0 (poly#vertex_count ()));
            GlDraw.ends ()) (map#get_polygons_array ());
        (* draw the basis *)
        GlDraw.begins `lines;
            GlDraw.color (1.0, 0.0, 0.0);
            GlDraw.vertex3 (0.0, 0.0, 0.0);
            GlDraw.vertex3 (1024.0, 0.0, 0.0);
            GlDraw.color (0.0, 1.0, 0.0);
            GlDraw.vertex3 (0.0, 0.0, 0.0);
            GlDraw.vertex3 (0.0, 1024.0, 0.0);
            GlDraw.color (0.0, 0.0, 1.0);
            GlDraw.vertex3 (0.0, 0.0, 0.0);
            GlDraw.vertex3 (0.0, 0.0, 1024.0);
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
            (* move forward *)
            |119 -> pos <- (x -. movestep *. syaw, y, z +. movestep *. cyaw)
            (* strafe right *)
            |100 -> pos <- (x -. movestep *. cyaw, y, z -. movestep *. syaw)
            (* move backward *)
            |115 -> pos <- (x +. movestep *. syaw, y, z -. movestep *. cyaw)
            (* strafe left *)
            |97  -> pos <- (x +. movestep *. cyaw, y, z +. movestep *. syaw)
            (* move up *)
            |39  -> pos <- (x, y -. movestep, z)
            (* move down *)
            |47  -> pos <- (x, y +. movestep, z)
            (* rotate up *)
            |65362 -> orientation <- (pitch -. rotstep, yaw, roll)
            (* rotate right *)
            |65363 -> orientation <- (pitch, yaw +. rotstep, roll)
            (* rotate down *)
            |65364 -> orientation <- (pitch +. rotstep, yaw, roll)
            (* right left *)
            |65361 -> orientation <- (pitch, yaw -. rotstep, roll)
            (* fuck you, user *)
            |_ -> print_endline ("uncaptured key: " ^ (string_of_int in_key))
        end;
        self#display ();
        false
end
