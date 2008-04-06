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
        (*GlLight.material ~face:`front (`ambient_and_diffuse (0.8, 0.8, 0.8, 1.0));*)
        List.iter Gl.enable [(*`cull_face; `lighting; `light0;*) `depth_test];
        (*GlDraw.cull_face `back;*)
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
            (* draw ceiling *)
            let glarray = Array.make (3*(poly#vertex_count ())) 0.0 in
            for i = 0 to poly#vertex_count () - 1 do
                let (x0, y0) = points.((poly#endpoint_indices ()).(i))#vertex () in
                glarray.(3*i) <- float x0;
                glarray.(3*i + 1) <- poly#ceiling_height () *. 1024.0;
                glarray.(3*i + 2) <- float y0;
            done;
            let raw = Raw.of_float_array glarray `double in
            GlArray.enable `vertex;
            GlArray.vertex `three raw;
            GlDraw.color Colors.ceiling_fill_color;
            GlArray.draw_arrays `polygon 0 (poly#vertex_count ());
            GlDraw.color Colors.ceiling_color;
            GlArray.draw_arrays `line_loop 0 (poly#vertex_count ());
            (* draw floor *)
            let glarray = Array.make (3*(poly#vertex_count ())) 0.0 in
            for i = 0 to poly#vertex_count () - 1 do
                let (x0, y0) = points.((poly#endpoint_indices ()).(i))#vertex () in
                glarray.(3*i) <- float x0;
                glarray.(3*i + 1) <- poly#floor_height () *. 1024.0;
                glarray.(3*i + 2) <- float y0;
            done;
            let raw = Raw.of_float_array glarray `double in
            GlArray.enable `vertex;
            GlArray.vertex `three raw;
            GlDraw.color Colors.floor_fill_color;
            GlArray.draw_arrays `polygon 0 (poly#vertex_count ());
            GlDraw.color Colors.floor_color;
            GlArray.draw_arrays `line_loop 0 (poly#vertex_count ()))
            (map#get_polygons_array ());
        GlDraw.color Colors.wall_fill_color;
        Array.iter (fun line ->
            let (cw_side, ccw_side) =
                line#cw_poly_side_index (), line#ccw_poly_side_index () in
            let (cw_poly, ccw_poly) =
                line#cw_poly_owner (), line#ccw_poly_owner () in
            let (p0, p1) = line#endpoints () in
            let p0x, p0y = (map#get_points_array ()).(p0)#vertex () in
            let p1x, p1y = (map#get_points_array ()).(p1)#vertex () in
            if cw_poly != -1 && ccw_poly != -1 then begin
                (* ceiling to ceiling, floor to floor *)
                let cw_poly = (map#get_polygons_array ()).(cw_poly) in
                let ccw_poly = (map#get_polygons_array ()).(ccw_poly) in
                GlDraw.begins `quads;
                    GlDraw.vertex3 (float p0x, cw_poly#floor_height () *. 1024.0, float p0y);
                    GlDraw.vertex3 (float p0x, ccw_poly#floor_height () *. 1024.0, float p0y);
                    GlDraw.vertex3 (float p1x, ccw_poly#floor_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p1x, cw_poly#floor_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p0x, cw_poly#ceiling_height () *. 1024.0, float p0y);
                    GlDraw.vertex3 (float p0x, ccw_poly#ceiling_height () *. 1024.0, float p0y);
                    GlDraw.vertex3 (float p1x, ccw_poly#ceiling_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p1x, cw_poly#ceiling_height () *. 1024.0, float p1y);
                GlDraw.ends ();
            end else if cw_poly != -1 then begin
                (* cw_poly gets full side *)
                let cw_poly = (map#get_polygons_array ()).(cw_poly) in
                GlDraw.begins `quads;
                    GlDraw.vertex3 (float p0x, cw_poly#floor_height () *. 1024.0, float p0y);
                    GlDraw.vertex3 (float p1x, cw_poly#floor_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p1x, cw_poly#ceiling_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p0x, cw_poly#ceiling_height () *. 1024.0, float p0y);
                GlDraw.ends ();
            end else if ccw_poly != -1 then begin
                (* ccw_poly_gets full side *)
                let ccw_poly = (map#get_polygons_array ()).(ccw_poly) in
                GlDraw.begins `quads;
                    GlDraw.vertex3 (float p0x, ccw_poly#floor_height () *. 1024.0, float p0y);
                    GlDraw.vertex3 (float p1x, ccw_poly#floor_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p1x, ccw_poly#ceiling_height () *. 1024.0, float p1y);
                    GlDraw.vertex3 (float p0x, ccw_poly#ceiling_height () *. 1024.0, float p0y);
                GlDraw.ends ();
            end)
            (map#get_lines_array ());
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
