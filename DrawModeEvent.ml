open DrawModeWindows

(* an event utility, note that the 8.0 is actually configurable *)
let highlight_distance () =
    let pixel_epsilon = 8.0 in
    pixel_epsilon /. (orthodrawer#scale ())

let resize_callback width height =
    let upper = float MapFormat.half_map_width -.
                float width /. orthodrawer#scale () in
    hadj#set_bounds ~upper ();
    let upper = float MapFormat.half_map_width -.
                float height /. orthodrawer#scale () in
    vadj#set_bounds ~upper ()

(* this gets called when the scrollbar values change *)
let slider_callback _ =
    orthodrawer#set_origin (int_of_float hadj#value, int_of_float vadj#value);
    orthodrawer#draw ()

let scroll_callback dx dy =
    hadj#set_value (hadj#value +. (dx *. 20.0 /. orthodrawer#scale ()));
    vadj#set_value (vadj#value +. (dy *. 20.0 /. orthodrawer#scale ()))

(* this gets called when we start applying a tool *)
let tool_begin_event x y button (state: Gdk.Tags.modifier list) =
    let x, y = float x, float y in
    (* unwrap values actually useful to us *)
    let tool = active_tool () in
    (* get nearby objects, since tools frequently need them *)
    let (point_d, point_i) = MapFormat.get_closest_point x y in
    let (line_d, line_i) = MapFormat.get_closest_line x y in
    let poly = MapFormat.get_enclosing_poly x y in
    let (obj_d, obj_i) = MapFormat.get_closest_object x y in
    (* biiiiig switch statement that selects what exactly we want to be doing
     * with this mouse click *)
    let int_entry = try Some (int_of_string numeric_entry#text)
                    with _ -> None in
    let float_entry = try Some (float_of_string numeric_entry#text)
                      with _ -> None in
    begin match (!mode, button, int_entry,
                 float_entry, poly) with
    (* a bunch of these get and set media/light/height/whatever attributes *)
    |Lights_Liquid, 1, Some v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_media_lightsource v
    |Lights_Liquid, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#media_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |Lights_Ceiling, 1, Some v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_ceiling_lightsource v
    |Lights_Ceiling, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#ceiling_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |Lights_Floor, 1, Some v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_floor_lightsource v
    |Lights_Floor, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#floor_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |Elevation_Ceiling, 1, _, Some v, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_ceiling_height v
    |Elevation_Ceiling, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#ceiling_height () in
        numeric_entry#set_text (string_of_float v)
    |Elevation_Floor, 1, _, Some v, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_floor_height v
    |Elevation_Floor, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#floor_height () in
        numeric_entry#set_text (string_of_float v)
    |Liquids, 1, Some v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_media_index v
    |Liquids, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#media_index () in
        numeric_entry#set_text (string_of_int v)
    |Draw_Mode, 1, _, _, _ ->
        (* in draw mode, we have to deal with what kind of tool to apply *)
        if tool = buttonarrow then
            (* see TODO list about things pertaining to this that need to be
             * fixed; there are quite a few. *)
            match List.mem `SHIFT state, !highlight with
            (* the arrow tool selects things on mouse down, and multiple things
             * when shift is being held *)
            |true, Point n when point_d < highlight_distance () ->
                    highlight := Point (point_i :: n)
            |_ when point_d < highlight_distance () ->
                    highlight := Point [point_i]
            |true, Object n when obj_d < highlight_distance () ->
                    highlight := Object (obj_i :: n)
            |_ when obj_d < highlight_distance () ->
                    highlight := Object [obj_i]
            |true, Line n when line_d < highlight_distance () ->
                    highlight := Line (line_i :: n)
            |_ when line_d < highlight_distance () ->
                    highlight := Line [line_i]
            |true, Poly m when poly <> None ->
                    let Some n = poly in
                    highlight := Poly (n :: m)
            |_ when poly <> None ->
                    let Some n = poly in
                    highlight := Poly [n]
            |_ ->
                    highlight := No_Highlight
        (* the line tool draws lines *)
        else if tool = buttonline then
            GeomEdit.start_line x y (highlight_distance ())
        (* and the fill tool fills line loops with polygons *)
        else if tool = buttonfill then
            GeomEdit.fill_poly (int_of_float x) (int_of_float y)
        else ()
    |Draw_Mode, 3, _, _, _ ->
        if tool = buttonarrow then
            (* a right-click in draw mode with the arrow tool means we want to
             * inspect a map element *)
            if point_d < highlight_distance () then
                MapDialogs.point_dialog !MapFormat.points.(point_i)
            else if obj_d < highlight_distance () then
                MapDialogs.obj_dialog !MapFormat.objs.(obj_i)
            else if line_d < highlight_distance () then (
                MapDialogs.line_dialog !MapFormat.lines.(line_i)
            ) else if poly <> None then let Some n = poly in (
                MapDialogs.poly_dialog !MapFormat.polygons.(n))
        else ()
    |_ -> () end

(* this gets called when we're dragging a tool around *)
let tool_in_event x0 y0 old_x old_y x y =
    let x0, y0, old_x, old_y, x, y = float x0, float y0, float old_x,
                                     float old_y, float x, float y in
    (* extract values actually useful to us *)
    let tool = active_tool () in
    begin match !mode with
    |Draw_Mode ->
        (* if we're panning, then pan *)
        if tool = buttonpan then
            let horig = DrawModeWindows.hadj#value +. x0 in
            let vorig = DrawModeWindows.vadj#value +. y0 in
            DrawModeWindows.hadj#set_value (horig -. x);
            DrawModeWindows.vadj#set_value (vorig -. y)
            (* if we're using the arrow, drag *)
        else if tool = buttonarrow then
            let delta_x = (x -. old_x) in
            let delta_y = (y -. old_y) in
            (* utilities for easy dragging *)
            let shift_point p =
                let point = !MapFormat.points.(p) in
                let (px, py) = point#vertex () in
                point#set_vertex (int_of_float (float px +. delta_x),
                int_of_float (float py +. delta_y));
                MapFormat.recalculate_lengths p in
            let shift_obj obj =
                let (x, y, z) = obj#point () in
                obj#set_point (int_of_float (float x +. delta_x),
                int_of_float (float y +. delta_y), z) in
                begin match !highlight with
                    |Point ns ->
                        List.iter shift_point ns
                    |Line ns ->
                        let points = List.fold_left (fun x y ->
                            let p0, p1 = !MapFormat.lines.(y)#endpoints () in
                            p0 :: p1 :: x) [] ns in
                        List.iter shift_point (CamlExt.nub points)
                    |Poly ns ->
                        let points = List.fold_left (fun x y ->
                            let poly = !MapFormat.polygons.(y) in
                            let points = poly#endpoint_indices () in
                            let points = Array.sub points 0
                                (poly#vertex_count ()) in
                            Array.to_list points @ x) [] ns in
                        List.iter shift_point (CamlExt.nub points)
                    |Object ns ->
                        List.iter (fun n ->
                            shift_obj !MapFormat.objs.(n)) ns
                    |_ -> () end
            else if tool = buttonline then
                GeomEdit.intermediate_line x y
            else ()
        |_ -> () end

(* and this gets called when we release the mouse button and apply the tool *)
let tool_end_event x0 y0 x y (button: int) _ =
    let x0, y0, x, y = float x0, float y0, float x, float y in
    let tool = active_tool () in
    begin match !mode with
    |Draw_Mode ->
        if tool = buttonarrow then
            (* if we were dragging around objects, be sure to place them in the
             * appropriate polygon! *)
            match !highlight with
                |Object n ->
                    List.iter (fun n ->
                        let obj = !MapFormat.objs.(n) in
                        let (x, y, _) = obj#point () in
                        let poly = MapFormat.get_enclosing_poly
                                                    (float x) (float y) in
                        match poly with
                            |None -> MapFormat.delete_obj n
                            |Some a -> obj#set_polygon a) n
                |_ -> ()
        else if tool = buttonzoom then
            (* if we were using the zoom tool, apply the zoom *)
            match button with
            |1 -> zoom_at 2.0 (int_of_float x) (int_of_float y)
            |3 -> zoom_at 0.5 (int_of_float x) (int_of_float y)
            |_ -> ()
        else if tool = buttonline then begin
            (* if we were drawing a line, finalize it *)
            (* TODO: does connect_line really need access to gldrawer? *)
            ignore (GeomEdit.connect_line x y (highlight_distance ())); ()
        end else ()
    |_ -> () end
