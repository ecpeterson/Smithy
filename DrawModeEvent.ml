open DrawModeWindows
open DrawModeSettings

(* while we're drawing a line, this keeps track of the point we selected at the
 * initial click *)
let start_point = ref 0
let draw_intermediate = ref false
let intermediate_x = ref 0
let intermediate_y = ref 0

(* while we're dragging our line around it would be nice to see it *)
let intermediate_line x y =
    intermediate_x := int_of_float x;
    intermediate_y := int_of_float y

(* an event utility, note that the 8.0 is actually configurable *)
let highlight_distance orthodrawer =
    let pixel_epsilon = 8.0 in
    pixel_epsilon /. orthodrawer#scale

(* this gets called when we start applying a tool *)
let tool_begin_event orthodrawer x y button (state: Gdk.Tags.modifier list) =
    let x, y = float x, float y in
    (* unwrap values actually useful to us *)
    let tool = !active_tool in
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
        if tool = ArrowTool then begin
            (* see TODO list about things pertaining to this that need to be
             * fixed; there are quite a few. *)
            begin match List.mem `SHIFT state, !highlight with
            (* the arrow tool selects things on mouse down, and multiple things
             * when shift is being held *)
            |true, Point n when point_d < highlight_distance orthodrawer ->
                    highlight := Point (point_i :: n)
            |_ when point_d < highlight_distance orthodrawer ->
                    highlight := Point [point_i]
            |true, Object n when obj_d < highlight_distance orthodrawer ->
                    highlight := Object (obj_i :: n)
            |_ when obj_d < highlight_distance orthodrawer ->
                    highlight := Object [obj_i]
            |true, Line n when line_d < highlight_distance orthodrawer ->
                    highlight := Line (line_i :: n)
            |_ when line_d < highlight_distance orthodrawer ->
                    highlight := Line [line_i]
            |true, Poly m when poly <> None ->
                    let Some n = poly in
                    highlight := Poly (n :: m)
            |_ when poly <> None ->
                    let Some n = poly in
                    highlight := Poly [n]
            |_ ->
                    highlight := No_Highlight
            end;
            orthodrawer#draw () end
        (* the line tool draws lines *)
        else if tool = LineTool then begin
            draw_intermediate := true;
            intermediate_line x y;
            start_point :=
                GeomEdit.start_line x y (highlight_distance orthodrawer) end
        (* and the fill tool fills line loops with polygons *)
        else if tool = FillTool then begin
            GeomEdit.fill_poly (int_of_float x) (int_of_float y);
            orthodrawer#draw () end
        else ()
    |Draw_Mode, 3, _, _, _ ->
        if tool = ArrowTool then
            (* a right-click in draw mode with the arrow tool means we want to
             * inspect a map element *)
            if point_d < highlight_distance orthodrawer then
                MapDialogs.point_dialog !MapFormat.points.(point_i)
            else if obj_d < highlight_distance orthodrawer then
                MapDialogs.obj_dialog !MapFormat.objs.(obj_i)
            else if line_d < highlight_distance orthodrawer then (
                MapDialogs.line_dialog !MapFormat.lines.(line_i)
            ) else if poly <> None then let Some n = poly in (
                MapDialogs.poly_dialog !MapFormat.polygons.(n))
        else ()
    |_ -> () end

(* this gets called when we're dragging a tool around *)
let tool_in_event orthodrawer x0 y0 old_x old_y x y =
    let x0, y0, old_x, old_y, x, y = float x0, float y0, float old_x,
                                     float old_y, float x, float y in
    (* extract values actually useful to us *)
    let tool = !active_tool in
    begin match !mode with
    |Draw_Mode ->
        (* if we're panning, then pan *)
        if tool = PanTool then
            let xo, yo = orthodrawer#origin in
            orthodrawer#set_origin ((int_of_float (float xo +. x0 -. x)),
                                    (int_of_float (float yo +. y0 -. y)))
        (* if we're using the arrow, drag *)
        else if tool = ArrowTool then begin
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
                    let points = Array.sub points 0 (poly#vertex_count ()) in
                    Array.to_list points @ x) [] ns in
                List.iter shift_point (CamlExt.nub points)
            |Object ns ->
                List.iter (fun n -> shift_obj !MapFormat.objs.(n)) ns
            |_ -> () end;
            orthodrawer#draw () end
        else if tool = LineTool then begin
            intermediate_line x y;
            orthodrawer#draw () end
        else ()
    |_ -> () end

(* and this gets called when we release the mouse button and apply the tool *)
let tool_end_event orthodrawer x0 y0 x y (button: int) _ =
    let x0, y0, x, y = float x0, float y0, float x, float y in
    let tool = !active_tool in
    begin match !mode with
    |Draw_Mode ->
        if tool = ArrowTool then
            (* if we were dragging around objects, be sure to place them in the
             * appropriate polygon! *)
            begin match !highlight with
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
            end
        else if tool = ZoomTool then
            (* if we were using the zoom tool, apply the zoom *)
            match button with
            |1 -> orthodrawer#zoom_at 2.0 (int_of_float x) (int_of_float y)
            |3 -> orthodrawer#zoom_at 0.5 (int_of_float x) (int_of_float y)
            |_ -> ()
        else if tool = LineTool then begin
            (* if we were drawing a line, finalize it *)
            draw_intermediate := false;
            ignore (GeomEdit.connect_line !start_point x y
                                          (highlight_distance orthodrawer));
            orthodrawer#draw ()
        end else ()
    |_ -> () end

(* depending upon what mode we're in, launch the appropriate dialog *)
let edit_current_item orthodrawer =
    try match (int_of_string numeric_entry#text, !mode) with
    |(-1, _) -> ()
    |(index, Liquids) ->
        MapDialogs.media_dialog !MapFormat.media.(index)
    |(index, Lights_Floor)
    |(index, Lights_Liquid)
    |(index, Lights_Ceiling) ->
        MapDialogs.light_dialog !MapFormat.lights.(index)
    |_ -> ()
    with Failure "int_of_string" -> ()

(* depending upon what mode we're in, spawn in a new light/media/whatever and
 * open the editor so that we can customize it *)
let make_new_item orthodrawer =
    begin match !mode with
    |Liquids ->
        let n = MapDialogs.make_media () in
        numeric_entry#set_text (string_of_int n)
    |Lights_Floor
    |Lights_Liquid
    |Lights_Ceiling ->
        let l = MapDialogs.make_light () in
        numeric_entry#set_text (string_of_int l)
    |_ -> () end;
    orthodrawer#draw ()
