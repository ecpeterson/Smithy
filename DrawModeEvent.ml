(*** DrawModeEvent.ml contains routines that handle the keyboard and mouse
 * events encounted by the application while in draw mode. ***)
open CamlExt
open DrawModeWindows
open DrawModeSettings

(* while we're drawing a line, this keeps track of the point we selected at the
 * initial click *)
let start_point = ref 0
let draw_intermediate = ref false
let intermediate_x = ref 0.0
let intermediate_y = ref 0.0

(* while we're dragging our line around it would be nice to see it *)
let intermediate_line x y =
    intermediate_x := x; intermediate_y := y

(* an event utility *)
let highlight_distance orthodrawer =
    !pixel_epsilon /. orthodrawer#scale

(* this gets called when we start applying a tool *)
let tool_begin_event toolbar orthodrawer x y button
                     (state: Gdk.Tags.modifier list) =
    (* unwrap values actually useful to us *)
    let mode = if orthodrawer#space then Draw_Mode else !mode in
    let tool = if orthodrawer#space then PanTool else !active_tool in
    (* get nearby objects, since tools frequently need them *)
    let (point_d, point_i) = MapFormat.get_closest_point x y in
    let (line_d, line_i) = MapFormat.get_closest_line x y in
    let poly = MapFormat.get_enclosing_poly x y in
    let (obj_d, obj_i) = MapFormat.get_closest_object x y in
    let (anno_d, anno_i) = MapFormat.get_closest_annotation x y in
    (* biiiiig switch statement that selects what exactly we want to be doing
     * with this mouse click *)
    begin match (mode, button, toolbar#int_entry,
                 toolbar#float_entry, poly) with
    (* a bunch of these get and set media/light/height/whatever attributes *)
    |Lights_Liquid, 1, v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_media_lightsource v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Lights_Liquid, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#media_lightsource in
        toolbar#set_int v
    |Lights_Ceiling, 1, v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_ceiling_lightsource v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Lights_Ceiling, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#ceiling_lightsource in
        toolbar#set_int v
    |Lights_Floor, 1, v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_floor_lightsource v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Lights_Floor, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#floor_lightsource in
        toolbar#set_int v
    |Elevation_Ceiling, 1, _, v, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_ceiling_height v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Elevation_Ceiling, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#ceiling_height in
        toolbar#set_float v
    |Elevation_Floor, 1, _, v, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_floor_height v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Elevation_Floor, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#floor_height in
        toolbar#set_float v
    |Liquids, 1, v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_media_index v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Liquids, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#media_index in
        toolbar#set_int v
    |Sounds_Ambient, 1, v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_ambient_sound_image_index v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Sounds_Ambient, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#ambient_sound_image_index in
        toolbar#set_int v
    |Sounds_Random, 1, v, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        poly#set_random_sound_image_index v;
        toolbar#update_list;
        orthodrawer#draw ()
    |Sounds_Random, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = poly#random_sound_image_index in
        toolbar#set_int v
    |Polygon_Types, 1, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let ov = poly#kind in
        let v =
            of_enum MapTypes.poly_kind_descriptor toolbar#cb_index in
        poly#set_kind v;
        begin match ov, v with
        |MapTypes.Platform, MapTypes.Platform ->
            MapDialogs.platform_dialog
                !MapFormat.platforms.(poly#permutation) orthodrawer#draw;
            ()
        |MapTypes.Platform, _ ->
            MapFormat.delete_platform poly#permutation;
            orthodrawer#draw ()
        |_, MapTypes.Platform ->
            let plat = new MapTypes.platform in
            plat#set_polygon_index p;
            MapDialogs.platform_dialog plat orthodrawer#draw;
            let plat_idx = MapFormat.add_platform plat in
            poly#set_permutation plat_idx
        |_, _ -> orthodrawer#draw (); ()
        end
    |Polygon_Types, 3, _, _, Some p ->
        let poly = !MapFormat.polygons.(p) in
        let v = to_enum MapTypes.poly_kind_descriptor poly#kind in
        toolbar#set_cb_index v
    |Draw_Mode, 1, _, _, _ ->
        (* in draw mode, we have to deal with what kind of tool to apply *)
        if tool = ArrowTool then begin
            (* see TODO list about things pertaining to this that need to be
             * fixed; there are quite a few. *)
            begin match List.mem `SHIFT state, !highlight with
            (* the arrow tool selects things on mouse down, and multiple things
             * when shift is being held *)
            |true, Point n when point_d < highlight_distance orthodrawer ->
                if List.mem point_i n then
                    (* if this point is already in the selection list, remove *)
                    highlight := Point (List.filter (fun x -> x != point_i) n)
                else
                    (* otherwise, add it fresh *)
                    highlight := Point (point_i :: n)
            |_ when point_d < highlight_distance orthodrawer ->
                highlight := Point [point_i]
            |true, Object n when obj_d < highlight_distance orthodrawer ->
                if List.mem obj_i n then
                    highlight := Object (List.filter (fun x -> x != obj_i) n)
                else
                    highlight := Object (nub (obj_i :: n))
            |_ when obj_d < highlight_distance orthodrawer ->
                highlight := Object [obj_i]
            |true, Annotation n when anno_d < highlight_distance orthodrawer ->
                if List.mem anno_i n then
                    highlight := Annotation
                                        (List.filter (fun x -> x != anno_i) n)
                else
                    highlight := Annotation (nub (anno_i :: n))
            |_ when anno_d < highlight_distance orthodrawer ->
                highlight := Annotation [anno_i]
            |true, Line n when line_d < highlight_distance orthodrawer ->
                if List.mem line_i n then
                    highlight := Line (List.filter (fun x -> x != line_i) n)
                else
                    highlight := Line (nub (line_i :: n))
            |_ when line_d < highlight_distance orthodrawer ->
                highlight := Line [line_i]
            |true, Poly m when poly <> None ->
                let n = of_opt poly in
                if List.mem n m then
                    highlight := Poly (List.filter (fun x -> x != n) m)
                else
                    highlight := Poly (n :: m)
            |_ when poly <> None ->
                let n = of_opt poly in
                highlight := Poly [n]
            |_ ->
                highlight := No_Highlight
            end;
            orthodrawer#draw () end
        (* the line tool draws lines *)
        else if tool = LineTool then begin
            draw_intermediate := true;
            let (x, y) = point_filter (x, y) in
            intermediate_line x y;
            start_point :=
                GeomEdit.start_line x y (highlight_distance orthodrawer) end
        (* and the fill tool fills line loops with polygons *)
        else if tool = FillTool then begin
            GeomEdit.fill_poly x y;
            orthodrawer#draw () end
        else if tool = ObjTool then begin
            begin match poly with
                |Some poly ->
                    let objidx = GeomEdit.make_object (int_of_float x)
                                                      (int_of_float y) poly in
                    MapDialogs.obj_dialog !MapFormat.objs.(objidx)
                        orthodrawer#draw
                |_ -> () end;
            orthodrawer#draw () end
        else if tool = TextTool then begin
            begin match poly with
                |Some poly ->
                    let annoidx = GeomEdit.make_annotation x y poly in
                    MapDialogs.anno_dialog !MapFormat.annotations.(annoidx)
                        orthodrawer#draw
                |_ -> () end;
            orthodrawer#draw () end
        else ()
    |Draw_Mode, 3, _, _, _ ->
        if tool = ArrowTool then begin
            (* a right-click in draw mode with the arrow tool means we want to
             * inspect a map element *)
            (if point_d < highlight_distance orthodrawer then
                MapDialogs.point_dialog !MapFormat.points.(point_i)
                                        orthodrawer#draw
            else if obj_d < highlight_distance orthodrawer then
                MapDialogs.obj_dialog !MapFormat.objs.(obj_i)
                                      orthodrawer#draw
            else if anno_d < highlight_distance orthodrawer then
                MapDialogs.anno_dialog !MapFormat.annotations.(anno_i)
                                       orthodrawer#draw
            else if line_d < highlight_distance orthodrawer then
                MapDialogs.line_dialog !MapFormat.lines.(line_i)
                                       orthodrawer#draw
            else if poly <> None then
                let n = of_opt poly in
                MapDialogs.poly_dialog !MapFormat.polygons.(n)
                                       orthodrawer#draw);
            ()
        end
        else ()
    |_ -> () end

(* this gets called when we're dragging a tool around *)
let tool_in_event toolbar orthodrawer x0 y0 old_x old_y x y =
    (* extract values actually useful to us *)
    let mode = if orthodrawer#space then Draw_Mode else !mode in
    let tool = if orthodrawer#space then PanTool else !active_tool in
    begin match mode with
    |Draw_Mode ->
        (* if we're panning, then pan *)
        if tool = PanTool then
            let xo, yo = orthodrawer#origin in
            orthodrawer#set_origin (xo +. x0 -. x, yo +. y0 -. y)
        (* if we're using the arrow, drag *)
        else if tool = ArrowTool then begin
            let delta_x = (x -. old_x) in
            let delta_y = (y -. old_y) in
            (* utilities for easy dragging *)
            let has_enclosing_poly ix iy =
                match MapFormat.get_enclosing_poly ix iy with
                |Some p -> true
                |None   -> false in
            let shift_points ns =
                let point_index_vertex p =
                    let point = !MapFormat.points.(p) in
                    point#vertex in
                let shift_point p =
                    let (px, py) = point_index_vertex p in
                    !MapFormat.points.(p)#set_vertex
                        (px +. delta_x, py +. delta_y);
                    MapFormat.recalculate_lengths p in
                List.iter (fun p -> shift_point p) ns in
            let shift_obj o dx dy=
                let (ox, oy, oz) = !MapFormat.objs.(o)#point in
                !MapFormat.objs.(o)#set_point ((ox +. dx), (oy +. dy), oz) in
            let shift_objs ns =
                let objs_shift_valid ns =
                    let obj_place_valid o =
                        let (ox, oy, _) = !MapFormat.objs.(o)#point in
                        if has_enclosing_poly x y then
                            Some (x -. ox, y -. oy)
                        else
                            None in
                    let obj_shift_valid o dx dy =
                        let (ox, oy, _) = !MapFormat.objs.(o)#point in
                        has_enclosing_poly (ox +. dx) (oy +. dy) in
                    match ns with
                    |x::xs ->
                        begin match (obj_place_valid x) with
                        |Some (dx, dy) ->
                            if (List.for_all
                                    (fun o -> obj_shift_valid o dx dy) xs) then
                                Some (dx, dy)
                            else
                                None
                        |None -> None end
                    |[] -> None in
                match objs_shift_valid ns with
                |Some (dx, dy) -> List.iter (fun o -> shift_obj o dx dy) ns
                |None -> () in
            let shift_anno a dx dy=
                let (ax, ay) = !MapFormat.annotations.(a)#location in
                !MapFormat.annotations.(a)#set_location (ax +. dx, ay +. dy) in
            let shift_annos ns =
                let annos_shift_valid ns =
                    let anno_place_valid a =
                        let (ax, ay) = !MapFormat.annotations.(a)#location in
                        if has_enclosing_poly x y then
                            Some (x -. ax, y -. ay)
                        else
                            None in
                    let anno_shift_valid a dx dy =
                        let (ax, ay) = !MapFormat.annotations.(a)#location in
                        has_enclosing_poly (ax +. dx) (ay +. dy) in
                    match ns with
                    |x::xs ->
                        begin match (anno_place_valid x) with
                        |Some (dx, dy) ->
                            if (List.for_all
                                    (fun a -> anno_shift_valid a dx dy) xs) then
                                Some (dx, dy)
                            else
                                None
                        |None -> None end
                    |[] -> None in
                match annos_shift_valid ns with
                |Some (dx, dy) -> List.iter (fun a -> shift_anno a dx dy) ns
                |None -> () in
            begin match !highlight with
            |Point ns ->
                shift_points ns
            |Line ns ->
                let points = List.fold_left (fun x y ->
                    let p0, p1 = !MapFormat.lines.(y)#endpoints in
                    p0 :: p1 :: x) [] ns in
                shift_points (nub points)
            |Annotation ns ->
                shift_annos ns
            |Poly ns ->
                let points = List.fold_left (fun x y ->
                    let poly = !MapFormat.polygons.(y) in
                    let points = poly#endpoint_indices in
                    let points = Array.sub points 0 poly#vertex_count in
                    Array.to_list points @ x) [] ns in
                shift_points (nub points);
                List.iter (fun o -> shift_obj o delta_x delta_y)
                          (array_grep_indices
                               (fun obj -> List.mem obj#polygon ns)
                               !MapFormat.objs);
                List.iter (fun a -> shift_anno a delta_x delta_y)
                          (array_grep_indices
                               (fun anno -> List.mem anno#polygon_index ns)
                               !MapFormat.annotations)
            |Object ns ->
                shift_objs ns
            |_ -> () end;
            orthodrawer#draw () end
        else if tool = LineTool then begin
            intermediate_line x y;
            orthodrawer#draw () end
        else ()
    |_ -> () end

(* and this gets called when we release the mouse button and apply the tool *)
let tool_end_event toolbar orthodrawer x0 y0 x y (button: int) _ =
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
                    let (x, y, _) = obj#point in
                    match MapFormat.get_enclosing_poly x y with
                    |None -> MapFormat.delete_obj n
                    |Some a -> obj#set_polygon a) n
            |Annotation n ->
                List.iter (fun n ->
                    let anno = !MapFormat.annotations.(n) in
                    let (x, y) = anno#location in
                    match MapFormat.get_enclosing_poly x y with
                    |None -> MapFormat.delete_annotation n
                    |Some a -> anno#set_polygon_index a) n
            (* if we resize a polygon so that an object or annotation is
             * outside of it, delete that object/annotation *)
            (* TODO *)
            |Point n -> ()
            |Line n -> ()
            |Poly n -> ()
            |_ -> ()
            end
        else if tool = ZoomTool then
            (* if we were using the zoom tool, apply the zoom *)
            match button with
            |1 -> orthodrawer#zoom_at 2.0 x y
            |3 -> orthodrawer#zoom_at 0.5 x y
            |_ -> ()
        else if tool = LineTool then begin
            (* if we were drawing a line, finalize it *)
            let (x, y) = point_filter (x, y) in
            draw_intermediate := false;
            ignore (GeomEdit.connect_line !start_point x y
                                          (highlight_distance orthodrawer));
            orthodrawer#draw ()
        end else ()
    |_ -> () end

(* depending upon what mode we're in, launch the appropriate dialog *)
let edit_current_item toolbar orthodrawer =
    try match (toolbar#int_entry, !mode) with
    |(-1, _) -> ()
    |(index, Liquids) ->
        MapDialogs.media_dialog !MapFormat.media.(index) orthodrawer#draw
    |(index, Lights_Floor)
    |(index, Lights_Liquid)
    |(index, Lights_Ceiling) ->
        MapDialogs.light_dialog !MapFormat.lights.(index) orthodrawer#draw
    |(index, Sounds_Random) ->
        MapDialogs.random_dialog !MapFormat.randoms.(index) orthodrawer#draw
    |(index, Sounds_Ambient) ->
        MapDialogs.ambient_dialog !MapFormat.ambients.(index) orthodrawer#draw
    |_ -> ()
    with Failure "int_of_string" -> ()

(* depending upon what mode we're in, spawn in a new light/media/whatever and
 * open the editor so that we can customize it *)
let make_new_item toolbar orthodrawer =
    let creator = begin match !mode with
    |Liquids ->
        Some (fun draw -> MapDialogs.make_media draw)
    |Lights_Floor
    |Lights_Liquid
    |Lights_Ceiling ->
        Some (fun draw -> MapDialogs.make_light draw)
    |Sounds_Random ->
        Some (fun draw -> MapDialogs.make_random draw)
    |Sounds_Ambient ->
        Some (fun draw -> MapDialogs.make_ambient draw)
    |_ -> None end in
    match creator with
    |Some f ->
        toolbar#set_int (f orthodrawer#draw);
        toolbar#update_list
    |None -> ()
