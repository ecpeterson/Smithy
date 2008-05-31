open CamlExt
open DrawModeWindows

(* this is configurable *)
let highlighted_point_thickness = 5

(* stateful information *)
let grid_factor = ref 3

(* module methods *)
let draw_grid _ =
    (* draw grid lines *)
    orthodrawer#set_color Colors.grid_color;
    let grid_factor = 32 * (pow 2 !grid_factor) in
    for i = -grid_factor to grid_factor do
        let it = i * (MapFormat.half_map_width/grid_factor) in
        orthodrawer#line (-MapFormat.half_map_width, it)
                         ( MapFormat.half_map_width, it);
        orthodrawer#line (it, -MapFormat.half_map_width)
                         (it,  MapFormat.half_map_width);
    done;
    (* draw grid points *)
    orthodrawer#set_color Colors.anchor_point_color;
    for i = -32 to 32 do
        for j = -32 to 32 do
            let it, jt = i * (MapFormat.half_map_width/32),
                         j * (MapFormat.half_map_width/32) in
            orthodrawer#point (it, jt)
        done;
    done

let draw_points _ =
    orthodrawer#set_color Colors.point_color;
    !MapFormat.points |> Array.map (fun x -> x#vertex ())
                      |> Array.to_list
                      |> orthodrawer#points

let draw_lines _ =
    let draw_these f =
        !MapFormat.lines |> Array.to_list
                         |> List.filter f
                         |> List.map (fun x ->
                             let x, y = x#endpoints () in
                             !MapFormat.points.(x)#vertex (),
                             !MapFormat.points.(y)#vertex ())
                         |> orthodrawer#segments in
    orthodrawer#set_color Colors.solid_line_color;
    draw_these (fun x -> List.mem MapTypes.SOLID (x#flags ()));
    orthodrawer#set_color Colors.transparent_line_color;
    draw_these (fun x -> List.mem MapTypes.TRANSPARENT (x#flags ()) &&
                         not (List.mem MapTypes.SOLID (x#flags ())));
    orthodrawer#set_color Colors.passable_line_color;
    draw_these (fun x -> not (List.mem MapTypes.TRANSPARENT (x#flags ())) &&
                         not (List.mem MapTypes.SOLID (x#flags ())))

let draw_polygons _ =
    let max =
        match !mode with
        |Polygon_Types ->
            float (List.length ((fun (_, x)-> x) MapTypes.poly_kind_descriptor))
        |Liquids ->
            Array.length !MapFormat.media |> float
        |Lights_Floor
        |Lights_Ceiling
        |Lights_Liquid ->
            Array.length !MapFormat.lights |> float
        |_ -> 0.0 in
    let draw_polygon poly =
        let vertex_array = List.map (fun x -> !MapFormat.points.(x)#vertex())
            (MapFormat.get_poly_ring poly) in
        begin match !mode with
        |Draw_Mode ->
            if GeomEdit.vertex_array_is_concave vertex_array then
                orthodrawer#set_color Colors.invalid_polygon
            else
                orthodrawer#set_color Colors.polygon_color;
        |Elevation_Ceiling ->
            let n = poly#ceiling_height () /. 18.0 +. 0.5 in
            orthodrawer#set_color (n, n, n)
        |Elevation_Floor ->
            let n = poly#floor_height () /. 18.0 +. 0.5 in
            orthodrawer#set_color (n, n, n)
        |Polygon_Types ->
            let kind = float (CamlExt.to_enum MapTypes.poly_kind_descriptor
                                              (poly#kind ())) in
            CamlExt.hsv_to_rgb (kind /. max,
                                Colors.poly_type_saturation,
                                Colors.poly_type_value)
                |> orthodrawer#set_color;
        |Liquids ->
            if poly#media_index () = -1 then
                orthodrawer#set_color (0.5, 0.5, 0.5)
            else
                let n = float (poly#media_index ()) /. (max -. 1.0) in
                orthodrawer#set_color (n, 0.0, 0.0)
        |Lights_Floor ->
            orthodrawer#set_color (float (poly#floor_lightsource ()) /. max,
                                   0.0, 0.0)
        |Lights_Ceiling ->
            orthodrawer#set_color (float (poly#ceiling_lightsource ()) /. max,
                                   0.0, 0.0)
        |Lights_Liquid ->
            orthodrawer#set_color (float (poly#media_lightsource ()) /. max,
                                   0.0, 0.0)
        |_ -> () end;
        orthodrawer#polygon true vertex_array in
    Array.iter draw_polygon !MapFormat.polygons

let draw_highlight _ =
    orthodrawer#set_color Colors.highlight_color;
    match !highlight with
    |Point ns ->
        List.iter (fun n ->
            let x, y = !MapFormat.points.(n)#vertex () in
            orthodrawer#fat_point (x, y) highlighted_point_thickness) ns
    |Line ns ->
        List.iter (fun n ->
            let p0, p1 = !MapFormat.lines.(n)#endpoints () in
            let x0, y0 = !MapFormat.points.(p0)#vertex () in
            let x1, y1 = !MapFormat.points.(p1)#vertex () in
            orthodrawer#line (x0, y0) (x1, y1)) ns
    |Poly ns ->
        List.iter (fun n ->
            let vertex_array =
                List.map (fun x -> !MapFormat.points.(x)#vertex())
                    (MapFormat.get_poly_ring !MapFormat.polygons.(n)) in
            orthodrawer#polygon true vertex_array) ns
    |No_Highlight |_ -> ()

let draw _ =
    orthodrawer#set_color Colors.background_color;
    orthodrawer#clear ();
    draw_grid ();
    draw_polygons ();
    draw_lines ();
    draw_points ();
    draw_highlight ();
    (* draw the line we're in the middle of laying, if appropriate *)
    if !GeomEdit.draw_intermediate then begin
        orthodrawer#set_color Colors.solid_line_color;
        let x, y = !MapFormat.points.(!GeomEdit.start_point)#vertex () in
        orthodrawer#line (x, y)
                         (!GeomEdit.intermediate_x, !GeomEdit.intermediate_y)
    end

(* set up draw mode event hooks *)
let _ =
    orthodrawer#connect_draw draw;
    ignore (vadj#connect#value_changed DrawModeEvent.slider_callback);
    ignore (hadj#connect#value_changed DrawModeEvent.slider_callback);
    orthodrawer#connect_mousedown DrawModeEvent.tool_begin_event;
    orthodrawer#connect_mouseup   DrawModeEvent.tool_end_event;
    orthodrawer#connect_mousedrag DrawModeEvent.tool_in_event;
    orthodrawer#connect_scroll DrawModeEvent.scroll_callback
