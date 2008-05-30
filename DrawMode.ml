open CamlExt
open DrawModeWindows

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
    orthodrawer#set_color Colors.line_color;
    !MapFormat.lines |> Array.map (fun x -> x#endpoints ())
                     |> Array.to_list
                     |> List.map (fun (x, y) ->
                         (!MapFormat.points.(x)#vertex (),
                          !MapFormat.points.(y)#vertex ()))
                     |> orthodrawer#segments

(* TODO: this is actually mode dependent, of course *)
let draw_polygons _ =
    let draw_polygon poly =
        let vertex_array = List.map (fun x -> !MapFormat.points.(x)#vertex())
            (MapFormat.get_poly_ring poly) in
        if GeomEdit.vertex_array_is_concave vertex_array then
            orthodrawer#set_color Colors.invalid_polygon
        else
            orthodrawer#set_color Colors.polygon_color;
        orthodrawer#polygon true vertex_array in
    Array.iter draw_polygon !MapFormat.polygons

let draw _ =
    orthodrawer#set_color Colors.background_color;
    orthodrawer#clear ();
    draw_grid ();
    draw_polygons ();
    draw_lines ();
    draw_points ()

(* set up draw mode event hooks *)
let _ =
    orthodrawer#connect_draw draw;
    vadj#connect#value_changed DrawModeEvent.slider_callback;
    hadj#connect#value_changed DrawModeEvent.slider_callback;
    orthodrawer#connect_mousedown DrawModeEvent.tool_begin_event;
    orthodrawer#connect_mouseup   DrawModeEvent.tool_end_event;
    orthodrawer#connect_mousedrag DrawModeEvent.tool_in_event;
    orthodrawer#connect_scroll DrawModeEvent.scroll_callback
