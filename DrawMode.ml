(*** DrawMode.ml contains routines that handle the actual drawing of the
 * overhead map while in draw mode. ***)
open CamlExt
open DrawModeWindows
open DrawModeSettings
open MapTypes

(* module methods *)
let draw_grid _ =
    (* draw grid lines *)
    orthodrawer#set_color !Colors.grid_color;
    (* 0 -> 2 WU, 1 -> 1 WU, ..., 4 -> 1/8 WU *)
    let grid_size = 2048 / (pow 2 !grid_factor) in
    for i = (-MapFormat.half_map_width / grid_size) to
            (MapFormat.half_map_width / grid_size) do
        let it = i * grid_size in
        orthodrawer#line (-MapFormat.half_map_width, it)
                         ( MapFormat.half_map_width, it);
        orthodrawer#line (it, -MapFormat.half_map_width)
                         (it,  MapFormat.half_map_width);
    done;
    (* draw grid points *)
    orthodrawer#set_color !Colors.anchor_point_color;
    let grid_size = if grid_size < 1024 then 1024 else grid_size in
    for i = (-MapFormat.half_map_width / grid_size) to
            (MapFormat.half_map_width / grid_size) do
        for j = (-MapFormat.half_map_width / grid_size) to
                (MapFormat.half_map_width / grid_size) do
            let it, jt = (i * grid_size),
                         (j * grid_size) in
            orthodrawer#fat_point (it, jt) 2
        done;
    done

let draw_points _ =
    orthodrawer#set_color !Colors.point_color;
    !MapFormat.points |> Array.map (fun x -> x#vertex ())
                      |> Array.iter (fun (x, y) ->
                                        orthodrawer#fat_point (x, y) 2)

let draw_lines _ =
    let draw_these f =
        !MapFormat.lines |> Array.to_list
                         |> List.filter f
                         |> List.map (fun x ->
                             let x, y = x#endpoints () in
                             !MapFormat.points.(x)#vertex (),
                             !MapFormat.points.(y)#vertex ())
                         |> orthodrawer#segments in
    orthodrawer#set_color !Colors.solid_line_color;
    draw_these (fun x -> List.mem SOLID (x#flags ()));
    orthodrawer#set_color !Colors.transparent_line_color;
    draw_these (fun x -> List.mem TRANSPARENT (x#flags ()) &&
                         not (List.mem SOLID (x#flags ())));
    orthodrawer#set_color !Colors.passable_line_color;
    draw_these (fun x -> not (List.mem TRANSPARENT (x#flags ())) &&
                         not (List.mem SOLID (x#flags ())))

let draw_polygons _ =
    let max =
        match !mode with
        |Polygon_Types ->
            float (List.length ((fun (_, x)-> x) poly_kind_descriptor))
        |Liquids ->
            Array.length !MapFormat.media |> float
        |Lights_Floor
        |Lights_Ceiling
        |Lights_Liquid ->
            Array.length !MapFormat.lights |> float
        |Sounds_Random ->
            Array.length !MapFormat.randoms |> float
        |Sounds_Ambient ->
            Array.length !MapFormat.ambients |> float
        |_ -> 0.0 in
    let draw_polygon poly =
        let vertex_array = List.map (fun x -> !MapFormat.points.(x)#vertex())
            (MapFormat.get_poly_ring poly) in
        begin match !mode with
        |Draw_Mode ->
            if CamlExt.vertex_array_is_concave vertex_array then
                orthodrawer#set_color !Colors.invalid_polygon
            else
                orthodrawer#set_color !Colors.polygon_color;
        |Elevation_Ceiling ->
            let n = poly#ceiling_height () /. 18.0 +. 0.5 in
            orthodrawer#set_color (n, n, n)
        |Elevation_Floor ->
            let n = poly#floor_height () /. 18.0 +. 0.5 in
            orthodrawer#set_color (n, n, n)
        |Polygon_Types ->
            let kind = float (CamlExt.to_enum poly_kind_descriptor
                                              (poly#kind ())) in
            CamlExt.hsv_to_rgb (kind /. max,
                                !Colors.poly_type_saturation,
                                !Colors.poly_type_value)
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
        |Sounds_Random ->
            let n = poly#random_sound_image_index () in
            orthodrawer#set_color (
                if n = -1 then (0.5, 0.5, 0.5) else (float n /. max, 0.0, 0.0))
        |Sounds_Ambient ->
            let n = poly#ambient_sound_image_index () in
            orthodrawer#set_color (
                if n = -1 then (0.5, 0.5, 0.5) else (float n /. max, 0.0, 0.0))
        |_ -> () end;
        if (poly#floor_height () > !DrawModeSettings.floor_cutoff &&
            poly#ceiling_height () < !DrawModeSettings.ceiling_cutoff) then
                orthodrawer#polygon true vertex_array in
    Array.iter draw_polygon !MapFormat.polygons

let draw_objects _ =
    let draw_obj obj =
        let draw_filename name x y =
            orthodrawer#image (GMisc.image ~file:name ()) x y in
        let draw_arrow x y color facing =
            orthodrawer#set_color color;
            orthodrawer#arrow x y facing in
        let draw_item index x y =
            let name = List.nth ItemStrings.item_strings index in
            try
                let filename = Hashtbl.find Resources.item_hash name in
                draw_filename filename x y
            with Not_found -> print_endline ("failed to find" ^ name) in
        let x, y, z = obj#point () in
        let p = !MapFormat.polygons.(obj#polygon ()) in
        if (p#ceiling_height () < !DrawModeSettings.ceiling_cutoff &&
            p#floor_height () > !DrawModeSettings.floor_cutoff) then
        match obj#kind () with
        |Monster -> if !DrawModeSettings.show_monsters then
                       draw_arrow x y (1.0, 0.0, 0.0) (obj#facing ())
        |Scenery -> if !DrawModeSettings.show_scenery then
                       draw_filename Resources.sceneryfile x y
        |Item    -> if !DrawModeSettings.show_objects then
                       draw_item (obj#index ()) x y
        |Player  -> if !DrawModeSettings.show_players then
                       draw_arrow x y (1.0, 1.0, 0.0) (obj#facing ())
        |Goal    -> if !DrawModeSettings.show_goals then
                       draw_filename Resources.goalfile    x y
        |Sound_Source -> if !DrawModeSettings.show_sounds then
                       draw_filename Resources.sound_sourcefile x y
    in
    Array.iter draw_obj !MapFormat.objs

let draw_highlight _ =
    orthodrawer#set_color !Colors.highlight_color;
    match !highlight with
    |Point ns ->
        List.iter (fun n ->
            let x, y = !MapFormat.points.(n)#vertex () in
            orthodrawer#fat_point (x, y) !highlighted_point_thickness) ns
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

let draw_platforms _ =
    Array.iter (fun plat ->
        let poly = !MapFormat.polygons.(plat#polygon_index ()) in
        Array.sub (poly#endpoint_indices ()) 0 (poly#vertex_count ()) |>
            GeomEdit.point_center |>
            orthodrawer#centered_text (string_of_int (plat#polygon_index ())))
                !MapFormat.platforms

let draw_annotations _ =
    orthodrawer#set_color !Colors.annotation_color;
    Array.iter (fun annotation -> orthodrawer#text
                                  (annotation#text ())
                                  (annotation#location ()))
               !MapFormat.annotations;
    (* we want the annotation to be on top of the current highlight if the
     * current highlight isn't an annotation, so we have to handle this here *)
    match !highlight with
    |Annotation n ->
        orthodrawer#set_color !Colors.highlight_color;
        List.iter (fun a -> let anno = !MapFormat.annotations.(a) in
                            orthodrawer#text (anno#text ()) (anno#location ()))
                  n
    |_ -> ()

let draw orthodrawer =
    orthodrawer#set_color !Colors.background_color;
    orthodrawer#clear ();
    if !DrawModeSettings.display_grid then draw_grid ();
    begin try draw_polygons () with _ -> print_endline "Polygons!" end;
    begin try draw_lines () with _ -> print_endline "Lines!" end;
    begin try draw_points () with _ -> print_endline "Points!" end;
    begin try draw_highlight () with _ -> print_endline "Highlight!" end;
    begin try draw_objects () with _ -> print_endline "Objects!" end;
    begin try draw_platforms () with _ -> print_endline "Platforms!" end;
    begin try draw_annotations () with _ -> print_endline "Annotations!" end;
    (* draw the line we're in the middle of laying, if appropriate *)
    if !DrawModeEvent.draw_intermediate then begin
        orthodrawer#set_color !Colors.solid_line_color;
        let x, y = !MapFormat.points.(!DrawModeEvent.start_point)#vertex () in
        let xe, ye = GeomEdit.point_filter (!DrawModeEvent.intermediate_x,
                                            !DrawModeEvent.intermediate_y) in
        orthodrawer#line (x, y) (xe, ye)
    end

(* set up draw mode event hooks *)
let _ =
    orthodrawer#connect_draw draw;
    orthodrawer#connect_mousedown DrawModeEvent.tool_begin_event;
    orthodrawer#connect_mouseup   DrawModeEvent.tool_end_event;
    orthodrawer#connect_mousedrag DrawModeEvent.tool_in_event;
    newbutton#connect#clicked
        (fun _ -> DrawModeEvent.make_new_item orthodrawer);
    editbutton#connect#clicked
        (fun _ -> DrawModeEvent.edit_current_item orthodrawer);
