(*** Smithy.ml houses the main Smithy application and contains all of the glue
 * and interface functionality. ***)

open MapFormat
open Resources

(* initialize GTK itself *)
let _ = GtkMain.Main.init ()

(* a function that kills the application, will eventually check for things like
 * altered documents and request saves *)
let quit_application () =
    GMain.Main.quit ()

(* creates a menu in a menubar *)
let create_menu label menubar =
    let item = GMenu.menu_item ~label ~packing:menubar#append () in
    GMenu.menu ~packing:item#set_submenu ()

(** set up GTK widgets **)
(* the main draw window *)
let window = GWindow.window ~width:500 ~height:300 ~title:"Smithy"
    ~allow_shrink:true ()
let vbox = GPack.vbox ~packing:window#add ()
let menu_bar = GMenu.menu_bar ~packing:vbox#pack ~height:20 ()
let file_menu = create_menu "File" menu_bar
let view_menu = create_menu "View" menu_bar
let special_menu = create_menu "Special" menu_bar
let hbox = GPack.hbox ~packing:vbox#add ()
let eventbox = GBin.event_box ~packing:hbox#add ()
let ar = GlGtk.area [`USE_GL;`RGBA;`DOUBLEBUFFER]
                    ~packing:eventbox#add ~show:true ()
let vadj = GData.adjustment ~value:0.0 ~lower:(0.0 -. half_map_width)
                            ~upper:half_map_width ~step_incr:16.0 ()
let hadj = GData.adjustment ~value:0.0 ~lower:(0.0 -. half_map_width)
                            ~upper:half_map_width ~step_incr:16.0 ()
let gl = new GlFlatDraw.gldrawer ar vadj hadj
let vscroll = GRange.scrollbar `VERTICAL ~packing:hbox#pack
    ~adjustment:vadj ()
let hscroll = GRange.scrollbar `HORIZONTAL ~packing:vbox#pack
    ~adjustment:hadj ()
let sb = GMisc.statusbar ~packing:vbox#pack ()
let sbc = sb#new_context ~name:"Test"
let set_status x = sbc#pop (); sbc#push x

(* the toolbar *)
let toolbar = GWindow.window ~type_hint:`TOOLBAR ~title:"Smithy Toolkit" ()
let vbox = GPack.vbox ~packing:toolbar#add ()
let hbox1 = GPack.hbox ~packing:vbox#pack ()
let hbox2 = GPack.hbox ~packing:vbox#pack ()
let hbox3 = GPack.hbox ~packing:vbox#pack ()
let hbox4 = GPack.hbox ~packing:vbox#pack ()
let buttonarrow = GButton.toggle_button ~packing:hbox1#pack ~active:true ()
let buttonline = GButton.toggle_button ~packing:hbox1#pack ()
let buttonpoly = GButton.toggle_button ~packing:hbox2#pack ()
let buttonfill = GButton.toggle_button ~packing:hbox2#pack ()
let buttonpan = GButton.toggle_button ~packing:hbox3#pack ()
let buttonzoom = GButton.toggle_button ~packing:hbox3#pack ()
let buttontext = GButton.toggle_button ~packing:hbox4#pack ()
let buttonobj = GButton.toggle_button ~packing:hbox4#pack ()
let buttons = [buttonarrow; buttonline; buttonpoly; buttonfill;
               buttonpan; buttonzoom; buttontext; buttonobj]
let arrowglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm arrowfile ()) ~packing:buttonarrow#add ()
let lineglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm linefile ()) ~packing:buttonline#add ()
let polyglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm polyfile ()) ~packing:buttonpoly#add ()
let fillglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm fillfile ()) ~packing:buttonfill#add ()
let panglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm panfile ()) ~packing:buttonpan#add ()
let zoomglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm zoomfile ()) ~packing:buttonzoom#add ()
let textglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm textfile ()) ~packing:buttontext#add ()
let objglyph =
    GMisc.pixmap (GDraw.pixmap_from_xpm objfile ()) ~packing:buttonobj#add ()

(* and the alternative toolbar *)
let entry_toolbar = GWindow.window ~type_hint:`TOOLBAR ~title:"Smithy"
                                   ~show:false ()
let vbox = GPack.vbox ~packing:entry_toolbar#add ()
let hbox = GPack.hbox ~packing:vbox#add ()
let entry_label = GMisc.label ~text:"Height: " ~packing:hbox#add ()
let numeric_entry = GEdit.entry ~packing:hbox#add ()
let mediabox = GPack.hbox ~packing:vbox#add ()
let editbutton = GButton.button ~label:"Edit Media..." ~packing:mediabox#add ()
let newbutton = GButton.button ~label:"New Media..." ~packing:mediabox#add ()

(* and the map object we'll be recycling *)
let map = new map

(* routines that deal with the alternative toolbar *)
let numeric_int = ref (None)
let numeric_float = ref (None)

let entry_callback () =
    let t = numeric_entry#text in
    begin numeric_int := try Some (int_of_string t) with _ -> begin
        try Some (int_of_float (float_of_string t)) with _ -> None end end;
    begin numeric_float := try Some (float_of_string t) with _ -> begin
        try Some (float (int_of_string t)) with _ -> None end end
let edit_current_item () =
    match (!numeric_int, gl#mode ()) with
    |(None, _) |(Some (-1), _) -> ()
    |(Some index, GlFlatDraw.Media) ->
        let media = map#get_media_array () in
        MapDialogs.media_dialog (Array.get media index)
    |(Some index, GlFlatDraw.Floor_Light)
    |(Some index, GlFlatDraw.Media_Light)
    |(Some index, GlFlatDraw.Ceiling_Light) ->
        let lights = map#get_lights_array () in
        MapDialogs.light_dialog (Array.get lights index)
    |_ -> ()
let make_new_item () =
    match gl#mode () with
        |GlFlatDraw.Media ->
            let n = MapDialogs.make_media map in
            numeric_entry#set_text (string_of_int n);
            gl#draw ()
        |GlFlatDraw.Floor_Light
        |GlFlatDraw.Media_Light
        |GlFlatDraw.Ceiling_Light ->
            let l = MapDialogs.make_light map in
            numeric_entry#set_text (string_of_int l);
            gl#draw ()
        |_ -> ()

(* if we need to do any cleanup work when the window/app gets destroyed, this is
 * the place to do it *)
let deleting_window _ =
    false

let set_title x = window#set_title x

let new_map () =
    ()

(* called when a button on the toolbar is clicked, before the appropriate
 * toggle_button gets its activity set.  we just make sure that state of all the
 * buttons is clear, but really more could be done here *)
let toolbar_clicked which =
    List.iter (fun x -> x#set_active false) buttons

(* gets the active tool for our event handlers that have to figure out which
 * tool to pass to *)
let active_tool () =
    let rec a_t_aux lst =
        if (List.hd lst)#active
        then List.hd lst
        else a_t_aux (List.tl lst) in
    a_t_aux buttons

(* mutable values to keep track of mouse clicks / tool applications with *)
let x0 = ref 0.0
let y0 = ref 0.0
let x1 = ref 0.0
let y1 = ref 0.0

(* these get called when we get interface ticks for zoom in, zoom out *)
let zoom_in () =
    gl#set_zoom_with_center (gl#get_zoom () *. 2.0) !x1 !y1

let zoom_out () =
    gl#set_zoom_with_center (gl#get_zoom () *. 0.5) !x1 !y1

(* the constant here is actually configurable, controls how sensitive the mouse
 * is as far as needing to be close to geometric objects to select them *)
let highlight_distance () = 4.0 /. (gl#get_zoom ())

(* this gets called when we start applying a tool *)
let tool_begin_event mouse_descriptor =
    let x = GdkEvent.Button.x mouse_descriptor in
    let y = GdkEvent.Button.y mouse_descriptor in
    let button = GdkEvent.Button.button mouse_descriptor in
    let tool = active_tool () in
    x0 := x; y0 := y; x1 := x; y1 := y;
    let (x, y) = gl#to_map_coords x y in
    let (point_d, point_i) = map#get_closest_point x y in
    let (line_d, line_i) = map#get_closest_line x y in
    let poly = map#get_enclosing_poly x y in
    let (obj_d, obj_i) = map#get_closest_object x y in
    begin match (gl#mode (), button, !numeric_int, !numeric_float, poly) with
    |GlFlatDraw.Media_Light, 1, Some v, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        poly#set_media_lightsource v
    |GlFlatDraw.Media_Light, 3, _, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        let v = poly#media_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |GlFlatDraw.Ceiling_Light, 1, Some v, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        poly#set_ceiling_lightsource v
    |GlFlatDraw.Ceiling_Light, 3, _, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        let v = poly#ceiling_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |GlFlatDraw.Floor_Light, 1, Some v, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        poly#set_floor_lightsource v
    |GlFlatDraw.Floor_Light, 3, _, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        let v = poly#floor_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |GlFlatDraw.Ceiling_Height, 1, _, Some v, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        poly#set_ceiling_height v
    |GlFlatDraw.Ceiling_Height, 3, _, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        let v = poly#ceiling_height () in
        numeric_entry#set_text (string_of_float v)
    |GlFlatDraw.Floor_Height, 1, _, Some v, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        poly#set_floor_height v
    |GlFlatDraw.Floor_Height, 3, _, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        let v = poly#floor_height () in
        numeric_entry#set_text (string_of_float v)
    |GlFlatDraw.Media, 1, Some v, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        poly#set_media_index v
    |GlFlatDraw.Media, 3, _, _, Some p ->
        let poly = Array.get (map#get_polygons_array ()) p in
        let v = poly#media_index () in
        numeric_entry#set_text (string_of_int v)
    |GlFlatDraw.Draw, 1, _, _, _ ->
        if tool = buttonarrow then
            if point_d < highlight_distance () then
                gl#set_highlighted (GlFlatDraw.Point point_i)
            else if obj_d < highlight_distance () then
                gl#set_highlighted (GlFlatDraw.Object obj_i)
            else if line_d < highlight_distance () then
                gl#set_highlighted (GlFlatDraw.Line line_i)
            else if poly != None then
                let Some n = poly in gl#set_highlighted (GlFlatDraw.Poly n)
            else
                gl#set_highlighted GlFlatDraw.No_Highlight
        else if tool = buttonline then
            GeomEdit.start_line x y map (highlight_distance ())
        else if tool = buttonfill then
            GeomEdit.fill_poly (int_of_float x) (int_of_float y) map
        else ()
    |GlFlatDraw.Draw, 3, _, _, _ ->
        if tool = buttonarrow then
            if point_d < highlight_distance () then
                MapDialogs.point_dialog (Array.get (map#get_points_array ()) point_i)
            else if obj_d < highlight_distance () then
                MapDialogs.obj_dialog (Array.get (map#get_objs_array ()) obj_i)
            else if line_d < highlight_distance () then
                MapDialogs.line_dialog (Array.get (map#get_lines_array ()) line_i) map
            else if poly != None then let Some n = poly in
                MapDialogs.poly_dialog (Array.get (map#get_polygons_array ()) n) map
        else ()
    |_ -> () end;
    gl#draw ();
    false

(* this gets called when we're dragging a tool around *)
let tool_in_event motion_descriptor =
    let x = GdkEvent.Motion.x motion_descriptor in
    let y = GdkEvent.Motion.y motion_descriptor in
    let old_x = !x1 in
    let old_y = !y1 in
    let tool = active_tool () in
    begin match gl#mode () with
    |GlFlatDraw.Draw ->
        if tool = buttonpan then
            let horig = hadj#value +. old_x in
            let vorig = vadj#value +. old_y in
            hadj#set_value (horig -. x);
            vadj#set_value (vorig -. y)
        else if tool = buttonarrow then
            let delta_x = (x -. old_x) /. (gl#get_zoom ()) in
            let delta_y = (y -. old_y) /. (gl#get_zoom ()) in
            let shift_point p =
                let point = Array.get (map#get_points_array ()) p in
                let (px, py) = point#vertex () in
                point#set_vertex (int_of_float (float px +. delta_x),
                                  int_of_float (float py +. delta_y));
                map#recalculate_lengths p in
            let shift_line line =
                let (p0, p1) = line#endpoints () in
                shift_point p0;
                shift_point p1 in
            let rec shift_poly poly n =
                if n = poly#vertex_count () then () else
                let index = Array.get (poly#endpoint_indices ()) n in
                shift_point index;
                shift_poly poly (n+1) in
            let shift_obj obj =
                let (x, y, z) = obj#point () in
                obj#set_point (int_of_float (float x +. delta_x),
                               int_of_float (float y +. delta_y), z) in
            begin match gl#highlighted () with
                |GlFlatDraw.Point n ->
                    shift_point n
                |GlFlatDraw.Line n ->
                    shift_line (Array.get (map#get_lines_array ()) n)
                |GlFlatDraw.Poly n ->
                    let poly = Array.get (map#get_polygons_array ()) n in
                    shift_poly poly 0
                |GlFlatDraw.Object n ->
                    shift_obj (Array.get (map#get_objs_array ()) n)
                |_ -> () end;
            gl#draw ()
        else if tool = buttonline then
            let (x, y) = gl#to_map_coords x y in
            GeomEdit.draw_line x y map gl
        else ()
    |_ -> () end;
    x1 := x;
    y1 := y;
    false

(* and this gets called when we release the mouse button and apply the tool *)
let tool_end_event mouse_descriptor =
    let tool = active_tool () in
    let button = GdkEvent.Button.button mouse_descriptor in
    let x = GdkEvent.Button.x mouse_descriptor in
    let y = GdkEvent.Button.y mouse_descriptor in
    begin match gl#mode () with
    |GlFlatDraw.Draw ->
        if tool = buttonarrow then
            match gl#highlighted () with GlFlatDraw.Object n -> begin
                let obj = Array.get (map#get_objs_array ()) n in
                let (x, y, _) = obj#point () in
                let poly = map#get_enclosing_poly (float x) (float y) in
                match poly with
                    |None -> map#delete_obj n
                    |Some a -> obj#set_polygon a end
            |_ -> ()
        else if tool = buttonzoom then
            match button with
                |1 -> zoom_in ()
                |3 -> zoom_out ()
                |_ -> ()
        else if tool = buttonline then
            let (x, y) = gl#to_map_coords x y in
            GeomEdit.connect_line x y map gl (highlight_distance ())
        else ()
    |_ -> () end;
    false

(* TODO: make this event driven *)
let handle_keys key =
    let state = GdkEvent.Key.state key in
    let keyval = GdkEvent.Key.keyval key in
    let choose_button b =
        toolbar_clicked b;
        b#clicked () in
    begin match keyval with
        |97 -> choose_button buttonarrow
        |108 -> choose_button buttonline
        |112 -> choose_button buttonpoly
        |102 -> choose_button buttonfill
        |104 -> choose_button buttonpan (* h is for hand *)
        |122 -> choose_button buttonzoom
        |116 -> choose_button buttontext
        |111 -> choose_button buttonobj
        |65535
        |65288 -> GeomEdit.delete gl map
        |_   -> () end;
    false

let change_mode box entry buttons button_text1 button_text2 label_text mode =
    if box then toolbar#show () else toolbar#misc#hide ();
    if entry then entry_toolbar#show () else entry_toolbar#misc#hide ();
    if buttons then mediabox#misc#show () else mediabox#misc#hide ();
    entry_label#set_text label_text;
    newbutton#set_label button_text1;
    editbutton#set_label button_text2;
    gl#set_mode mode
let to_draw_mode () =
    change_mode true false false "" "" "" GlFlatDraw.Draw
let to_floor_height_mode () =
    change_mode false true false "" "" "Height:" GlFlatDraw.Floor_Height
let to_ceiling_height_mode () =
    change_mode false true false "" "" "Height:" GlFlatDraw.Ceiling_Height
let to_media_light_mode () =
    change_mode false true true "New Light..." "Edit Light..." "Light:" GlFlatDraw.Media_Light
let to_floor_light_mode () =
    change_mode false true true "New Light..." "Edit Light..." "Light:" GlFlatDraw.Floor_Light
let to_ceiling_light_mode () =
    change_mode false true true "New Light..." "Edit Light..." "Light:" GlFlatDraw.Ceiling_Light
let to_media_mode () =
    change_mode false true true "New Media..." "Edit Media..." "Media:" GlFlatDraw.Media

(* menus, to be passed to GToolkit.  not really stylistically proper to be
 * defined in this file at this location, but because the pairs have to refer to
 * the helper functions, I can't think of a better place to put them for the
 * moment.  I'll figure it out eventually. *)
let file_menu_toolkit =
    [`I ("_New Level...", new_map);
     `I ("_Open...", FileDialogs.open_file_dialog map set_title gl);
     `I ("_Save Level", FileDialogs.silent_save map set_title);
     `I ("Save Level _As...", FileDialogs.save_file_dialog map set_title);
     `S;
     `I ("_Merge Levels...", CamlExt.id);
     `I ("_Export Level...", CamlExt.id);
     `S;
     `I ("_Quit", quit_application)]

let view_menu_toolkit =
    [`I ("_Draw Mode", to_draw_mode);
     `I ("_Visual Mode", CamlExt.id);
     `S;
     `M ("_Elevation", [`I ("Floor", to_floor_height_mode);
                        `I ("Ceiling", to_ceiling_height_mode)]);
     `M ("_Textures",  [`I ("Floor", CamlExt.id); `I ("Ceiling", CamlExt.id);]);
     `I ("_Polygon Types", CamlExt.id);
     `S;
     `M ("_Lights",    [`I ("Floor", to_floor_light_mode);
                        `I ("Ceiling", to_ceiling_light_mode);
                        `I ("Liquids", to_media_light_mode)]);
     `I ("Li_quids", to_media_mode);
     `M ("_Sounds",    [`I ("Ambient Sounds", CamlExt.id);
                       `I ("Random Sounds", CamlExt.id)])]

let special_menu_toolkit =
    [`I ("Zoom In", zoom_in);
     `I ("Zoom Out", zoom_out);
     `S;
     `I ("Map M_anager", MapDialogs.map_manager gl);
     `I ("View _Height Window", CamlExt.id);
     `I ("_Goto...", CamlExt.id);
     `S;
     `I ("Set _Level Parameters...", MapDialogs.info_dialog map);
     `I ("Set _Item Parameters...", CamlExt.id);
     `I ("Set _Monster Parameters...", CamlExt.id);
     `I ("Edit Map Item _Parameters...", CamlExt.id);
     `S;
     `I ("_Recenter Level", CamlExt.id);
     `I ("Pave Level", CamlExt.id);
     `I ("Nuke Objects Only...", CamlExt.id);
     `I ("Nuke and Pave Level...", CamlExt.id)]

(* entry point for the application proper *)
let _ =
    window#event#connect#delete ~callback:deleting_window;
    window#connect#destroy ~callback:quit_application;
    window#event#connect#key_press ~callback:handle_keys;
    eventbox#event#connect#button_press ~callback:tool_begin_event;
    eventbox#event#connect#button_release ~callback:tool_end_event;
    eventbox#event#connect#motion_notify ~callback:tool_in_event;
    numeric_entry#connect#changed entry_callback;
    editbutton#connect#clicked edit_current_item;
    newbutton#connect#clicked make_new_item;
    gl#set_map map;
    GToolbox.build_menu file_menu ~entries:file_menu_toolkit;
    GToolbox.build_menu view_menu ~entries:view_menu_toolkit;
    GToolbox.build_menu special_menu ~entries:special_menu_toolkit;
    window#show ();
    List.iter
        (fun obj -> ignore (obj#event#connect#button_press ~callback:
            (fun _ -> toolbar_clicked obj; false))) buttons;
    toolbar#show ();
    let args = Sys.argv in
    if Array.length args > 1 then
        map#read_from_file (Array.get args (Array.length args - 1));
    GMain.Main.main ()
