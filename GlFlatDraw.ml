(*** GlFlatDraw.ml contains the object (which contains the routines) that
 * control the updating of the overhead map surface.  ***)

open CamlExt
open Colors

(** GTK toolbar that we own for this mode **)
let toolbar = GWindow.window ~type_hint:`TOOLBAR ~title:"Smithy Toolkit"
                             ~show:false ()
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
let _ = GMisc.pixmap (GDraw.pixmap_from_xpm Resources.arrowfile ())
                     ~packing:buttonarrow#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.linefile ())
                     ~packing:buttonline#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.polyfile ())
                     ~packing:buttonpoly#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.fillfile ())
                     ~packing:buttonfill#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.panfile ())
                     ~packing:buttonpan#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.zoomfile ())
                     ~packing:buttonzoom#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.textfile ())
                     ~packing:buttontext#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.objfile ())
                     ~packing:buttonobj#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.arrowfile ())
                     ~packing:buttonarrow#add () |> ignore

let active_tool () =
    let rec a_t_aux lst =
        if (List.hd lst)#active
            then List.hd lst
            else a_t_aux (List.tl lst) in
    a_t_aux buttons
    
let toolbar_clicked which _ =
    List.iter (fun x -> x#set_active false) buttons;
    false

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
(** end GTK instantiation **)

(* we keep track of what item is highlighted as part of the drawing / interface
 * object, and we have an enumerative type to match across *)
type highlighted_component = [`No_Highlight       |
                              `Point of int list  |
                              `Line of int list   |
                              `Poly of int list   |
                              `Object of int list ]

(* the drawing object has a bunch of different states that it can be in that
 * affects what information is displayed, here we have an enumerative type that
 * describes the modes *)
type renderer_mode = Draw | Floor_Height | Ceiling_Height | Media |
                     Floor_Light | Ceiling_Light | Media_Light | Polygon_Type

open MapFormat

(* a helper utility to draw a polygon point ring *)
let draw_poly poly_ring =
    let rec d_p_aux poly_ring =
        let (x, y) = List.hd poly_ring in
        GlDraw.vertex2 (float x, float y);
        match poly_ring with
            |x :: [] -> GlDraw.ends ()
            |x :: xs -> d_p_aux xs
            |_ -> () in
    GlDraw.begins `polygon;
    d_p_aux poly_ring

(* checks a point ring for concavity *)
let vertex_array_is_concave vertices =
    let length = List.length vertices in
    (* this runs a generic comparison function on the cross products of adjacent
     * lines, returns true if all the comparisons pass and false otherwise *)
    let rec loop_compare vertices comp n =
        if n = length then true else begin
        let next1 = (if n = length - 1 then 0 else n + 1) in
        let next2 = (if next1 = length - 1 then 0 else next1 + 1) in
        let (p0x, p0y) = List.nth vertices n in
        let (p1x, p1y) = List.nth vertices next1 in
        let (p2x, p2y) = List.nth vertices next2 in
        let p0 = (p1x - p0x, p1y - p0y) in
        let p1 = (p2x - p1x, p2y - p1y) in
        if comp (CamlExt.cross p0 p1) then
            loop_compare vertices comp (n+1)
        else
            false end in
    (* if all the crosses are nonpositive, we have a clockwise point loop *)
    let vertex_array_is_cw vertices =
        loop_compare vertices (fun x -> x <= 0) 0 in
    (* if all the crosses are nonnegative, we have a ccw point loop *)
    let vertex_array_is_ccw vertices =
        loop_compare vertices (fun x -> x >= 0) 0 in
    (* and we want to test for loops that are neither completely cw nor ccw *)
    not (vertex_array_is_cw vertices) && not (vertex_array_is_ccw vertices)

(** this is the generic drawing / interface class **)
class gldrawer (ar:GlGtk.area)
               (vadj: GData.adjustment)
               (hadj: GData.adjustment) = object (self)
    (*** vars ***)
    (* store translational state data *)
    val mutable height = 0.0
    val mutable width = 0.0
    val mutable voffset = 0.0
    val mutable hoffset = 0.0

    (* store zoom and zoom-like state data *)
    val mutable zoom_factor = 1.0
    val mutable grid_factor = 3

    (* store a reference to the map geometry object *)
    val mutable map = new MapFormat.map
    method set_map x = map <- x

    (* and store mode information *)
    val mutable highlighted_component = `No_Highlight
    val mutable mode = Draw

    (*** methods ***)
(* depending upon what mode we're in, launch the appropriate dialog *)
    method private edit_current_item () =
        match (int_of_string numeric_entry#text, mode) with
        |(-1, _) -> ()
        |(index, Media) ->
            let media = map#get_media_array () in
            MapDialogs.media_dialog media.(index)
        |(index, Floor_Light)
        |(index, Media_Light)
        |(index, Ceiling_Light) ->
            let lights = map#get_lights_array () in
            MapDialogs.light_dialog lights.(index)
        |_ -> ()

(* depending upon what mode we're in, spawn in a new light/media/whatever and
 * open the editor so that we can customize it *)
    method private make_new_item () =
        begin match mode with
            |Media ->
                let n = MapDialogs.make_media map in
                numeric_entry#set_text (string_of_int n)
            |Floor_Light
            |Media_Light
            |Ceiling_Light ->
                let l = MapDialogs.make_light map in
                numeric_entry#set_text (string_of_int l)
            |_ -> () end;
        self#draw ()

    method init () =
        GlDraw.line_width 1.0;
        GlClear.color Colors.background_color;
        Gl.disable `depth_test;
        self#reortho ()

(* initialize the opengl renderer, add callbacks *)
    initializer
        GlDraw.line_width 1.0;
        GlDraw.shade_model `smooth;
        GlClear.color background_color;
        Gl.disable `depth_test;
        List.iter Gl.enable [`line_smooth; `point_smooth];
        vadj#connect#value_changed (fun _ -> self#set_voffset (vadj#value));
        hadj#connect#value_changed (fun _ -> self#set_hoffset (hadj#value));
        self#reortho ();
        (* also set up the toolbars that we own *)
        editbutton#connect#clicked self#edit_current_item;
        newbutton#connect#clicked self#make_new_item;
        List.iter (fun obj ->
            ignore (obj#event#connect#button_press ~callback:
                (toolbar_clicked obj))) buttons

(* this gets called when the gtk drawing surface gets resized or when the
 * scrollbars get moved so we can update our ortho coordinates *)
    method reshape w h =
        width <- w;
        height <- h;
        self#reortho ()

(* when our perspective over the map changes, we call reortho to tell OpenGL to
 * move with our program state *)
    method reortho () =
        GlMat.mode `modelview;
        GlMat.load_identity ();
        GlDraw.viewport 0 0 (int_of_float width) (int_of_float height);
        GlMat.mode `projection;
        GlMat.load_identity ();
        GlMat.ortho (0.0 +. hoffset, width +. hoffset)
                    (height +. voffset, 0.0 +. voffset)
                    (-1.0, 0.0);
        GlMat.scale ~x:zoom_factor ~y:zoom_factor ~z:1.0 ();
        self#draw ()

(* draws the background grid lines *)
    method private draw_grid_lines () =
        let rec grid_loop index bound spacing =
            if index > bound then () else begin
            let pos = spacing *. (float index) in
            GlDraw.color grid_color;
            GlDraw.vertex2 (pos, -32768.0);
            GlDraw.vertex2 (pos, 32768.0);
            GlDraw.ends ();
            GlDraw.begins `lines;
            GlDraw.vertex2 (-32768.0, pos);
            GlDraw.vertex2 (32768.0, pos);
            grid_loop (index + 1) bound spacing end in
        let grid_count = 32 * (CamlExt.pow 2 grid_factor) in
        GlDraw.line_width 1.0;
        GlDraw.begins `lines;
        grid_loop (-grid_count) grid_count
            (32768.0 /. (32.0 *. (2.0 ** (float grid_factor))));
        GlDraw.ends ()

(* draws the background grid points *)
    method private draw_grid_points () =
        let rec grid_point_loop index1 index2 bound spacing =
            GlDraw.vertex2 ((float index1 *. spacing), (float index2 *. spacing));
            if index1 <= bound then
                grid_point_loop (index1+1) index2 bound spacing
            else if index2 <= bound then
                grid_point_loop (-32) (index2+1) bound spacing
            else () in
        GlDraw.point_size 2.0;
        GlDraw.color anchor_point_color;
        GlDraw.begins `points;
        grid_point_loop (-64) (-64) 64 (map_width /. 64.0);
        GlDraw.ends ()

(* controls the drawing of the background grid *)
    method private draw_grid () =
        self#draw_grid_lines ();
        self#draw_grid_points ()

(* draws the map points *)
    method private draw_points () =
        let draw_point p =
            let (vx, vy) = p#vertex () in
            GlDraw.vertex2 (float vx, float vy) in
        let points = map#get_points_array () in
        GlDraw.point_size 2.0;
        GlDraw.color point_color;
        GlDraw.begins `points;
        Array.iter draw_point points;
        GlDraw.ends ()

(* draws the map lines *)
    method private draw_lines () =
        let points = map#get_points_array () in
        let draw_line line =
            let (v1, v2) = line#endpoints () in
            let p1 = points.(v1) in
            let p2 = points.(v2) in
            let (x1, y1) = p1#vertex () in
            let (x2, y2) = p2#vertex () in
            if List.mem MapTypes.TRANSPARENT (line#flags ()) then
                GlDraw.color transparent_line_color
            else
                GlDraw.color line_color;
            GlDraw.vertex2 (float x1, float y1);
            GlDraw.vertex2 (float x2, float y2) in
        let lines = map#get_lines_array () in
        GlDraw.line_width 1.0;
        GlDraw.begins `lines;
        Array.iter draw_line lines;
        GlDraw.ends ()

(* draws the map polys *)
    method private draw_polys () =
        let polygons = map#get_polygons_array () in
        let poly_count = Array.length polygons in
        let points = map#get_points_array () in
        (* some modes require a singular precalculation of some kind, typically
         * the number of distinct types of something *)
        let count =
            match mode with
                |Media ->
                    let rec count_media i acci accl =
                        if i = poly_count then acci else
                        let m = polygons.(i)#media_index () in
                        if List.mem m accl
                            then count_media (i+1) acci accl
                            else count_media (i+1) (acci+1) (m :: accl) in
                    count_media 0 0 []
                |Floor_Light |Ceiling_Light |Media_Light ->
                    Array.length (map#get_lights_array ())
                |_ -> 0 in
        (* render_fn is the function that gets called during the poly iteration,
         * and which rendering mode can have its own render function.  TODO:
         * this code is bulky and repetitive, local lets would slim it down
         * considerably *)
        let render_fn =
            match mode with
                |Draw -> (fun x ->
                    (* shade based on concavity *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    let concave = vertex_array_is_concave vertex_array in
                    if concave then GlDraw.color invalid_polygon
                        else GlDraw.color polygon_color;
                    draw_poly vertex_array)
                |Media_Light -> (fun x ->
                    (* shade based on media light *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    let color = match (x#media_index (), x#media_lightsource ()) with
                        (_, -1) |(-1, _) -> (0.5, 0.5, 0.5)
                        |         (_, l) -> (float l /. (float count), 0.0, 0.0) in
                    GlDraw.color color;
                    draw_poly vertex_array)
                |Floor_Light -> (fun x ->
                    (* shade based on floor light *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    let color = match x#floor_lightsource () with
                        (-1) -> (0.5, 0.5, 0.5)
                          |l -> (float l /. (float count), 0.0, 0.0) in
                    GlDraw.color color;
                    draw_poly vertex_array)
                |Ceiling_Light -> (fun x ->
                    (* shade based on ceiling light *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    let color = match x#ceiling_lightsource () with
                        (-1) -> (0.5, 0.5, 0.5)
                          |l -> (float l /. (float count), 0.0, 0.0) in
                    GlDraw.color color;
                    draw_poly vertex_array)
                |Media -> (fun x ->
                    (* shade based on media index *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    let color = match x#media_index () with
                        (-1) -> (0.5, 0.5, 0.5)
                          |m -> (float m /. (float count), 0.0, 0.0) in
                    GlDraw.color color;
                    draw_poly vertex_array)
                |Ceiling_Height -> (fun x ->
                    (* shade based on ceiling height *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    (* convert to an rgb-range value *)
                    let height = x#ceiling_height () /. 18. +. 0.5 in
                    GlDraw.color (height, height, height);
                    draw_poly vertex_array)
                |Floor_Height -> (fun x ->
                    (* shade based on floor height *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    (* convert to an rgb-range value *)
                    let height = x#floor_height () /. 18. +. 0.5 in
                    GlDraw.color (height, height, height);
                    draw_poly vertex_array)
                |Polygon_Type -> (fun x ->
                    (* shade based on polygon type *)
                    let vertex_array = List.map (fun x ->
                        (map#get_points_array ()).(x)#vertex ()) (map#get_poly_ring x) in
                    let kind = float (CamlExt.to_enum MapTypes.poly_kind_descriptor
                        (x#kind ())) in
                    let length = float (List.length
                        ((fun (_, x) -> x) MapTypes.poly_kind_descriptor)) in
                    let color = CamlExt.hsv_to_rgb (kind /. length,
                                                    poly_type_saturation,
                                                    poly_type_value) in
                    GlDraw.color color;
                    draw_poly vertex_array)
                |_ -> (fun x -> ()) in (* ill-defined mode! *)
        (* actually do the iteration *)
        Array.iter render_fn polygons

(* draw all the objects *)
(* TODO: use those TGAs instead of uninformative green blips! *)
    method private draw_objs () =
        let objs = map#get_objs_array () in
        GlDraw.color (0.0, 1.0, 0.0);
        GlDraw.begins `points;
        Array.iter (fun x ->
            let (x, y, z) = x#point () in
            GlDraw.vertex2 (float x, float y)) objs;
        GlDraw.ends ()

(* draws the highlight on the selected geometry element *)
    method private draw_highlights () =
        GlDraw.color highlight_color;
        match highlighted_component with
            |`Point n ->
                List.iter (fun n ->
                    let (x, y) = (map#get_points_array ()).(n)#vertex () in
                    GlDraw.point_size 5.0;
                    GlDraw.begins `points;
                    GlDraw.vertex2 (float x, float y);
                    GlDraw.ends ()) n
            |`Line n ->
                List.iter (fun n ->
                    let line = (map#get_lines_array ()).(n) in
                    let (p0, p1) = line#endpoints () in
                    let (x0, y0) = (map#get_points_array ()).(p0)#vertex () in
                    let (x1, y1) = (map#get_points_array ()).(p1)#vertex () in
                    GlDraw.line_width 4.0;
                    GlDraw.begins `lines;
                    GlDraw.vertex2 (float x0, float y0);
                    GlDraw.vertex2 (float x1, float y1);
                    GlDraw.ends ()) n
            |`Poly n ->
                List.iter (fun n ->
                    let poly = (map#get_polygons_array ()).(n) in
                    let poly_ring = map#get_poly_ring poly in
                    draw_poly (List.map
                        (fun x -> (map#get_points_array ()).(x)#vertex ())
                        poly_ring)) n
            |`No_Highlight
            |_ -> () (* this fallthrough case is for other highlighted things *)

(* does the actual map draw to the back buffer *)
    method draw_without_update () =
        (* clear the screen, draw the grid *)
        GlClear.color background_color;
        GlClear.clear [`color];
        self#draw_grid ();
        (* draw the static map geometry *)
        self#draw_polys ();
        self#draw_lines ();
        self#draw_points ();
        self#draw_objs ();
        (* draw the highlighted components *)
        self#draw_highlights ()

(* mnemonic to blit the back buffer to the visible surface *)
    method refresh () =
        Gl.flush ();
        ar#swap_buffers ()

(* joins the render and blit routines *)
    method draw () =
        (* update the back buffer *)
        self#draw_without_update ();
        (* blit to screen *)
        self#refresh ()

(* allows outsiders to set our horizontal scroll *)
    method set_hoffset h =
        hoffset <- h;
        self#reortho ()

(* allows outsiders to set our vertical scroll *)
    method set_voffset v =
        voffset <- v;
        self#reortho ()

(* allows outsiders to set our zoom level *)
    method set_zoom z =
        hadj#set_value (hadj#value /. zoom_factor *. z);
        vadj#set_value (vadj#value /. zoom_factor *. z);
        zoom_factor <- z;
        hadj#set_bounds ~lower:(half_map_width *. (0. -. z))
                        ~upper:(half_map_width *. z -. width) ();
        vadj#set_bounds ~lower:(half_map_width *. (0. -. z))
                        ~upper:(half_map_width *. z -. height) ()

(* set the zoom level and recenter the screen *)
    method set_zoom_with_center z x y =
        hadj#set_bounds ~lower:(half_map_width *. (0. -. z))
                        ~upper:(half_map_width *. z -. width) ();
        vadj#set_bounds ~lower:(half_map_width *. (0. -. z))
                        ~upper:(half_map_width *. z -. height) ();
        let h = ((hadj#value +. x) /. zoom_factor *. z) -. x in
        let v = ((vadj#value +. y) /. zoom_factor *. z) -. y in
        zoom_factor <- z;
        hadj#set_value h;
        vadj#set_value v

(* allows outsiders to get out zoom level *)
    method get_zoom () =
        zoom_factor

(* allows outsiders to tell us to use a particular map object for the geometry
 * data *)
    method set_map x =
        map <- x

(* get/set renderer state *)
    method mode () = mode
    method set_mode x =
        mode <- x;
        if x != Draw then highlighted_component <- `No_Highlight

(* convert a screen location to a map location, used for clicks *)
    method to_map_coords x y =
        ((x +. hadj#value) /. zoom_factor, (y +. vadj#value) /. zoom_factor)
    
(* get/set highlights *)
    method set_highlighted x =
        highlighted_component <- x;
        self#draw ()
    method highlighted () : highlighted_component =
        highlighted_component

(* get/set the number of grid lines drawn *)
    method set_grid_factor x =
        grid_factor <- x
    method grid_factor () =
        grid_factor

(* deal with zoom events *)
    method zoom_in () =
        let (x, y) = self#to_map_coords (width /. 2.0) (height /. 2.0) in
        self#set_zoom_with_center (zoom_factor *. 2.0) x y
    method zoom_out () =
        let (x, y) = self#to_map_coords (width /. 2.0) (height /. 2.0) in
        self#set_zoom_with_center (zoom_factor *. 0.5) x y

(* and an event utility, note that the 4.0 is actually configurable *)
    method highlight_distance () =
        let pixel_epsilon = 4.0 in
        pixel_epsilon /. zoom_factor

(* this gets called when we start applying a tool *)
    method tool_begin_event x y button (state: Gdk.Tags.modifier list) =
        (* unwrap values actually useful to us *)
        let tool = active_tool () in
    (* get nearby objects, since tools frequently need them *)
    let (x, y) = self#to_map_coords x y in
    let (point_d, point_i) = map#get_closest_point x y in
    let (line_d, line_i) = map#get_closest_line x y in
    let poly = map#get_enclosing_poly x y in
    let (obj_d, obj_i) = map#get_closest_object x y in
    (* biiiiig switch statement that selects what exactly we want to be doing
     * with this mouse click *)
    let int_entry = try Some (int_of_string numeric_entry#text) with _ -> None in
    let float_entry = try Some (float_of_string numeric_entry#text) with _ -> None in
    begin match (mode, button, int_entry, float_entry, poly) with
    (* a bunch of these get and set media/light/height/whatever attributes *)
    |Media_Light, 1, Some v, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        poly#set_media_lightsource v
    |Media_Light, 3, _, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        let v = poly#media_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |Ceiling_Light, 1, Some v, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        poly#set_ceiling_lightsource v
    |Ceiling_Light, 3, _, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        let v = poly#ceiling_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |Floor_Light, 1, Some v, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        poly#set_floor_lightsource v
    |Floor_Light, 3, _, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        let v = poly#floor_lightsource () in
        numeric_entry#set_text (string_of_int v)
    |Ceiling_Height, 1, _, Some v, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        poly#set_ceiling_height v
    |Ceiling_Height, 3, _, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        let v = poly#ceiling_height () in
        numeric_entry#set_text (string_of_float v)
    |Floor_Height, 1, _, Some v, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        poly#set_floor_height v
    |Floor_Height, 3, _, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        let v = poly#floor_height () in
        numeric_entry#set_text (string_of_float v)
    |Media, 1, Some v, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        poly#set_media_index v
    |Media, 3, _, _, Some p ->
        let poly = (map#get_polygons_array ()).(p) in
        let v = poly#media_index () in
        numeric_entry#set_text (string_of_int v)
    |Draw, 1, _, _, _ ->
        (* in draw mode, we have to deal with what kind of tool to apply *)
        if tool = buttonarrow then
            (* see TODO list about things pertaining to this that need to be
             * fixed; there are quite a few. *)
            match List.mem `SHIFT state, highlighted_component with
            (* the arrow tool selects things on mouse down, and multiple things
             * when shift is being held *)
            |true, `Point n when point_d < self#highlight_distance () ->
                    highlighted_component <- `Point (point_i :: n)
            |_ when point_d < self#highlight_distance () ->
                    highlighted_component <- `Point [point_i]
            |true, `Object n when obj_d < self#highlight_distance () ->
                    highlighted_component <- `Object (obj_i :: n)
            |_ when obj_d < self#highlight_distance () ->
                    highlighted_component <- `Object [obj_i]
            |true, `Line n when line_d < self#highlight_distance () ->
                    highlighted_component <- `Line (line_i :: n)
            |_ when line_d < self#highlight_distance () ->
                    highlighted_component <- `Line [line_i]
            |true, `Poly m when poly != None ->
                    let Some n = poly in
                    highlighted_component <- `Poly (n :: m)
            |_ when poly != None ->
                    let Some n = poly in
                    highlighted_component <- `Poly [n]
            |_ ->
                    highlighted_component <- `No_Highlight
        (* the line tool draws lines *)
        else if tool = buttonline then
            GeomEdit.start_line x y map (self#highlight_distance ())
        (* and the fill tool fills line loops with polygons *)
        else if tool = buttonfill then
            GeomEdit.fill_poly (int_of_float x) (int_of_float y) map
        else ()
    |Draw, 3, _, _, _ ->
        if tool = buttonarrow then
            (* a right-click in draw mode with the arrow tool means we want to
             * inspect a map element *)
            if point_d < self#highlight_distance () then
                MapDialogs.point_dialog (map#get_points_array ()).(point_i)
            else if obj_d < self#highlight_distance () then
                MapDialogs.obj_dialog (map#get_objs_array ()).(obj_i)
            else if line_d < self#highlight_distance () then
                MapDialogs.line_dialog (map#get_lines_array ()).(line_i) map
            else if poly != None then let Some n = poly in
                MapDialogs.poly_dialog (map#get_polygons_array ()).(n) map
        else ()
    |_ -> () end;
    self#draw ();
    false

(* this gets called when we're dragging a tool around *)
    method tool_in_event ((x0, y0) : float*float)
                         ((old_x, old_y) : float*float)
                         ((x, y) : float*float) =
        (* extract values actually useful to us *)
        let tool = active_tool () in
        begin match mode with
        |Draw ->
            (* if we're panning, then pan *)
            if tool = buttonpan then
                let horig = hadj#value +. old_x in
                let vorig = vadj#value +. old_y in
                hadj#set_value (horig -. x);
                vadj#set_value (vorig -. y)
                (* if we're using the arrow, drag *)
            else if tool = buttonarrow then
                let delta_x = (x -. old_x) /. zoom_factor in
                let delta_y = (y -. old_y) /. zoom_factor in
                (* utilities for easy dragging *)
                let shift_point p =
                    let point = (map#get_points_array ()).(p) in
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
                        let index = (poly#endpoint_indices ()).(n) in
                        shift_point index;
                        shift_poly poly (n+1) in
                let shift_obj obj =
                    let (x, y, z) = obj#point () in
                    obj#set_point (int_of_float (float x +. delta_x),
                    int_of_float (float y +. delta_y), z) in
                    begin match highlighted_component with
                        |`Point n ->
                            List.iter (fun n -> shift_point n) n
                        |`Line n ->
                            List.iter (fun n ->
                                shift_line (map#get_lines_array ()).(n)) n
                        |`Poly n ->
                            List.iter (fun n -> 
                                let poly = (map#get_polygons_array ()).(n) in
                                shift_poly poly 0) n
                        |`Object n ->
                            List.iter (fun n ->
                                shift_obj (map#get_objs_array ()).(n)) n
                        |_ -> () end;
                self#draw ()
                else if tool = buttonline then
                    (* and if we're drawing a line, keep drawing its intermediates *)
                    let (x, y) = self#to_map_coords x y in
                    (* TODO: does draw_line really want access to the lower drawer? *)
                    GeomEdit.draw_line x y map self
                else ()
            |_ -> () end;
        false

(* and this gets called when we release the mouse button and apply the tool *)
    method tool_end_event ((x0, y0): float*float)
                          ((x, y): float*float)
                          (button: int) =
        let tool = active_tool () in
        begin match mode with
        |Draw ->
            if tool = buttonarrow then
                (* if we were dragging around objects, be sure to place them in the
                 * appropriate polygon! *)
                match highlighted_component with
                    |`Object n ->
                        List.iter (fun n ->
                            let obj = (map#get_objs_array ()).(n) in
                            let (x, y, _) = obj#point () in
                            let poly = map#get_enclosing_poly (float x) (float y) in
                            match poly with
                                |None -> map#delete_obj n
                                |Some a -> obj#set_polygon a) n
                    |_ -> ()
            else if tool = buttonzoom then
                (* if we were using the zoom tool, apply the zoom *)
                match button with
                |1 -> self#set_zoom_with_center (zoom_factor *. 2.0) x y
                |3 -> self#set_zoom_with_center (zoom_factor *. 0.5) x y
                |_ -> ()
            else if tool = buttonline then
                (* if we were drawing a line, finalize it *)
                let (x, y) = self#to_map_coords x y in
                (* TODO: does connect_line really need access to gldrawer? *)
                GeomEdit.connect_line x y map self (self#highlight_distance ())
            else ()
        |_ -> () end;
        false

(* handles keyboard shortcuts *)
(* TODO: make this event driven *)
    method handle_key key =
        let choose_button b =
            toolbar_clicked b ();
            b#clicked () in
        begin match key with
            |97  -> choose_button buttonarrow
            |108 -> choose_button buttonline
            |112 -> choose_button buttonpoly
            |102 -> choose_button buttonfill
            |104 -> choose_button buttonpan (* h is for hand *)
            |122 -> choose_button buttonzoom
            |116 -> choose_button buttontext
            |111 -> choose_button buttonobj
            (* TODO: does delete really need access to gldrawer? *)
            |65535 | 65288 -> GeomEdit.delete self map
            |_   -> () end;
        false
end
