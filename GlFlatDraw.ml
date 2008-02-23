(*** GlFlatDraw.ml contains the object (which contains the routines) that
 * control the updating of the overhead map surface.  ***)

(** colors!
 * TODO: make configurable and move somewhere else **)
(* these define what color things get painted while in draw mode *)
let background_color       = (0.3, 0.3, 0.3)
let grid_color             = (0.5, 0.5, 0.5)
let anchor_point_color     = (0.3, 0.8, 0.8)
let polygon_color          = (1.0, 1.0, 1.0)
let point_color            = (1.0, 0.0, 0.0)
let line_color             = (0.0, 0.0, 0.0)
let transparent_line_color = (0.0, 1.0, 1.0)
let highlight_color        = (1.0, 0.5, 0.0)
let invalid_polygon        = (1.0, 0.5, 0.5)

(* these define S and V in the HSV calculation while in poly type edit mode *)
let poly_type_saturation = 0.5
let poly_type_value = 0.5
(** end colors! **)

(* we keep track of what item is highlighted as part of the drawing / interface
 * object, and we have an enumerative type to match across *)
type highlighted_component = No_Highlight | Point of int list |
                             Line of int list | Poly of int list |
                             Object of int list

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

    (* and store mode information *)
    val mutable highlighted_component = No_Highlight
    val mutable mode = Draw

    (*** methods ***)
(* initialize the opengl renderer, add callbacks *)
    initializer
        GlDraw.line_width 1.0;
        GlDraw.shade_model `smooth;
        GlClear.color background_color;
        Gl.disable `depth_test;
        List.iter Gl.enable [`line_smooth; `point_smooth];
        vadj#connect#value_changed (fun _ -> self#set_voffset (vadj#value));
        hadj#connect#value_changed (fun _ -> self#set_hoffset (hadj#value));
        self#reortho ()

(* this gets called when the gtk drawing surface gets resized or when the
 * scrollbars get moved so we can update our ortho coordinates *)
    method reshape w h =
        width <- w;
        height <- h;
        self#reortho ()

(* when our perspective over the map changes, we call reortho to tell OpenGL to
 * move with our program state *)
    method reortho () =
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
            |Point n ->
                List.iter (fun n ->
                    let (x, y) = (map#get_points_array ()).(n)#vertex () in
                    GlDraw.point_size 5.0;
                    GlDraw.begins `points;
                    GlDraw.vertex2 (float x, float y);
                    GlDraw.ends ()) n
            |Line n ->
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
            |Poly n ->
                List.iter (fun n ->
                    let poly = (map#get_polygons_array ()).(n) in
                    let poly_ring = map#get_poly_ring poly in
                    draw_poly (List.map
                        (fun x -> (map#get_points_array ()).(x)#vertex ())
                        poly_ring)) n
            |No_Highlight
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
        if x != Draw then highlighted_component <- No_Highlight

(* convert a screen location to a map location, used for clicks *)
    method to_map_coords x y =
        ((x +. hadj#value) /. zoom_factor, (y +. vadj#value) /. zoom_factor)
    
(* get/set highlights *)
    method set_highlighted x =
        highlighted_component <- x;
        self#draw ()
    method highlighted () =
        highlighted_component

(* get/set the number of grid lines drawn *)
    method set_grid_factor x =
        grid_factor <- x
    method grid_factor () =
        grid_factor
end
