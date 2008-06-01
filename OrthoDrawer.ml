class orthoDrawer ?width:explicit_width ?height:explicit_height
                  ?packing:(packing = ignore) () =
object (self)
    (* widgets *)
    val mutable eventbox = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable drawable_onscreen = None
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()

    (* actual data *)
    val mutable mousedown_callback =
        (fun x y button state -> ())
    val mutable mouseup_callback =
        (fun x0 y0 x1 y1 button state -> ())
    val mutable mousedrag_callback =
        (fun x0 y0 x1 y1 x2 y2 -> ())
    val mutable draw_callback =
        (fun _ -> ())
    val mutable resize_callback =
        (fun width height -> ())
    val mutable scroll_callback =
        (fun dx dy -> ())
    val mutable click0 = 0, 0
    val mutable click1 = 0, 0
    val mutable origin = 0, 0
    val mutable scale = 0.1

    (* dimension information *)
    val mutable width = 0
    val mutable height = 0

    (* accessors *)
    method connect_mousedown f = mousedown_callback <- f
    method connect_mouseup f = mouseup_callback <- f
    method connect_mousedrag f = mousedrag_callback <- f
    method connect_draw f = draw_callback <- f
    method connect_resize f = resize_callback <- f
    method connect_scroll f = scroll_callback <- f
    method size = drawable#size
    method origin = origin
    method set_origin x = origin <- x
    method scale = scale
    method set_scale x = scale <- x
    method widget = eventbox#coerce

    (* constructor *)
    initializer
        begin match explicit_width, explicit_height with
        |None, None ->
            eventbox <- GBin.event_box ~packing ();
            area <- GMisc.drawing_area ~packing:eventbox#add ()
        |Some width, None ->
            eventbox <- GBin.event_box ~width ~packing ();
            area <- GMisc.drawing_area ~width ~packing:eventbox#add ()
        |None, Some height ->
            eventbox <- GBin.event_box ~height ~packing ();
            area <- GMisc.drawing_area ~height ~packing:eventbox#add ()
        |Some width, Some height ->
            eventbox <- GBin.event_box ~width ~height ~packing ();
            area <- GMisc.drawing_area ~width ~height ~packing:eventbox#add ()
        end;
        area#event#add [`BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
                        `STRUCTURE; `EXPOSURE; `SCROLL; `POINTER_MOTION_HINT];
        ignore (eventbox#event#connect#motion_notify
                    ~callback:self#mousedrag_callback);
        ignore (eventbox#event#connect#button_press
                    ~callback:self#mousedown_callback);
        ignore (eventbox#event#connect#button_release
                    ~callback:self#mouseup_callback);
        ignore (area#event#connect#configure ~callback:self#resize_callback);
        ignore (area#event#connect#expose ~callback:self#draw_callback);
        ignore (area#event#connect#scroll ~callback:self#scroll_callback);
        ()

    (* event callbacks *)
    method private resize_callback geom_descriptor =
        let new_width = GdkEvent.Configure.width geom_descriptor in
        let new_height = GdkEvent.Configure.height geom_descriptor in
        resize_callback new_width new_height;
        drawable_onscreen <- None;
        false
    method private draw_callback _ =
        begin match drawable_onscreen with
        |None ->
            area#misc#realize ();
            drawable_onscreen <- Some (new GDraw.drawable (area#misc#window));
            let Some drawable_onscreen = drawable_onscreen in
            let explicit_width =
                (match explicit_width  with None -> 0 | Some n -> n) in
            let explicit_height =
                (match explicit_height with None -> 0 | Some n -> n) in
            let win_width, win_height = drawable_onscreen#size in
            width <- max win_width explicit_width;
            height <- max win_height explicit_height;
            buffer <- GDraw.pixmap ~width ~height ();
            drawable <- new GDraw.drawable (buffer#pixmap);
        |Some x -> ()
        end;
        let Some drawable_onscreen = drawable_onscreen in

        draw_callback ();
        drawable_onscreen#put_pixmap ~x:0 ~y:0 buffer#pixmap;
        false
    method private scroll_callback scroll_descriptor =
        (match GdkEvent.Scroll.direction scroll_descriptor with
        | `UP    -> scroll_callback   0.0 (-1.0)
        | `DOWN  -> scroll_callback   0.0   1.0
        | `LEFT  -> scroll_callback (-1.0)  0.0
        | `RIGHT -> scroll_callback   1.0   0.0);
        self#draw ();
        false
    method private mousedrag_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Motion.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Motion.y mouse_descriptor) in
        let x, y = self#to_map (x, y) in
        let (oldx, oldy) = click1 in
        click1 <- x, y;
        let x0, y0 = click0 in
        mousedrag_callback x0 y0 oldx oldy x y;
        self#draw ();
        false
    method private mousedown_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let x, y = self#to_map (x, y) in
        click0 <- x, y;
        click1 <- x, y;
        mousedown_callback x y button state;
        self#draw ();
        false
    method private mouseup_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let (x0, y0) = click0 in
        let x, y = self#to_map (x, y) in
        click1 <- x, y;
        mouseup_callback x0 y0 x y button state;
        self#draw ();
        false

    (* other public methods *)
    method to_screen (x, y) =
        let xo, yo = origin in
        (int_of_float (float (x - xo) *. scale),
            int_of_float (float (y - yo) *. scale))

    method to_map (x, y) =
        let xo, yo = origin in
        (int_of_float (float x /. scale) + xo,
            int_of_float (float y /. scale) + yo)

    method point (x, y) =
        let (x, y) = self#to_screen (x, y) in
        drawable#point ~x ~y

    method fat_point (x, y) thickness =
        let (x, y) = self#to_screen (x, y) in
        drawable#rectangle ~x:(x-thickness/2) ~y:(y-thickness/2)
                           ~width:thickness ~height:thickness ~filled:true ()

    method line (x0, y0) (x1, y1) =
        let (x0t, y0t) = self#to_screen (x0, y0) in
        let (x1t, y1t) = self#to_screen (x1, y1) in
        drawable#line ~x:x0t ~y:y0t ~x:x1t ~y:y1t

    method polygon filled points =
        let points = List.map self#to_screen points in
        drawable#polygon ~filled points

    method points points =
        let points = List.map self#to_screen points in
        drawable#points points

    method segments lines =
        let lines = List.map
            (fun (p0, p1) -> (self#to_screen p0, self#to_screen p1)) lines in
        drawable#segments lines

    method linear_curve points =
        let points = List.map self#to_screen points in
        drawable#lines points

    method set_color (r, g, b) =
        let r = int_of_float (r *. 65535.0) in
        let g = int_of_float (g *. 65535.0) in
        let b = int_of_float (b *. 65535.0) in
        drawable#set_foreground (`RGB (r, g, b))

    method clear () =
        let width, height = drawable#size in
        drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ()

    method draw () = ignore (self#draw_callback (Obj.magic ()))
end
