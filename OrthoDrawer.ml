class orthoDrawer packing_fn = object (self)
    val mutable eventbox = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()
    val mutable drawable_onscreen = Obj.magic ()

    initializer
        eventbox <- GBin.event_box ~packing:packing_fn ();
        area <- GMisc.drawing_area ~packing:eventbox#add ();
        area#misc#realize ();
        drawable_onscreen <- new GDraw.drawable (area#misc#window);
        area#event#add [`BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
                        `EXPOSURE; `STRUCTURE; `SCROLL; `POINTER_MOTION_HINT];
        ignore (eventbox#event#connect#button_press
                    ~callback:self#mousedown_callback);
        ignore (eventbox#event#connect#motion_notify
                    ~callback:self#mousedrag_callback);
        ignore (eventbox#event#connect#button_release
                    ~callback:self#mouseup_callback);
        ignore (area#event#connect#expose ~callback:self#draw_callback);
        ignore (area#event#connect#configure ~callback:self#resize_callback);
        ignore (area#event#connect#scroll ~callback:self#scroll_callback);
        ()

    val mutable click0 = 0, 0
    val mutable click1 = 0, 0
    val mutable mousedown_callback =
        (fun x y button state -> ())
    val mutable mouseup_callback =
        (fun x0 y0 x1 y1 button state -> ())
    val mutable mousedrag_callback =
        (fun x0 y0 x1 y1 x2 y2 -> ())
    val mutable draw_callback =
        (fun _ -> ())
    val mutable scroll_callback =
        (fun dx dy -> ())

    method connect_mousedown f = mousedown_callback <- f
    method connect_mouseup f = mouseup_callback <- f
    method connect_mousedrag f = mousedrag_callback <- f
    method connect_draw f = draw_callback <- f
    method connect_scroll f = scroll_callback <- f

    method private draw_callback _ =
        draw_callback ();
        drawable_onscreen#put_pixmap ~x:0 ~y:0 buffer#pixmap;
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
        self#draw (); false
    method private mouseup_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let (x0, y0) = click0 in
        let x, y = self#to_map (x, y) in
        click1 <- x, y;
        mouseup_callback x0 y0 x y button state;
        self#draw (); false
    method private mousedrag_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Motion.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Motion.y mouse_descriptor) in
        let x, y = self#to_map (x, y) in
        let (oldx, oldy) = click1 in
        click1 <- x, y;
        let x0, y0 = click0 in
        mousedrag_callback x0 y0 oldx oldy x y;
        self#draw (); false
    method private resize_callback geom_descriptor =
        let width = GdkEvent.Configure.width geom_descriptor in
        let height = GdkEvent.Configure.height geom_descriptor in
        buffer <- GDraw.pixmap ~width ~height ();
        drawable <- new GDraw.drawable (buffer#pixmap);
        false
    method private scroll_callback scroll_descriptor =
        (match GdkEvent.Scroll.direction scroll_descriptor with
        | `UP    -> scroll_callback   0.0 (-1.0)
        | `DOWN  -> scroll_callback   0.0   1.0
        | `LEFT  -> scroll_callback (-1.0)  0.0
        | `RIGHT -> scroll_callback   1.0   0.0);
        self#draw (); false

    val mutable origin = (0, 0)
    val mutable scale = 0.1

    method origin () = origin
    method scale () = scale
    method set_origin x = origin <- x
    method set_scale x = scale <- x

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

    (* manual invocation of a redraw *)
    method draw () = ignore (self#draw_callback (Obj.magic ()))
end
