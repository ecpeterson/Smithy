class orthoDrawer ?width:explicit_width ?height:explicit_height
                  ?packing:(packing = ignore)
                  ~xmin ~xmax ~ymin ~ymax
                  ?xstart:(xstart = 0.0) ?ystart:(ystart = 0.0) () =
object (self)
    (* widgets *)
    val mutable table = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable drawable_onscreen = None
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()
    val mutable hadj = Obj.magic ()
    val mutable vadj = Obj.magic ()

    (* actual data *)
    val mutable mousedown_callback =
        (fun self x y button state -> ())
    val mutable mouseup_callback =
        (fun self x0 y0 x1 y1 button state -> ())
    val mutable mousedrag_callback =
        (fun self x0 y0 x1 y1 x2 y2 -> ())
    val mutable draw_callback =
        (fun self -> ())
    val mutable click0 = 0, 0
    val mutable click1 = 0, 0
    val mutable scale = 0.1
    val mutable suppress_draw = false

    (* accessors *)
    method connect_mousedown f = mousedown_callback <- f
    method connect_mouseup f = mouseup_callback <- f
    method connect_mousedrag f = mousedrag_callback <- f
    method connect_draw f = draw_callback <- f
    method private set_origin_raw (x, y) =
        suppress_draw <- true;
        hadj#set_value (float x);
        vadj#set_value (float y);
        suppress_draw <- false
    method private set_scale_raw x =
        scale <- x
    method set_origin (x, y) = self#set_origin_raw (x, y); self#draw ()
    method set_scale x = self#set_scale_raw x; self#draw ()
    method origin = (int_of_float hadj#value), (int_of_float vadj#value)
    method scale = scale
    method widget = table#coerce

    (* constructor *)
    initializer
        begin match explicit_width, explicit_height with
        |None, None ->
            table <- GPack.table ~columns:2 ~rows:2 ~packing ()
        |Some width, None ->
            table <- GPack.table ~columns:2 ~rows:2 ~width ~packing ()
        |None, Some height ->
            table <- GPack.table ~columns:2 ~rows:2 ~height ~packing ()
        |Some width, Some height ->
            table <- GPack.table ~columns:2 ~rows:2 ~width ~height ~packing ()
        end;

        let eventbox =
            GBin.event_box ~packing:(table#attach ~left:0 ~top:0 ~expand:`BOTH)
                           () in

        area <- GMisc.drawing_area ~packing:eventbox#add ();
        area#misc#realize ();

        hadj <- GData.adjustment ();
        vadj <- GData.adjustment ();
        let hscroll = GRange.scrollbar `HORIZONTAL
                          ~packing:(table#attach ~left:0 ~top:1)
                          ~adjustment:hadj () in
        let vscroll = GRange.scrollbar `VERTICAL
                          ~packing:(table#attach ~left:1 ~top:0)
                          ~adjustment:vadj () in

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
        ignore (hadj#connect#value_changed ~callback:self#draw);
        ignore (vadj#connect#value_changed ~callback:self#draw);
        ()

    (* private methods *)
    method private adjust_scroll_range width height =
        let lower = xmin in
        let upper = xmax -. float width /. scale in
        let upper = if upper < lower then lower else upper in
        let step_incr = 24.0 /. scale in
        let page_incr = 240.0 /. scale in
        suppress_draw <- true;
        hadj#set_bounds
            ~lower ~upper ~step_incr ~page_incr ();
        let upper = ymax -. float height /. scale in
        let upper = if upper < lower then lower else upper in
        vadj#set_bounds
            ~lower ~upper ~step_incr ~page_incr ();
        suppress_draw <- false;
        self#set_origin_raw (width, height)

    (* event callbacks *)
    method private resize_callback geom_descriptor =
        let new_width = GdkEvent.Configure.width geom_descriptor in
        let new_height = GdkEvent.Configure.height geom_descriptor in
        self#adjust_scroll_range new_width new_height;
        drawable_onscreen <- None;
        false
    method private draw_callback _ =
        if not suppress_draw then begin
        begin match drawable_onscreen with
        |None ->
            drawable_onscreen <- Some (new GDraw.drawable (area#misc#window));
            let Some drawable_onscreen = drawable_onscreen in
            let width, height = drawable_onscreen#size in
            buffer <- GDraw.pixmap ~width ~height ();
            drawable <- new GDraw.drawable (buffer#pixmap);
        |Some x -> ()
        end;
        draw_callback self;
        let Some drawable_onscreen = drawable_onscreen in
        drawable_onscreen#put_pixmap ~x:0 ~y:0 buffer#pixmap;
        end;
        false
    method private scroll_callback scroll_descriptor =
        (* XXX: base this on something variable *)
        let dx, dy = (int_of_float (32.0 /. scale)),
                     (int_of_float (32.0 /. scale)) in
        let xo, yo = self#origin in
        let xo, yo = (match GdkEvent.Scroll.direction scroll_descriptor with
        | `UP    -> xo, (yo - dy)
        | `DOWN  -> xo, (yo + dy)
        | `LEFT  -> (xo - dx), yo
        | `RIGHT -> (xo + dx), yo) in
        self#set_origin (xo, yo);
        false
    method private mousedrag_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Motion.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Motion.y mouse_descriptor) in
        let x, y = self#to_map (x, y) in
        let (oldx, oldy) = click1 in
        click1 <- x, y;
        let x0, y0 = click0 in
        mousedrag_callback self x0 y0 oldx oldy x y;
        false
    method private mousedown_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let x, y = self#to_map (x, y) in
        click0 <- x, y;
        click1 <- x, y;
        mousedown_callback self x y button state;
        false
    method private mouseup_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let (x0, y0) = click0 in
        let x, y = self#to_map (x, y) in
        click1 <- x, y;
        mouseup_callback self x0 y0 x y button state;
        false

    (* other public methods *)
    method to_screen (x, y) =
        let xo, yo = self#origin in
        (int_of_float (float (x - xo) *. scale),
            int_of_float (float (y - yo) *. scale))

    method to_map (x, y) =
        let xo, yo = self#origin in
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

    method zoom_at factor xt1 yt1 =
        let xo, yo = self#origin in
        let x, y = self#to_screen (xt1, yt1) in
        let orig_scale = scale in
        let new_scale = orig_scale *. factor in
        scale <- new_scale;
        let xt2, yt2 = self#to_map (x, y) in
        let xnew, ynew =
            (float x) /. orig_scale -. (float x) /. new_scale +. (float xo),
            (float y) /. orig_scale -. (float y) /. new_scale +. (float yo) in
        let Some drawable_onscreen = drawable_onscreen in
        let width, height = drawable_onscreen#size in
        self#adjust_scroll_range width height;
        self#set_origin ((int_of_float xnew), (int_of_float ynew));

    method zoom factor =
        let cx, cy = drawable#size in
        let cx, cy = self#to_map ((cx / 2), (cy / 2)) in
        self#zoom_at factor cx cy

    method draw () = ignore (self#draw_callback (Obj.magic ()))
end
