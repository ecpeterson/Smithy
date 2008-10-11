(*** OrthoDrawer.ml contains a GTK widget class that allows 2D drawing of
 * points, lines, polygons, and bitmaps under scaling and translation
 * transformations. ***)
open CamlExt

class orthoDrawer ?width:explicit_width ?height:explicit_height
                  ?packing:(packing = ignore)
                  ~xmin ~xmax ~ymin ~ymax () =
object (self)
    (* widgets *)
    val mutable table = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable drawable_onscreen = None
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()
    val mutable hadj = Obj.magic ()
    val mutable vadj = Obj.magic ()
    val mutable pango_context = Obj.magic ()
    val mutable pango_layout = Obj.magic ()

    (* actual data *)
    val mutable mousedown_callback =
        (fun self x y button state -> ())
    val mutable mouseup_callback =
        (fun self x0 y0 x1 y1 button state -> ())
    val mutable mousedrag_callback =
        (fun self x0 y0 x1 y1 x2 y2 -> ())
    val mutable spacedown_callback =
        (fun _ -> ())
    val mutable spaceup_callback =
        (fun _ -> ())
    val mutable draw_callback =
        (fun self -> ())
    val mutable click0 = 0.0, 0.0
    val mutable click1 = 0.0, 0.0
    val mutable scale = 1.0
    val mutable suppress_draw = false
    (* XXX: if someone could tell me how to get the record fields out of
     * Gdk.GC.values, that would be a great help *)
    val mutable current_color = (`RGB (0, 0, 0))
    val mutable space = false

    (* accessors *)
    method connect_mousedown f = mousedown_callback <- f
    method connect_mouseup f = mouseup_callback <- f
    method connect_mousedrag f = mousedrag_callback <- f
    method connect_spacedown f = spacedown_callback <- f
    method connect_spaceup f = spaceup_callback <- f
    method connect_draw f = draw_callback <- f
    method private set_origin_raw (x, y) =
        suppress_draw <- true;
        hadj#set_value x;
        vadj#set_value y;
        suppress_draw <- false
    method private set_scale_raw x =
        scale <- x
    method set_origin (x, y) = self#set_origin_raw (x, y); self#draw ()
    method set_scale x = self#set_scale_raw x; self#draw ()
    method origin = hadj#value, vadj#value
    method scale = scale
    method widget = table#coerce
    method set_cursor cursor =
        Gdk.Window.set_cursor area#misc#window (Gdk.Cursor.create cursor)
    method space = space

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

        hadj <- GData.adjustment ();
        vadj <- GData.adjustment ();
        GRange.scrollbar `HORIZONTAL ~packing:(table#attach ~left:0 ~top:1)
                                     ~adjustment:hadj ();
        GRange.scrollbar `VERTICAL ~packing:(table#attach ~left:1 ~top:0)
                                   ~adjustment:vadj ();

        area#event#add [`BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
                        `STRUCTURE; `EXPOSURE; `SCROLL; `POINTER_MOTION_HINT];
        eventbox#misc#set_can_focus true;
        eventbox#misc#connect#realize ~callback:eventbox#misc#grab_focus;
        ignore (eventbox#event#connect#motion_notify
                    ~callback:self#mousedrag_callback);
        ignore (eventbox#event#connect#button_press
                    ~callback:self#mousedown_callback);
        ignore (eventbox#event#connect#button_release
                    ~callback:self#mouseup_callback);
        ignore (eventbox#event#connect#key_press
                    ~callback:self#keydown_callback);
        ignore (eventbox#event#connect#key_release
                    ~callback:self#keyup_callback);
        ignore (area#event#connect#configure ~callback:self#resize_callback);
        ignore (area#event#connect#expose ~callback:self#draw_callback);
        ignore (area#event#connect#scroll ~callback:self#scroll_callback);
        ignore (hadj#connect#value_changed ~callback:self#draw);
        ignore (vadj#connect#value_changed ~callback:self#draw);
        pango_context <- new GPango.context_rw
                                            (Gdk.Screen.get_pango_context ());
        pango_context#set_font_by_name "sans 8";
        pango_layout <- pango_context#create_layout;
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
        suppress_draw <- false
    method private to_screen (x, y) =
        let xo, yo = self#origin in
        (x -. xo) *. scale, (y -. yo) *. scale
    method private to_map (x, y) =
        let xo, yo = self#origin in
        (x /. scale) +. xo, (y /. scale) +. yo

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
            let drawable_onscreen = of_opt drawable_onscreen in
            let width, height = drawable_onscreen#size in
            buffer <- GDraw.pixmap ~width ~height ();
            drawable <- new GDraw.drawable (buffer#pixmap);
        |Some x -> ()
        end;
        draw_callback self;
        let drawable_onscreen = of_opt drawable_onscreen in
        drawable_onscreen#put_pixmap ~x:0 ~y:0 buffer#pixmap;
        end;
        false
    method private scroll_callback scroll_descriptor =
        (* XXX: base this on something variable *)
        let dx, dy = 32.0 /. scale, 32.0 /. scale in
        let xo, yo = self#origin in
        let xo, yo = (match GdkEvent.Scroll.direction scroll_descriptor with
        | `UP    -> xo, (yo -. dy)
        | `DOWN  -> xo, (yo +. dy)
        | `LEFT  -> (xo -. dx), yo
        | `RIGHT -> (xo +. dx), yo) in
        self#set_origin (xo, yo);
        false
    method private mousedrag_callback mouse_descriptor =
        let x = GdkEvent.Motion.x mouse_descriptor in
        let y = GdkEvent.Motion.y mouse_descriptor in
        let x, y = self#to_map (x, y) in
        let (oldx, oldy) = click1 in
        click1 <- x, y;
        let x0, y0 = click0 in
        mousedrag_callback self x0 y0 oldx oldy x y;
        false
    method private mousedown_callback mouse_descriptor =
        let x = GdkEvent.Button.x mouse_descriptor in
        let y = GdkEvent.Button.y mouse_descriptor in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let x, y = self#to_map (x, y) in
        click0 <- x, y;
        click1 <- x, y;
        mousedown_callback self x y button state;
        false
    method private mouseup_callback mouse_descriptor =
        let x = GdkEvent.Button.x mouse_descriptor in
        let y = GdkEvent.Button.y mouse_descriptor in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        let (x0, y0) = click0 in
        let x, y = self#to_map (x, y) in
        click1 <- x, y;
        mouseup_callback self x0 y0 x y button state;
        false
    method private keydown_callback key_descriptor =
        let key = GdkEvent.Key.string key_descriptor in
        if key = " " then begin
            space <- true;
            spacedown_callback () end;
        false
    method private keyup_callback key_descriptor =
        let key = GdkEvent.Key.string key_descriptor in
        if key = " " then begin
            space <- false;
            spaceup_callback () end;
        false

    (* other public methods *)
    method point (x, y) =
        let x, y = self#to_screen (x, y) in
        let x, y = int_of_float x, int_of_float y in
        drawable#point ~x ~y

    method fat_point ((x, y) : (float * float)) (thickness : int) =
        let x, y = self#to_screen (x, y) in
        let x, y = int_of_float x, int_of_float y in
        drawable#rectangle ~x:(x-thickness/2) ~y:(y-thickness/2)
                           ~width:thickness ~height:thickness ~filled:true ()

    method line (x0, y0) (x1, y1) =
        let (x0t, y0t) = self#to_screen (x0, y0) in
        let x0t, y0t = int_of_float x0t, int_of_float y0t in
        let (x1t, y1t) = self#to_screen (x1, y1) in
        let x1t, y1t = int_of_float x1t, int_of_float y1t in
        drawable#line ~x:x0t ~y:y0t ~x:x1t ~y:y1t

    method polygon filled points =
        let points = List.map (fun (x, y) ->
            let x, y = self#to_screen (x, y) in
            int_of_float x, int_of_float y) points in
        drawable#polygon ~filled points

    method points points =
        let points = List.map (fun (x, y) ->
            let x, y = self#to_screen (x, y) in
            int_of_float x, int_of_float y) points in
        drawable#points points

    method segments lines =
        let lines = List.map
            (fun (p0, p1) ->
                let p0x, p0y = self#to_screen p0 in
                let p1x, p1y = self#to_screen p1 in
                (int_of_float p0x, int_of_float p0y),
                    (int_of_float p1x, int_of_float p1y)) lines in
        drawable#segments lines

    method linear_curve points =
        let points = List.map (fun (x, y) ->
            let x, y = self#to_screen (x, y) in
            int_of_float x, int_of_float y) points in
        drawable#lines points

    method image (im: GMisc.image) x y =
        let (x, y) = self#to_screen (x, y) in
        let pixbuf = im#pixbuf in
        let width = GdkPixbuf.get_width pixbuf in
        let height = GdkPixbuf.get_height pixbuf in
        let (x, y) = (int_of_float x - width/2, int_of_float y - height/2) in
        drawable#put_pixbuf x y pixbuf

    method arrow x y facing =
        let (x, y) = self#to_screen (x, y) in
        let x, y = int_of_float x, int_of_float y in
        let r = 8.0 in
        let p1x = x + int_of_float (r *. cos facing) in
        let p1y = y + int_of_float (r *. sin facing) in
        let r = 10.0 in
        let p2x = x + int_of_float (r *. cos (facing +. twopi *. 0.4)) in
        let p2y = y + int_of_float (r *. sin (facing +. twopi *. 0.4)) in
        let p3x = x + int_of_float (r *. cos (facing -. twopi *. 0.4)) in
        let p3y = y + int_of_float (r *. sin (facing -. twopi *. 0.4)) in
        let poly = [(p1x, p1y); (p2x, p2y); (p3x, p3y)] in
        drawable#polygon ~filled:true poly;
        drawable#set_foreground `BLACK;
        drawable#polygon ~filled:false poly;
        drawable#set_foreground current_color

    method set_color (r, g, b) =
        let r = int_of_float (r *. 65535.0) in
        let g = int_of_float (g *. 65535.0) in
        let b = int_of_float (b *. 65535.0) in
        current_color <- (`RGB (r, g, b));
        drawable#set_foreground current_color

    method clear () =
        let width, height = drawable#size in
        drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ()

    method zoom_at factor xt1 yt1 =
        let xo, yo = self#origin in
        let x, y = self#to_screen (xt1, yt1) in
        let orig_scale = scale in
        let new_scale = orig_scale *. factor in
        scale <- new_scale;
        let xnew, ynew =
            x /. orig_scale -. x /. new_scale +. xo,
            y /. orig_scale -. y /. new_scale +. yo in
        let drawable_onscreen = of_opt drawable_onscreen in
        let width, height = drawable_onscreen#size in
        self#adjust_scroll_range width height;
        self#set_origin (xnew, ynew);

    method zoom factor =
        let cx, cy = drawable#size in
        let cx, cy = self#to_map (float cx /. 2., float cy /. 2.) in
        self#zoom_at factor cx cy

    method set_font name =
        pango_context#set_font_by_name name
    method centered_text str (x, y) =
        let x, y = self#to_screen (x, y) in
        Pango.Layout.set_text pango_layout str;
        let dx, dy = Pango.Layout.get_pixel_size pango_layout in
        let x, y = (int_of_float x) - dx/2, (int_of_float y) - dy/2 in
        drawable#put_layout ~x ~y pango_layout
    (* outputs at the lower-left corner *)
    method text str (x, y) =
        let x, y = self#to_screen (x, y) in
        Pango.Layout.set_text pango_layout str;
        let _, dy = Pango.Layout.get_pixel_size pango_layout in
        let x, y = int_of_float x, (int_of_float y) - dy in
        drawable#put_layout ~x ~y pango_layout

    method draw () = ignore (self#draw_callback (Obj.magic ()))

    method center_on (x, y) =
        match drawable_onscreen with
        |None -> ()
        |Some drawable_onscreen ->
            let width, height = drawable_onscreen#size in
            let width, height = float width /. scale, float height /. scale in
            self#set_origin (x -. width /. 2.0,
                             y -. height /. 2.0)
end
