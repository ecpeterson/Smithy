let width = 64
let height = 64
let ticks = 512
let radius = width / 2
let pi = 2.0 *. acos 0.0
let radians_of_ticks t = ((float t) /. (float ticks)) *. 2.0 *. pi
let point_on_circle tick =
    let r = float radius in
    let rads = radians_of_ticks tick in
    let x = r *. cos rads in
    let y = r *. sin rads in
    (radius + (int_of_float x), radius + (int_of_float y))
let pos_to_theta x y =
    let x, y = (float (x - radius), float (y - radius)) in
    let theta = atan2 y x in
    let theta = if theta < 0.0 then theta +. 2.0 *. pi else theta in
    int_of_float (theta /. (2.0 *. pi) *. (float ticks))

class dialSlider packing_fn = object (self)
    val mutable eventbox = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable drawable_onscreen = Obj.magic ()
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()

    initializer
        eventbox <- GBin.event_box ~packing:packing_fn ();
        area <- GMisc.drawing_area ~packing:eventbox#add ();
        buffer <- GDraw.pixmap ~width ~height ();
        drawable <- new GDraw.drawable (buffer#pixmap);
        area#misc#realize ();
        drawable_onscreen <- new GDraw.drawable (area#misc#window);
        area#event#add [`BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
                        `EXPOSURE; `SCROLL; `POINTER_MOTION_HINT];
        ignore (eventbox#event#connect#motion_notify
                    ~callback:self#mousedrag_callback);
        ignore (eventbox#event#connect#button_release
                    ~callback:self#mouseup_callback);
        ignore (eventbox#event#connect#button_press
                    ~callback:self#mousedown_callback);
        ignore (area#event#connect#expose ~callback:self#draw_callback);
        ignore (area#event#connect#scroll ~callback:self#scroll_callback);
        ()

    val mutable valuechanged_callback =
        (fun theta -> print_endline (string_of_int theta); ())
    val mutable theta = 0

    method connect_valuechanged f = valuechanged_callback <- f
    method theta = theta
    method set_theta t = theta <- t

    method private draw_callback _ =
        drawable#set_foreground (`COLOR (area#misc#style#bg `NORMAL));
        drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
        drawable#set_foreground (`RGB (65535, 0, 0));
        drawable#arc ~x:0 ~y:0 ~width ~height
                     ~start:0.0 ~angle:360.0 ~filled:true ();
        drawable#set_foreground (`RGB (0, 65535, 0));
        let x, y = point_on_circle theta in
        drawable#line ~x:(width / 2) ~y:(height / 2) ~x ~y;

        drawable_onscreen#put_pixmap ~x:0 ~y:0 buffer#pixmap;
        false
    method private scroll_callback scroll_descriptor =
        (match GdkEvent.Scroll.direction scroll_descriptor with
        | `UP    -> theta <- (theta - 1) mod ticks
        | `DOWN  -> theta <- (theta + 1) mod ticks
        | `LEFT | `RIGHT -> ());
        if theta < 0 then theta <- theta + ticks;
        valuechanged_callback theta;
        self#draw_callback (Obj.magic ());
        false
    method private mouseup_callback mouse_descriptor =
        valuechanged_callback theta;
        false
    method private mousedrag_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Motion.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Motion.y mouse_descriptor) in
        theta <- pos_to_theta x y;
        self#draw_callback (Obj.magic ());
        false
    method private mousedown_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        theta <- pos_to_theta x y;
        self#draw_callback (Obj.magic ());
        false
end
