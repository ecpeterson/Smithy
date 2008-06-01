let ball_radius = 8
let dial_radius = 32
let width = (dial_radius + ball_radius) * 2
let height = width
let cx = width / 2
let cy = height / 2
let ticks = 512
let twopi = 4.0 *. acos 0.0
let radians_of_ticks t = ((float t) /. (float ticks)) *. twopi
let point_on_circle tick =
    let r = float dial_radius in
    let rads = radians_of_ticks tick in
    let x = r *. cos rads in
    let y = r *. sin rads in
    (cx + (int_of_float x), cy + (int_of_float y))
let pos_to_theta x y =
    let x, y = (float (x - cx), float (y - cy)) in
    let theta = atan2 y x in
    let theta = if theta < 0.0 then theta +. twopi else theta in
    int_of_float (theta /. (twopi) *. (float ticks))

class dialSlider ?packing:(packing = ignore) () = object (self)
    val mutable eventbox = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable drawable_onscreen = None
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()

    initializer
        eventbox <- GBin.event_box ~packing ();
        area <- GMisc.drawing_area ~packing:eventbox#add ();
        buffer <- GDraw.pixmap ~width ~height ();
        drawable <- new GDraw.drawable (buffer#pixmap);
        area#event#add [`BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
                        `EXPOSURE; `SCROLL; `POINTER_MOTION_HINT];
        ignore (eventbox#event#connect#motion_notify
                    ~callback:self#mousedrag_callback);
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
    method widget = eventbox#coerce

    method private draw_callback _ =
        begin match drawable_onscreen with
        |None ->
            area#misc#realize ();
            drawable_onscreen <- Some (new GDraw.drawable (area#misc#window))
        |Some x -> ()
        end;
        let Some drawable_onscreen = drawable_onscreen in
        drawable#set_foreground (`COLOR (area#misc#style#bg `NORMAL));
        drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
        drawable#set_foreground (`COLOR (area#misc#style#dark `NORMAL));
        drawable#arc ~x:ball_radius ~y:ball_radius
                     ~width:(2 * dial_radius) ~height:(2 * dial_radius)
                     ~start:0.0 ~angle:360.0 ~filled:true ();
        drawable#set_foreground (`BLACK);
        drawable#arc ~x:ball_radius ~y:ball_radius
                     ~width:(2 * dial_radius) ~height:(2 * dial_radius)
                     ~start:0.0 ~angle:360.0 ~filled:false ();
        drawable#set_foreground (`COLOR (area#misc#style#mid `NORMAL));
        drawable#arc ~x:(ball_radius + dial_radius / 2)
                     ~y:(ball_radius + dial_radius / 2)
                     ~width:dial_radius ~height:dial_radius
                     ~start:0.0 ~angle:360.0 ~filled:true ();
        drawable#set_foreground (`RGB (65535, 0, 0));
        let x, y = point_on_circle theta in
        drawable#line ~x:cx ~y:cy ~x ~y;
        drawable#set_foreground (`COLOR (area#misc#style#light `NORMAL));
        drawable#arc ~x:(x - ball_radius) ~y:(y - ball_radius)
                     ~width:(2 * ball_radius) ~height:(2 * ball_radius)
                     ~start:0.0 ~angle:360.0 ~filled:true ();

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
    method private mousedrag_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Motion.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Motion.y mouse_descriptor) in
        let old_theta = theta in
        theta <- pos_to_theta x y;
        if theta <> old_theta then
            (self#draw_callback (Obj.magic ());
            valuechanged_callback theta);
        false
    method private mousedown_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let old_theta = theta in
        theta <- pos_to_theta x y;
        if theta <> old_theta then
            (self#draw_callback (Obj.magic ());
            valuechanged_callback theta);
        false
end
