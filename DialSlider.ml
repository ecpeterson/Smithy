(*** DialSlider.ml contains a GTK widget class used to pick directional
 * headings similar to the widgets employed by Forge. ***)

class dialSlider ?size:explicit_size
                 ?packing:(packing = ignore) () =
object (self)
    (* widgets *)
    val mutable eventbox = Obj.magic ()
    val mutable area = Obj.magic ()
    val mutable drawable_onscreen = None
    val mutable buffer = Obj.magic ()
    val mutable drawable = Obj.magic ()

    (* actual data *)
    val mutable valuechanged_callback = (fun (theta: float) -> ())
    val mutable theta = 0.0

    (* dimension information *)
    val mutable size = 0
    method private ball_radius = size / 8
    method private dial_radius = size / 2 - self#ball_radius
    method private wcenter =
        let Some drawable_onscreen = drawable_onscreen in
        let x, y = drawable_onscreen#size in x / 2, y / 2
    method private center = size / 2
    method private point_on_circle rads =
        let r = float self#dial_radius in
        let x = r *. cos rads in
        let y = r *. sin rads in
        (self#center + (int_of_float x), self#center + (int_of_float y))
    method private pos_to_theta x y =
        let cx, cy = self#wcenter in
        let x, y = (float (x - cx), float (y - cy)) in
        let theta = atan2 y x in
        let theta = if theta < 0.0 then theta +. CamlExt.twopi else theta in
        theta

    (* accessors *)
    method connect_valuechanged f = valuechanged_callback <- f
    method theta = theta
    method set_theta t = theta <- t
    method widget = eventbox#coerce

    (* constructor *)
    initializer
        begin match explicit_size with
        |None ->
            eventbox <- GBin.event_box ~packing ();
            area <- GMisc.drawing_area ~packing:eventbox#add ()
        |Some size ->
            let width, height = size, size in
            eventbox <- GBin.event_box ~width ~height ~packing ();
            area <- GMisc.drawing_area ~width ~height ~packing:eventbox#add ()
        end;
        area#event#add [`BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
                        `STRUCTURE; `EXPOSURE; `SCROLL; `POINTER_MOTION_HINT];
        ignore (eventbox#event#connect#motion_notify
                    ~callback:self#mousedrag_callback);
        ignore (eventbox#event#connect#button_press
                    ~callback:self#mousedown_callback);
        ignore (area#event#connect#configure ~callback:self#resize_callback);
        ignore (area#event#connect#expose ~callback:self#draw_callback);
        ignore (area#event#connect#scroll ~callback:self#scroll_callback);
        ()

    (* event callbacks *)
    method private resize_callback _ =
        drawable_onscreen <- None;
        false
    method private draw_callback _ =
        begin match drawable_onscreen with
        |None ->
            area#misc#realize ();
            drawable_onscreen <- Some (new GDraw.drawable (area#misc#window));
            let Some drawable_onscreen = drawable_onscreen in
            let width, height = drawable_onscreen#size in
            let explicit_size = (match explicit_size with
                                 |None -> 0
                                 |Some size -> size) in
            size <- max (min width height) explicit_size;
            buffer <- GDraw.pixmap ~width:size ~height:size ();
            drawable <- new GDraw.drawable (buffer#pixmap);
        |Some x -> ()
        end;
        let Some drawable_onscreen = drawable_onscreen in
        let width, height = size, size in
        drawable#set_foreground (`COLOR (area#misc#style#bg `NORMAL));
        drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
        drawable#set_foreground (`COLOR (area#misc#style#dark `NORMAL));
        drawable#arc ~x:self#ball_radius ~y:self#ball_radius
                     ~width:(2 * self#dial_radius)
                     ~height:(2 * self#dial_radius)
                     ~filled:true ~start:0.0 ~angle:360.0 ();
        drawable#set_foreground (`BLACK);
        drawable#arc ~x:self#ball_radius ~y:self#ball_radius
                     ~width:(2 * self#dial_radius)
                     ~height:(2 * self#dial_radius)
                     ~filled:false ~start:0.0 ~angle:360.0 ();
        drawable#set_foreground (`COLOR (area#misc#style#mid `NORMAL));
        drawable#arc ~x:(self#ball_radius + self#dial_radius / 2)
                     ~y:(self#ball_radius + self#dial_radius / 2)
                     ~width:self#dial_radius ~height:self#dial_radius
                     ~filled:true ~start:0.0 ~angle:360.0 ();
        drawable#set_foreground (`RGB (65535, 0, 0));
        let x, y = self#point_on_circle theta in
        drawable#line ~x:self#center ~y:self#center ~x ~y;
        drawable#set_foreground (`COLOR (area#misc#style#light `NORMAL));
        drawable#arc ~x:(x - self#ball_radius) ~y:(y - self#ball_radius)
                     ~width:(2 * self#ball_radius)
                     ~height:(2 * self#ball_radius)
                     ~filled:true ~start:0.0 ~angle:360.0 ();

        let x, y = self#wcenter in
        let x, y = x - size / 2, y - size / 2 in
        drawable_onscreen#put_pixmap ~x ~y buffer#pixmap;
        false
    method private scroll_callback scroll_descriptor =
        (match GdkEvent.Scroll.direction scroll_descriptor with
        | `UP    -> theta <- theta -. CamlExt.twopi /. 512.0
        | `DOWN  -> theta <- theta +. CamlExt.twopi /. 512.0
        | `LEFT | `RIGHT -> ());
        if theta < 0.0 then theta <- theta +. CamlExt.twopi;
        valuechanged_callback theta;
        self#draw_callback (Obj.magic ());
        false
    method private mousedrag_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Motion.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Motion.y mouse_descriptor) in
        let old_theta = theta in
        theta <- self#pos_to_theta x y;
        if theta <> old_theta then
            (self#draw_callback (Obj.magic ());
            valuechanged_callback theta);
        false
    method private mousedown_callback mouse_descriptor =
        let x = int_of_float (GdkEvent.Button.x mouse_descriptor) in
        let y = int_of_float (GdkEvent.Button.y mouse_descriptor) in
        let old_theta = theta in
        theta <- self#pos_to_theta x y;
        if theta <> old_theta then
            (self#draw_callback (Obj.magic ());
            valuechanged_callback theta);
        false
end
