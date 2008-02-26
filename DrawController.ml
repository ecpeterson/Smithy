open GlFlatDraw
open VisualMode

type glmode = Flat_Draw | VISUAL_MODE

class glcontroller (ar: GlGtk.area)
                   (vscroll: GRange.range)
                   (hscroll: GRange.range) = object (self)
    (* we need to keep track of our modes *)
    val mutable mode = Flat_Draw
    val gldrawer = new gldrawer ar (vscroll#adjustment) (hscroll#adjustment)
    val visualmode = new visualmode ar
    val map = new MapFormat.map

    method gldrawer = gldrawer
    method visualmode = visualmode
    method map = map

    initializer
        (* set up the child maps *)
        gldrawer#set_map map;
        visualmode#set_map map;
        (* set up the hooks to GTK *)
        GlDraw.shade_model `smooth;
        List.iter Gl.enable [`line_smooth; `point_smooth];
        ar#connect#reshape ~callback:self#reshape;
        ar#connect#realize ~callback:self#realize;
        ar#connect#display ~callback:self#display;
        ()

    method reshape ~width ~height =
        ar#make_current ();
        match mode with
            |Flat_Draw ->
                gldrawer#reshape (float width) (float height)
            |_ -> ()

    method realize () =
        ar#make_current ();
        match mode with
            |Flat_Draw ->
                gldrawer#reortho ()
            |_ -> ()

    method display () =
        ar#make_current ();
        match mode with
            |Flat_Draw ->
                gldrawer#draw ()
            |VISUAL_MODE ->
                visualmode#display ()
            |_ -> ()

    method mode () = mode
    method set_mode x =
        (* perform cleanup from old mode *)
        begin match mode with
            |_ -> ()
        end;
        (* set the mode *)
        mode <- x;
        (* initialize the new mode *)
        begin match mode with
            |Flat_Draw ->
                GlDraw.line_width 1.0;
                GlClear.color Colors.background_color;
                Gl.disable `depth_test;
                self#realize ()
            |VISUAL_MODE ->
                visualmode#init ()
            |_ -> ()
        end

    (* routines for piping the GUI inputs where they need to go *)
    method send_key (key: GdkEvent.Key.t) =
        let state = GdkEvent.Key.state key in
        let keyval = GdkEvent.Key.keyval key in
        match mode with
            |Flat_Draw -> gldrawer#handle_key keyval
            |VISUAL_MODE -> visualmode#send_key keyval
            |_ -> false

    val mutable click0 = 0.0, 0.0
    val mutable click1 = 0.0, 0.0

    method send_mousedown mouse_descriptor =
        let x = GdkEvent.Button.x mouse_descriptor in
        let y = GdkEvent.Button.y mouse_descriptor in
        let button = GdkEvent.Button.button mouse_descriptor in
        let state = Gdk.Convert.modifier (GdkEvent.Button.state mouse_descriptor) in
        (* store the beginning of the click *)
        click0 <- x, y;
        click1 <- x, y;
        (* let our modes handle the rest *)
        match mode with
            |Flat_Draw -> gldrawer#tool_begin_event x y button state
            |_ -> false

    method send_mousedrag (mouse_descriptor: GdkEvent.Motion.t) =
        let x = GdkEvent.Motion.x mouse_descriptor in
        let y = GdkEvent.Motion.y mouse_descriptor in
        let (oldx, oldy) = click1 in
        click1 <- (x, y);
        match mode with
            |Flat_Draw -> gldrawer#tool_in_event click0 (oldx, oldy) click1
            |_ -> false

    method send_mouseup   (mouse_descriptor: GdkEvent.Button.t) =
        let x = GdkEvent.Button.x mouse_descriptor in
        let y = GdkEvent.Button.y mouse_descriptor in
        let button = GdkEvent.Button.button mouse_descriptor in
        match mode with
            |Flat_Draw -> gldrawer#tool_end_event click0 click1 button
            |_ -> false
end
