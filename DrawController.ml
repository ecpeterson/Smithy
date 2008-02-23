open GlFlatDraw

type glmode = Flat_Draw | VISUAL_MODE

class glcontroller (ar: GlGtk.area)
                   (vscroll: GRange.range)
                   (hscroll: GRange.range) = object (self)
    (* we need to keep track of our modes *)
    val mutable mode = Flat_Draw
    val gldrawer = new gldrawer ar (vscroll#adjustment) (hscroll#adjustment)

    method gldrawer = gldrawer

    (* set up the hooks to GTK *)
    initializer
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
            |_ -> ()

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
                GlClear.color GlFlatDraw.background_color;
                Gl.disable `depth_test;
                self#realize ()
            |_ -> ()
        end
end
