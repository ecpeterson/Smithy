(*** Smithy.ml contains the actual application framework, and in particular the
 * entry point for the program. ***)

let okay_to_quit = ref true

(* honest to god entry point for the program. *)
let _ =
    ignore (GtkMain.Main.init ());
    Gdk.Rgb.init ();
    GtkBase.Widget.set_default_visual (Gdk.Rgb.get_visual ());
    GtkBase.Widget.set_default_colormap (Gdk.Rgb.get_cmap ());
    let main_window = new DrawModeWindows.drawModeWindow ~width:500 ~height:300
                                                         ~title:"Smithy"
                                                         ~show:true () in
    DrawMode.init_draw_mode main_window;
    let at_exit _ =
        if !okay_to_quit then begin
            okay_to_quit := false;
            Preferences.save_prefs main_window;
            GMain.Main.quit ()
        end in
    ignore (main_window#window#event#connect#delete ~callback:(fun _ -> false));
    ignore (main_window#window#connect#destroy ~callback:at_exit);
    Preferences.load_prefs main_window;
    let args = Sys.argv in
    if Array.length args > 1 then
        MapFormat.read_from_file args.(Array.length args - 1);
    GMain.Main.main ();
    at_exit ()
