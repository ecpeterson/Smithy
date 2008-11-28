(*** Smithy.ml contains the actual application framework, and in particular the
 * entry point for the program. ***)
open CamlExt

let okay_to_quit = ref true

(* honest to god entry point for the program. *)
let _ =
    let scale, width, height = Preferences.load_prefs () in
    ignore (GtkMain.Main.init ());
    Gdk.Rgb.init ();
    GtkBase.Widget.set_default_visual (Gdk.Rgb.get_visual ());
    GtkBase.Widget.set_default_colormap (Gdk.Rgb.get_cmap ());
    let main_window = new DrawModeWindows.drawModeWindow ~width ~height
                                                         ~title:"Smithy"
                                                         ~show:true () in
    DrawMode.init_draw_mode main_window;
    let at_exit _ =
        if !okay_to_quit then begin
            okay_to_quit := false;
            let dlg = GWindow.message_dialog
                ~message:"Would you like to save before quitting?"
                ~message_type:`WARNING ~buttons:GWindow.Buttons.yes_no
                ~modal:true ~title:Resources.warning () in
            begin match dlg#run () with
                |`YES -> FileDialogs.save_map_dialog main_window#set_title ()
                |_ -> ()
            end;
            dlg#destroy();
            Preferences.save_prefs main_window#orthodrawer#scale
                                   main_window#width main_window#height;
            GMain.Main.quit ()
        end in
    ignore (main_window#window#event#connect#delete ~callback:(fun _ ->
        at_exit (); true));
    ignore (main_window#window#connect#destroy ~callback:at_exit);
    main_window#orthodrawer#set_scale scale;
    let args = Sys.argv in
    if Array.length args > 1 then
        MapFormat.read_from_file args.(Array.length args - 1);
    GMain.Main.main ();
    at_exit ()
