(*** Smithy.ml contains the actual application framework, and in particular the
 * entry point for the program. ***)

let at_exit _ =
    Preferences.save_prefs ();
    GMain.Main.quit ()

(* honest to god entry point for the program.  note that all kinds of
 * initialization code gets executed before this, and in particular that the
 * top routine of CamlExt.ml _/MUST/_ be executed _/FIRST/_. *)
let _ =
    Preferences.load_prefs ();
    let main_window = DrawModeWindows.drawmode_window in
    ignore (main_window#event#connect#delete ~callback:(fun _ -> false));
    ignore (main_window#connect#destroy ~callback:at_exit);
    let args = Sys.argv in
    if Array.length args > 1 then
        MapFormat.read_from_file args.(Array.length args - 1);
    GMain.Main.main ()
