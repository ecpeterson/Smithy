(* honest to god entry point for the program *)
let _ =
    let main_window = DrawModeWindows.drawmode_window in
    ignore (main_window#event#connect#delete ~callback:(fun _ -> false));
    ignore (main_window#connect#destroy ~callback:GMain.Main.quit);
    ignore (main_window#event#connect#key_press
                                            ~callback:DrawModeEvent.send_key);
    (* callback hooks for mouse/keyboard events go here *)
    let args = Sys.argv in
    if Array.length args > 1 then
        MapFormat.read_from_file args.(Array.length args - 1);
    GMain.Main.main ()
