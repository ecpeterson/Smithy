(*** GenerateDialog.ml contains routines that allow automatic dialog
 * generation, execution, and result storage. ***)

type component =
    [ `H of component list
    | `V of component list
    | `F of string * component list
(*  | `T of int * int * (int * int * int * int * component) list *)
    | `E of string ref
    | `M of string list * int ref
    | `C of string * bool ref
    | `S of int ref
    | `O of (float * float * float) ref
    | `R of (string * bool ref) list
    | `L of string ]

let rec build_dialog descriptor ~packing ~cleanup () =
    match descriptor with
        |`O rgb :: descriptor ->
            let (r, g, b) = !rgb in
            let color = Gdk.Color.alloc (Gdk.Color.get_system_colormap ())
                       (`RGB (int_of_float (r *. 65535.0),
                              int_of_float (g *. 65535.0),
                              int_of_float (b *. 65535.0))) in
            let b = GButton.color_button ~color ~packing () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup ();
                let (r, g, b) = Gdk.Color.red b#color,
                                Gdk.Color.green b#color,
                                Gdk.Color.blue b#color in
                rgb := float r /. 65535.0, float g /. 65535.0,
                       float b /. 65535.0) ()
        |`C (label, active) :: descriptor ->
            let b = GButton.check_button ~label ~active:!active ~packing () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); active := b#active) ()
        |`S return :: descriptor ->
            let spinner = new DialSlider.dialSlider ~packing ~size:100 () in
            spinner#set_theta !return;
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); return := spinner#theta) ()
        |`F (label, components) :: descriptor ->
            let f = GBin.frame ~label ~packing () in
            let v = GPack.vbox ~packing:f#add () in
            let extra_cleanup = build_dialog components ~packing:v#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); extra_cleanup ()) ()
        |`R ((first_label, first_setting) :: bdesc) :: descriptor ->
            let vbox = GPack.vbox ~packing () in
            let first_button = GButton.radio_button ~label:first_label
                                ~active:!first_setting ~packing:vbox#add () in
            let buttons = List.map (fun (label, setting) ->
                GButton.radio_button ~label ~active:!setting
                    ~group:first_button#group ~packing:vbox#add ()) bdesc in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup ();
                List.iter2 (fun (_, setting) button -> setting:= button#active)
                    ((first_label, first_setting) :: bdesc)
                    (first_button :: buttons)) ()
        |`V components :: descriptor ->
            let vbox = GPack.vbox ~packing () in
            let extra_cleanup = build_dialog components ~packing:vbox#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); extra_cleanup ()) ()
        |`H components :: descriptor ->
            let hbox = GPack.hbox ~packing () in
            let extra_cleanup = build_dialog components ~packing:hbox#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); extra_cleanup ()) ()
        |`E str :: descriptor ->
            let entry = GEdit.entry ~packing ~text:!str () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); str := entry#text) ()
        |`L str :: descriptor ->
            GMisc.label ~text:str ~packing ~justify:`FILL ();
            build_dialog descriptor ~packing ~cleanup ()
        |`M (strings, ret) :: descriptor ->
            let cb = GEdit.combo_box_text ~packing ~strings () in
            let cb = (fun (x, _) -> x) cb in
            cb#set_active !ret;
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); ret := cb#active) ()
        |[] ->
            cleanup

let generate_dialog descriptor title =
    let w = GWindow.dialog ~title () in
    let cleanup = build_dialog descriptor ~packing:w#vbox#add
                                          ~cleanup:ignore () in
    w#add_button_stock `CANCEL `CANCEL;
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |`OK -> cleanup ();
    |_ -> () end;
    w#destroy ()
