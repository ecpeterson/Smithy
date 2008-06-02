(* descriptor format: list of components
 * `T of int * int * component list
 * `H of component list
 * `V of component list
 * `E of string ref
 * *)

type component =
    [ `H of component list
    | `V of component list
(*  | `T of int * int * (int * int * int * int * component) list *)
    | `E of string ref
    | `M of string list * int ref
    | `C of string * bool ref
    | `S of int ref
    | `L of string ]

let rec build_dialog descriptor ~packing ~cleanup () =
    match descriptor with
        |`C (label, active) :: descriptor ->
            let b = GButton.check_button ~label ~active:!active ~packing () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); active := b#active) ()
        |`S return :: descriptor ->
            let spinner = new DialSlider.dialSlider ~packing ~size:100 () in
            spinner#set_theta !return;
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup (); return := spinner#theta) ()
        |`V components :: descriptor ->
            let vbox = GPack.vbox ~packing () in
            let extra_cleanup = build_dialog components ~packing:vbox#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup ();
                extra_cleanup ()) ()
        |`H components :: descriptor ->
            let hbox = GPack.hbox ~packing () in
            let extra_cleanup = build_dialog components ~packing:hbox#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                cleanup ();
                extra_cleanup ()) ()
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
(*        |`T (columns, rows, components) :: descriptor ->
            let table = GPack.table ~columns ~rows ~packing () in
            let extra_cleanup = List.fold_left
                (fun extra_cleanup (top, left, bottom, right, component) ->
                    let more_extra_cleanup =
                        build_dialog [component] ~packing:ignore
                                                 ~cleanup:ignore () in
                    table#attach ~top ~left ~bottom ~right  *)
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
