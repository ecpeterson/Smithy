(*** GenerateDialog.ml contains routines that allow automatic dialog
 * generation, execution, and result storage. ***)

open CamlExt

type component =
    [ `H of component list
    | `V of component list
    | `F of string * component list
    | `E of string ref
    | `M of string list * int ref
    | `C of string * bool ref
    | `S of int ref
    | `O of (float * float * float) ref
    | `R of (string * bool ref) list
    | `L of string
    | `N of (string * (component list)) list * int ref
    | `I of (float * float * float * int * Gtk.Tags.orientation) * float ref ]

let rec build_dialog descriptor ~packing ~cleanup () =
    match descriptor with
        |`I ((lower, upper, step_incr, disp_size, orient), v) :: descriptor ->
            let adj = GData.adjustment ~value:!v ~lower
                                       ~upper:(upper +. step_incr) ~step_incr
                                       ~page_size:step_incr
                                       ~page_incr:(upper -. lower) () in
            let (pos, align) = begin match orient with
            |`VERTICAL ->
                (`BOTTOM, GBin.alignment ~height:disp_size ~packing ())
            |`HORIZONTAL ->
                (`RIGHT, GBin.alignment ~width:disp_size ~packing ()) end in
            let slider = GRange.scale orient ~adjustment:adj ~value_pos:pos
                                             ~digits:2 ~packing:align#add
                                             ~inverted:true () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                v := adj#value;
                cleanup ()) ()
        |`N (tabs, retvar) :: descriptor ->
            let notebook = GPack.notebook ~tab_pos:`TOP ~packing () in
            let cleanups = List.map (fun (text, components) ->
                let label = GMisc.label ~text () in
                let page = notebook#append_page ~tab_label:label#coerce in
                let vbox = GPack.vbox ~border_width:2
                    ~packing:(fun x -> ignore (page x)) () in
                build_dialog components ~packing:(vbox#pack)
                                        ~cleanup:(fun _ -> ()) ())
                tabs in
            notebook#goto_page !retvar;
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                List.iter (fun f -> f ()) cleanups;
                retvar := notebook#current_page;
                cleanup ()) ()
        |`O rgb :: descriptor ->
            let (r, g, b) = !rgb in
            let color = Gdk.Color.alloc (Gdk.Color.get_system_colormap ())
                       (`RGB (int_of_float (r *. 65535.0),
                              int_of_float (g *. 65535.0),
                              int_of_float (b *. 65535.0))) in
            let b = GButton.color_button ~color ~packing () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                let (r, g, b) = Gdk.Color.red b#color,
                                Gdk.Color.green b#color,
                                Gdk.Color.blue b#color in
                rgb := float r /. 65535.0, float g /. 65535.0,
                       float b /. 65535.0;
                cleanup ()) ()
        |`C (label, active) :: descriptor ->
            let b = GButton.check_button ~label ~active:!active ~packing () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                active := b#active; cleanup ()) ()
        |`S return :: descriptor ->
            let spinner = new DialSlider.dialSlider ~packing ~size:100 () in
            spinner#set_theta !return;
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                return := spinner#theta; cleanup ()) ()
        |`F (label, components) :: descriptor ->
            let f = GBin.frame ~label ~packing ~border_width:2 () in
            let v = GPack.vbox ~packing:f#add () in
            let extra_cleanup = build_dialog components ~packing:v#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                extra_cleanup (); cleanup ()) ()
        |`R ((first_label, first_setting) :: bdesc) :: descriptor ->
            let vbox = GPack.vbox ~packing ~border_width:2 () in
            let first_button = GButton.radio_button ~label:first_label
                                ~active:!first_setting ~packing:vbox#add () in
            let buttons = List.map (fun (label, setting) ->
                GButton.radio_button ~label ~active:!setting
                    ~group:first_button#group ~packing:vbox#add ()) bdesc in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                List.iter2 (fun (_, setting) button -> setting:= button#active)
                    ((first_label, first_setting) :: bdesc)
                    (first_button :: buttons);
                    cleanup ()) ()
        |`V components :: descriptor ->
            let vbox = GPack.vbox ~packing ~spacing:2 () in
            let extra_cleanup = build_dialog components ~packing:vbox#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                extra_cleanup (); cleanup ()) ()
        |`H components :: descriptor ->
            let hbox = GPack.hbox ~packing ~spacing:2 () in
            let extra_cleanup = build_dialog components ~packing:hbox#add
                                                        ~cleanup:ignore () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                extra_cleanup (); cleanup ()) ()
        |`E str :: descriptor ->
            let entry = GEdit.entry ~packing ~text:!str () in
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                str := entry#text; cleanup ()) ()
        |`L str :: descriptor ->
            GMisc.label ~text:str ~xpad:2 ~packing ~justify:`FILL ();
            build_dialog descriptor ~packing ~cleanup ()
        |`M (strings, ret) :: descriptor ->
            let cb = GEdit.combo_box_text ~packing ~strings () in
            let cb = (fun (x, _) -> x) cb in
            cb#set_active !ret;
            build_dialog descriptor ~packing ~cleanup:(fun _ ->
                ret := cb#active; cleanup ()) ()
        |[] ->
            cleanup

let generate_dialog descriptor apply title =
    let w = GWindow.dialog ~title ~border_width:2 ~resizable:false
                           ~position:`CENTER_ON_PARENT () in
    let response = ref false in
    let cleanup = build_dialog descriptor
                      ~packing:w#vbox#add
                      ~cleanup:(fun _ -> apply (); response := true) () in
    w#add_button_stock `APPLY `APPLY;
    w#add_button_stock `CANCEL `CANCEL;
    w#add_button_stock `OK `OK;
    w#set_default_response `OK;
    let rec run _ =
        begin match w#run () with
        |`OK -> cleanup ()
        |`APPLY -> cleanup (); run ()
        |_ -> () end in
    run ();
    w#destroy ();
    !response
