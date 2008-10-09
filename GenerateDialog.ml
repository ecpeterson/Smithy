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
    | `S of float ref
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
            GRange.scale orient ~adjustment:adj ~value_pos:pos ~digits:2
                                ~packing:align#add ~inverted:true ();
            adj#connect#value_changed ~callback:(fun _ ->
                v := adj#value;
                cleanup ());
            build_dialog descriptor ~packing ~cleanup ()
        |`N (tabs, retvar) :: descriptor ->
            let notebook = GPack.notebook ~tab_pos:`TOP ~packing () in
            List.map (fun (text, components) ->
                let label = GMisc.label ~text () in
                let page = notebook#append_page ~tab_label:label#coerce in
                let vbox = GPack.vbox ~border_width:2
                    ~packing:(fun x -> ignore (page x)) () in
                build_dialog components ~packing:(vbox#pack) ~cleanup ())
                tabs;
            notebook#goto_page !retvar;
            notebook#connect#switch_page ~callback:(fun new_page ->
                retvar := new_page;
                cleanup ());
            build_dialog descriptor ~packing ~cleanup ()
        |`O rgb :: descriptor ->
            let (r, g, b) = !rgb in
            let color = Gdk.Color.alloc (Gdk.Color.get_system_colormap ())
                       (`RGB (int_of_float (r *. 65535.0),
                              int_of_float (g *. 65535.0),
                              int_of_float (b *. 65535.0))) in
            let b = GButton.color_button ~color ~packing () in
            b#connect#color_set ~callback:(fun _ ->
                let (r, g, b) = Gdk.Color.red b#color,
                                Gdk.Color.green b#color,
                                Gdk.Color.blue b#color in
                rgb := float r /. 65535.0, float g /. 65535.0,
                       float b /. 65535.0;
                cleanup ());
            build_dialog descriptor ~packing ~cleanup ()
        |`C (label, active) :: descriptor ->
            let b = GButton.check_button ~label ~active:!active ~packing () in
            b#connect#toggled ~callback:(fun _ ->
                active := b#active;
                cleanup ());
            build_dialog descriptor ~packing ~cleanup ()
        |`S return :: descriptor ->
            let spinner = new DialSlider.dialSlider ~packing ~size:100 () in
            spinner#set_theta !return;
            spinner#connect_valuechanged (fun _ ->
                return := spinner#theta;
                cleanup ());
            build_dialog descriptor ~packing ~cleanup ()
        |`F (label, components) :: descriptor ->
            let f = GBin.frame ~label ~packing ~border_width:2 () in
            let v = GPack.vbox ~packing:f#add () in
            build_dialog components ~packing:v#add ~cleanup ();
            build_dialog descriptor ~packing ~cleanup ()
        |`R ((first_label, first_setting) :: bdesc) :: descriptor ->
            let callback setting button _ =
                setting := button#active;
                cleanup() in
            let vbox = GPack.vbox ~packing ~border_width:2 () in
            let first_button = GButton.radio_button ~label:first_label
                                ~active:!first_setting ~packing:vbox#add () in
            first_button#connect#toggled
                ~callback:(callback first_setting first_button);
            List.map (fun (label, setting) ->
                let button = GButton.radio_button ~label ~active:!setting
                    ~group:first_button#group ~packing:vbox#add () in
                button#connect#toggled ~callback:(callback setting button);
                button) bdesc;
            build_dialog descriptor ~packing ~cleanup ()
        |`V components :: descriptor ->
            let vbox = GPack.vbox ~packing ~spacing:2 () in
            build_dialog components ~packing:vbox#add ~cleanup ();
            build_dialog descriptor ~packing ~cleanup ()
        |`H components :: descriptor ->
            let hbox = GPack.hbox ~packing ~spacing:2 () in
            build_dialog components ~packing:hbox#add ~cleanup ();
            build_dialog descriptor ~packing ~cleanup ()
        |`E str :: descriptor ->
            let entry = GEdit.entry ~packing ~text:!str () in
            let callback _ =
                str := entry#text;
                cleanup () in
            callback ();
            entry#connect#changed ~callback;
            build_dialog descriptor ~packing ~cleanup ()
        |`L str :: descriptor ->
            GMisc.label ~text:str ~xpad:2 ~packing ~justify:`FILL ();
            build_dialog descriptor ~packing ~cleanup ()
        |`M (strings, ret) :: descriptor ->
            let cb = GEdit.combo_box_text ~packing ~strings () in
            let cb = (fun (x, _) -> x) cb in
            cb#set_active !ret;
            cb#connect#changed ~callback:(fun _ ->
                ret := cb#active;
                cleanup ());
            build_dialog descriptor ~packing ~cleanup ()
        |[] -> ()
        |_ -> raise (Failure "Invalid dialog descriptor")

let generate_dialog descriptor apply title =
    let w = GWindow.dialog ~title ~border_width:2 ~resizable:false
                           ~position:`CENTER_ON_PARENT () in
    build_dialog descriptor ~packing:w#vbox#add ~cleanup:apply ();
    w#add_button_stock `CLOSE `CLOSE;
    w#set_default_response `CLOSE;
    (* so we get the initial state drawn *)
    apply ();
    w#run ();
    w#destroy ()
