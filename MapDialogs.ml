open MapTypes

let labeled_entry ~label ~text ~packing =
    let hbox = GPack.hbox ~packing () in
    GMisc.label ~packing:hbox#add ~text:label ();
    GEdit.entry ~packing:hbox#add ~text ()

let point_dialog point =
    let (px, py) = point#vertex () in
    let w = GWindow.dialog ~title:"Point Parameters" () in
    let box = GPack.vbox ~packing:w#action_area#add () in
    let ex = labeled_entry ~label:"X Coordinate:" ~text:(string_of_int px)
            ~packing:box#add in
    let ey = labeled_entry ~label:"Y Coordinate:" ~text:(string_of_int py)
            ~packing:box#add in
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |`OK ->
        let vertex = (int_of_string ex#text, int_of_string ey#text) in
        point#set_vertex vertex
    |_ -> () end;
    w#destroy ()

let line_dialog line map =
    let flags = line#flags () in
    let (cw, ccw) = line#cw_poly_side_index (), line#ccw_poly_side_index () in
    let w = GWindow.dialog ~title:"Line Parameters" () in
    let box = GPack.vbox ~packing:w#action_area#add () in
    let solid = GButton.check_button ~label:"Solid"
        ~active:(List.mem SOLID flags) ~packing:box#add () in
    let transparent = GButton.check_button ~label:"Transparent"
        ~active:(List.mem TRANSPARENT flags) ~packing:box#add () in
    let empty = GButton.check_button ~label:"Empty"
        ~active:(cw = -1 && ccw = -1) ~packing:box#add () in
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |`OK ->
        let flags = if solid#active then [SOLID] else [] in
        let flags = if transparent#active then TRANSPARENT :: flags else flags in
        line#set_flags flags;
        if empty#active then begin
            let (cw, ccw) = (max cw ccw, min cw ccw) in
            if cw != -1 then map#delete_side cw;
            line#set_cw_poly_side_index (-1);
            if ccw != -1 then map#delete_side ccw;
            line#set_ccw_poly_side_index (-1)
        end else ()
    |_ -> () end;
    w#destroy ()

let poly_dialog poly =
    let deal_with_type_change kind () =
        begin match (poly#kind (), kind) with
        (* do we just want to open the platform dialog again? *)
        |(Platform, Platform) -> ()
        (* do we want to trash an old platform? *)
        |(Platform, _) -> ()
        (* do we want to create a new platform? *)
        |(_, Platform) -> ()
        (* do we need to take no special action? *)
        |_ -> () end;
        (* finally set the kind of the polgon *)
        poly#set_kind kind in
    let w = GWindow.dialog ~title:"Polygon Parameters" () in
    let box = GPack.vbox ~packing:w#action_area#add () in
    let hb1 = GPack.hbox ~packing:box#add () in
    let label = GMisc.label ~text:"Type: " ~packing:hb1#add () in
    let opt = GMenu.option_menu ~packing:hb1#add () in
    let menu = GMenu.menu ~packing:opt#set_menu () in
    CamlExt.iter_indexed (fun label index ->
            let item = GMenu.menu_item ~label ~packing:menu#append () in
            item#connect#activate (deal_with_type_change
                (CamlExt.of_enum MapTypes.poly_kind_descriptor (index + 1))))
        ["Normal"; "Item Impassable"; "Monster & Item Impassable"; "Hill";
         "Platform"; "Light On Trigger"; "Platform On Trigger";
         "Light Off Trigger"; "Platform Off Trigger"; "Teleporter";
         "Zone Border"; "Goal"; "Visible Monster Trigger";
         "Invisible Monster Trigger"; "Dual Monster Trigger"; "Item Trigger";
         "Must be Explored"; "Automatic Exit"];
    opt#set_history (CamlExt.to_enum MapTypes.poly_kind_descriptor (poly#kind ()) - 1);
    let liquid = labeled_entry ~label:"Liquid:" ~text:(string_of_int (poly#media_index ()))
            ~packing:box#add in
    w#add_button_stock `OK `OK;
    ignore (w#run ());
    begin try
        let liquid = int_of_string (liquid#text) in
        poly#set_media_index liquid;
    with _ -> () end;
    w#destroy ()

let obj_dialog obj =
    let window = GWindow.dialog ~title:"Object Parameters" () in
    let vbox = GPack.vbox ~packing:window#action_area#add () in
    let hbox = GPack.hbox ~packing:vbox#add () in
    GMisc.label ~packing:hbox#add ~text:"Group: " ();
    let opt = GMenu.option_menu ~packing:hbox#add () in
    let menu = GMenu.menu ~packing:opt#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:menu#append ()))
        ["Monster"; "Scenery"; "Object"; "Player"; "Goal"; "Sound"];
    let height_label = GMisc.label ~packing:vbox#add ~text:"Height: 0.0" () in
    let height_adj = GData.adjustment ~value:0.0 ~lower:(-9.0) ~upper:9.0 () in
    let height_scroll = GRange.scrollbar `HORIZONTAL ~packing:vbox#pack
                                         ~adjustment:height_adj () in
    height_adj#connect#changed (fun () ->
        let value = height_adj#value in
        height_label#set_text ("Height: " ^ string_of_float value));
    begin match window#run () with
    |_ -> () end;
    window#destroy ()

let info_dialog map () =
    let w = GWindow.dialog ~title:"Level Parameters" () in
    let vbox = GPack.vbox ~packing:w#action_area#add () in
    let level_name = labeled_entry ~label:"Level Name:" ~text:""
            ~packing:vbox#add in
    let twobox = GPack.hbox ~packing:vbox#add () in
    let lbox = GPack.vbox ~packing:twobox#add () in
    let fourbox = GPack.hbox ~packing:lbox#add () in
    GMisc.label ~packing:fourbox#add ~text:"Environment" ();
    let opt = GMenu.option_menu ~packing:fourbox#add () in
    let menu = GMenu.menu ~packing:opt#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:menu#append ()))
        ["Water"; "Lava"; "Sewage"; "Jjaro"; "Pfhor"];
    let fivebox = GPack.hbox ~packing:lbox#add () in
    GMisc.label ~packing:fivebox#add ~text:"Landscape" ();
    let opt = GMenu.option_menu ~packing:fivebox#add () in
    let menu = GMenu.menu ~packing:opt#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:menu#append ()))
        ["Daytime Lh'owon"; "Nighttime Lh'owon"; "Moon"; "Space"];
    let f2 = GBin.frame ~label:"Environment Type" ~packing:twobox#add () in
    let f2box = GPack.vbox ~packing:f2#add () in
    let vacuum = GButton.check_button ~label:"Vacuum" ~packing:f2box#add () in
    let rebellion = GButton.check_button ~label:"Rebellion"
                    ~packing:f2box#add () in
    let low_grav = GButton.check_button ~label:"Low Gravity"
                   ~packing:f2box#add () in
    let magnetic = GButton.check_button ~label:"Magnetic"
                   ~packing:f2box#add () in
    let threebox = GPack.hbox ~packing:vbox#add () in
    let f3 = GBin.frame ~label:"Game Type" ~packing:threebox#add () in
    let f3box = GPack.vbox ~packing:f3#add () in
    let solo = GButton.check_button ~label:"Single Player"
               ~packing:f3box#add () in
    let coop = GButton.check_button ~label:"Multiplayer Cooperative"
               ~packing:f3box#add () in
    let emfh = GButton.check_button ~label:"Multiplayer Carnage"
               ~packing:f3box#add () in
    let koth = GButton.check_button ~label:"King of the Hill"
               ~packing:f3box#add () in
    let ball = GButton.check_button ~label:"Kill the Man with the Ball"
               ~packing:f3box#add () in
    let f1 = GBin.frame ~label:"Mission Type" ~packing:threebox#add () in
    let f1box = GPack.vbox ~packing:f1#add () in
    let extermination = GButton.check_button ~label:"Extermination"
                        ~packing:f1box#add () in
    let exploration = GButton.check_button ~label:"Exploration"
                      ~packing:f1box#add () in
    let retrieval = GButton.check_button ~label:"Retrieval"
                    ~packing:f1box#add () in
    let repair = GButton.check_button ~label:"Repair" ~packing:f1box#add () in
    let rescue = GButton.check_button ~label:"Rescue" ~packing:f1box#add () in
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |_ -> () end;
    w#destroy ()

let edit_media media =
    let w = GWindow.dialog ~title:"Liquid Parameters" () in
    let vbox = GPack.vbox ~packing:w#action_area#add () in
    let hbox1 = GPack.hbox ~packing:vbox#add () in
    GMisc.label ~packing:hbox1#add ~text:"Type:" ();
    let opt = GMenu.option_menu ~packing:hbox1#add () in
    let menu = GMenu.menu ~packing:opt#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:menu#append ()))
        ["Water"; "Lava"; "Alien Goo"; "Sewage"; "Jjaro Goo"];
    let based_on = labeled_entry ~label:"Based On:" ~text:""
            ~packing:vbox#add in
    let tide_parameter = labeled_entry ~label:"Tide Parameter:" ~text:""
            ~packing:vbox#add in
    let hbox4 = GPack.hbox ~packing:vbox#add () in
    (* facing (subcaptioned Flow Direction) belongs in hbox4 *)
    let vbox2 = GPack.vbox ~packing:hbox4#add () in
    let flow_strength = labeled_entry ~label:"Flow Strength:" ~text:""
            ~packing:vbox2#add in
    let low_tide = labeled_entry ~label:"Low Tide:" ~text:""
            ~packing:vbox2#add in
    let high_tide = labeled_entry ~label:"High Tide:" ~text:""
            ~packing:vbox2#add in
    let obstructed = GButton.check_button
        ~label:"Liquid's sound obstructed by floor" ~packing:vbox#add () in
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |_ -> () end;
    w#destroy ()

let make_media map =
    let m = new MapTypes.media in
    edit_media m;
    map#add_media m

(* TODO: add an actual dialog here *)
let edit_light light =
    let generate_frame () =
        let frame = GBin.frame ~label:"Becoming Active" () in
        let vbox = GPack.vbox ~packing:frame#add () in
        let hbox = GPack.hbox ~packing:vbox#add () in
        GMisc.label ~text:"Function:" ~packing:hbox#add ();
        let func = GMenu.option_menu ~packing:hbox#add () in
        let menu = GMenu.menu ~packing:func#set_menu () in
        CamlExt.iter_indexed (fun label index ->
            ignore (GMenu.menu_item ~label ~packing:menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
        let period = labeled_entry ~label:"Period:" ~text:""
                ~packing:vbox#add in
        let dperiod = labeled_entry ~label:"D Period:" ~text:""
                ~packing:vbox#add in
        let intensity = labeled_entry ~label:"Intensity (%):" ~text:""
                ~packing:vbox#add in
        let dintensity = labeled_entry ~label:"D Intensity (%):" ~text:""
                ~packing:vbox#add in
        (frame, func, period, dperiod, intensity, dintensity) in
    let w = GWindow.dialog ~title:"Light Parameters" () in
    let table = GPack.table ~columns:3 ~rows:3 ~packing:w#action_area#add () in
    let vbox11 = GPack.vbox () in
    table#attach ~left:0 ~top:0 (vbox11#coerce);
    let hbox = GPack.hbox ~packing:vbox11#add () in
    GMisc.label ~text:"Preset:" ~packing:hbox#add ();
    let based_on = labeled_entry ~label:"Based On:" ~text:""
            ~packing:vbox11#add in
    let phase = labeled_entry ~label:"Phase:" ~text:"" ~packing:vbox11#add in
    let vbox12 = GPack.vbox () in
    let stateless = GButton.check_button ~label:"Stateless" ~packing:vbox12#add () in
    let active = GButton.check_button ~label:"Initially Active"
                                      ~packing:vbox12#add () in
    let hbox = GPack.hbox ~packing:vbox12#add () in
    GMisc.label ~text:"Tag:" ~packing:hbox#add ();
    table#attach ~left:1 ~top:0 (vbox12#coerce);
    let (f21, ba_func, ba_period, ba_dperiod, ba_intensity,
         ba_dintensity) = generate_frame () in
    table#attach ~left:0 ~top:1 (f21#coerce);
    let (f22, pa_func, pa_period, pa_dperiod, pa_intensity,
         pa_dintensity) = generate_frame () in
    table#attach ~left:1 ~top:1 (f22#coerce);
    let (f23, sa_func, sa_period, sa_dperiod, sa_intensity,
         sa_dintensity) = generate_frame () in
    table#attach ~left:2 ~top:1 (f23#coerce);
    let (f31, bi_func, bi_period, bi_dperiod, bi_intensity,
         bi_dintensity) = generate_frame () in
    table#attach ~left:0 ~top:2 (f31#coerce);
    let (f32, pi_func, pi_period, pi_dperiod, pi_intensity,
         pi_dintensity) = generate_frame () in
    table#attach ~left:1 ~top:2 (f32#coerce);
    let (f33, si_func, si_period, si_dperiod, si_intensity,
         si_dintensity) = generate_frame () in
    table#attach ~left:2 ~top:2 (f33#coerce);
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |_ -> () end;
    w#destroy ()

let make_light map =
    let l = new MapTypes.light in
    edit_light l;
    map#add_light l

let map_manager gl () =
    let w = GWindow.dialog ~title:"Map Manager" () in
    let vbox = GPack.vbox ~packing:w#action_area#add () in
    let hbox = GPack.hbox ~packing:vbox#add () in
    GMisc.label ~text:"Grid Size:" ~packing:hbox#add ();
    let entry = GEdit.entry ~packing:hbox#add
        ~text:(string_of_int (gl#grid_factor ())) () in
    let display_grid = GButton.check_button ~label:"Display Grid"
                                            ~packing:vbox#add () in
    let constrain_grid = GButton.check_button ~label:"Constrain Grid"
                                              ~packing:vbox#add () in
    let monsters = GButton.check_button ~label:"Show Monsters"
                                        ~packing:vbox#add () in
    let objects = GButton.check_button ~label:"Show Objects"
                                       ~packing:vbox#add () in
    let scenery = GButton.check_button ~label:"Show Scenery"
                                       ~packing:vbox#add () in
    let players = GButton.check_button ~label:"Show Players"
                                       ~packing:vbox#add () in
    let goals = GButton.check_button ~label:"Show Goals"
                                     ~packing:vbox#add () in
    let sounds = GButton.check_button ~label:"Show Sounds"
                                      ~packing:vbox#add () in
    let annotations = GButton.check_button ~label:"Show Annotations"
                                        ~packing:vbox#add () in
    let crosshairs = GButton.check_button ~label:"Visual Mode Crosshairs"
                                          ~packing:vbox#add () in
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |_ -> try gl#set_grid_factor (int_of_string entry#text) with _ -> () end;
    w#destroy ()
