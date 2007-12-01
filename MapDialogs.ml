open MapTypes

let point_dialog point =
    let (px, py) = point#vertex () in
    let w = GWindow.dialog ~title:"Point Parameters" () in
    let box = GPack.vbox ~packing:w#action_area#add () in
    let hb1 = GPack.hbox ~packing:box#add () in
    GMisc.label ~packing:hb1#pack ~text:"X Coordinate: " ();
    let ex = GEdit.entry ~packing:hb1#add ~width:100 ~height:20
        ~text:(string_of_int px) () in
    let hb2 = GPack.hbox ~packing:box#add () in
    GMisc.label ~packing:hb2#pack ~text:"Y Coordinate: " ();
    let ey = GEdit.entry ~packing:hb2#add ~width:100 ~height:20
        ~text:(string_of_int py) () in
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
    let w = GWindow.dialog ~title:"Polygon Parameters" () in
    let box = GPack.vbox ~packing:w#action_area#add () in
    let hb1 = GPack.hbox ~packing:box#add () in
    let label = GMisc.label ~text:"Type: " ~packing:hb1#add () in
    let opt = GMenu.option_menu ~packing:hb1#add () in
    let menu = GMenu.menu ~packing:opt#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (let item = GMenu.menu_item ~label ~packing:menu#append () in
                ()))
                (*item#connect#activate (fun _ -> poly#set_kind (index + 1))))*)
        ["Normal"; "Item Impassable"; "Monster & Item Impassable"; "Hill";
         "Platform"; "Light On Trigger"; "Platform On Trigger";
         "Light Off Trigger"; "Platform Off Trigger"; "Teleporter";
         "Zone Border"; "Goal"; "Visible Monster Trigger";
         "Invisible Monster Trigger"; "Dual Monster Trigger"; "Item Trigger";
         "Must be Explored"; "Automatic Exit"];
    opt#set_history (CamlExt.to_enum MapTypes.poly_kind_descriptor (poly#kind ()) - 1);
    let hb2 = GPack.hbox ~packing:box#add () in
    GMisc.label ~packing:hb2#pack ~text:"Liquid: " ();
    let liquid = GEdit.entry ~packing:hb2#add ~width:100 ~height:20
        ~text:(string_of_int (poly#media_index ())) () in
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
    let namebox = GPack.hbox ~packing:vbox#add () in
    GMisc.label ~packing:namebox#pack ~text:"Level Name:" ();
    let level_name = GEdit.entry ~packing:namebox#add () in
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
    let hbox2 = GPack.hbox ~packing:vbox#add () in
    GMisc.label ~packing:hbox2#add ~text:"Based On:" ();
    let based_on = GEdit.entry ~packing:hbox2#add () in
    let hbox3 = GPack.hbox ~packing:vbox#add () in
    GMisc.label ~packing:hbox3#add ~text:"Tide Parameter:" ();
    let tide_parameter = GEdit.entry ~packing:hbox3#add () in
    let hbox4 = GPack.hbox ~packing:vbox#add () in
    (* facing (subcaptioned Flow Direction) belongs in hbox4 *)
    let vbox2 = GPack.vbox ~packing:hbox4#add () in
    let hbox5 = GPack.hbox ~packing:vbox2#add () in
    GMisc.label ~packing:hbox5#add ~text:"Flow Strength:" ();
    let flow_strength = GEdit.entry ~packing:hbox5#add () in
    let hbox6 = GPack.hbox ~packing:vbox2#add () in
    GMisc.label ~packing:hbox6#add ~text:"Low Tide:" ();
    let low_tide = GEdit.entry ~packing:hbox6#add () in
    let hbox7 = GPack.hbox ~packing:vbox2#add () in
    GMisc.label ~packing:hbox7#add ~text:"High Tide:" ();
    let low_tide = GEdit.entry ~packing:hbox7#add () in
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
    let w = GWindow.dialog ~title:"Light Parameters" () in
    let table = GPack.table ~columns:3 ~rows:3 ~packing:w#action_area#add () in
    let vbox11 = GPack.vbox () in
    table#attach ~left:0 ~top:0 (vbox11#coerce);
    let hbox = GPack.hbox ~packing:vbox11#add () in
    GMisc.label ~text:"Preset:" ~packing:hbox#add ();
    let hbox = GPack.hbox ~packing:vbox11#add () in
    GMisc.label ~text:"Based On:" ~packing:hbox#add ();
    let hbox = GPack.hbox ~packing:vbox11#add () in
    GMisc.label ~text:"Phase:" ~packing:hbox#add ();
    let vbox12 = GPack.vbox () in
    let stateless = GButton.check_button ~label:"Stateless" ~packing:vbox12#add () in
    let active = GButton.check_button ~label:"Initially Active"
                                      ~packing:vbox12#add () in
    let hbox = GPack.hbox ~packing:vbox12#add () in
    GMisc.label ~text:"Tag:" ~packing:hbox#add ();
    table#attach ~left:1 ~top:0 (vbox12#coerce);
    let f21 = GBin.frame ~label:"Becoming Active" () in
    let vbox21 = GPack.vbox ~packing:f21#add () in
    let hbox = GPack.hbox ~packing:vbox21#add () in
    GMisc.label ~text:"Function:" ~packing:hbox#add ();
    let ba_function = GMenu.option_menu ~packing:hbox#add () in
    let ba_menu = GMenu.menu ~packing:ba_function#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:ba_menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
    let hbox = GPack.hbox ~packing:vbox21#add () in
    GMisc.label ~text:"Period:" ~packing:hbox#add ();
    let ba_period = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox21#add () in
    GMisc.label ~text:"D Period:" ~packing:hbox#add ();
    let ba_dperiod = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox21#add () in
    GMisc.label ~text:"Intensity (%):" ~packing:hbox#add ();
    let ba_intensity = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox21#add () in
    GMisc.label ~text:"D Intensity (%):" ~packing:hbox#add ();
    let ba_dintensity = GEdit.entry ~packing:hbox#add () in
    table#attach ~left:0 ~top:1 (f21#coerce);
    let f22 = GBin.frame ~label:"Primary Active" () in
    let vbox22 = GPack.vbox ~packing:f22#add () in
    let hbox = GPack.hbox ~packing:vbox22#add () in
    GMisc.label ~text:"Function:" ~packing:hbox#add ();
    let pa_function = GMenu.option_menu ~packing:hbox#add () in
    let pa_menu = GMenu.menu ~packing:pa_function#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:pa_menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
    let hbox = GPack.hbox ~packing:vbox22#add () in
    GMisc.label ~text:"Period:" ~packing:hbox#add ();
    let pa_period = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox22#add () in
    GMisc.label ~text:"D Period:" ~packing:hbox#add ();
    let pa_dperiod = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox22#add () in
    GMisc.label ~text:"Intensity (%):" ~packing:hbox#add ();
    let pa_intensity = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox22#add () in
    GMisc.label ~text:"D Intensity (%):" ~packing:hbox#add ();
    let pa_dintensity = GEdit.entry ~packing:hbox#add () in
    table#attach ~left:1 ~top:1 (f22#coerce);
    let f23 = GBin.frame ~label:"Secondary Active" () in
    let vbox23 = GPack.vbox ~packing:f23#add () in
    let hbox = GPack.hbox ~packing:vbox23#add () in
    GMisc.label ~text:"Function:" ~packing:hbox#add ();
    let sa_function = GMenu.option_menu ~packing:hbox#add () in
    let sa_menu = GMenu.menu ~packing:sa_function#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:sa_menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
    let hbox = GPack.hbox ~packing:vbox23#add () in
    GMisc.label ~text:"Period:" ~packing:hbox#add ();
    let sa_period = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox23#add () in
    GMisc.label ~text:"D Period:" ~packing:hbox#add ();
    let sa_dperiod = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox23#add () in
    GMisc.label ~text:"Intensity (%):" ~packing:hbox#add ();
    let sa_intensity = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox23#add () in
    GMisc.label ~text:"D Intensity (%):" ~packing:hbox#add ();
    let sa_dintensity = GEdit.entry ~packing:hbox#add () in
    table#attach ~left:2 ~top:1 (f23#coerce);
    let f31 = GBin.frame ~label:"Becoming Inactive" () in
    let vbox31 = GPack.vbox ~packing:f31#add () in
    table#attach ~left:0 ~top:2 (f31#coerce);
    let hbox = GPack.hbox ~packing:vbox31#add () in
    GMisc.label ~text:"Function:" ~packing:hbox#add ();
    let bi_function = GMenu.option_menu ~packing:hbox#add () in
    let bi_menu = GMenu.menu ~packing:bi_function#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:bi_menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
    let hbox = GPack.hbox ~packing:vbox31#add () in
    GMisc.label ~text:"Period:" ~packing:hbox#add ();
    let bi_period = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox31#add () in
    GMisc.label ~text:"D Period:" ~packing:hbox#add ();
    let bi_dperiod = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox31#add () in
    GMisc.label ~text:"Intensity (%):" ~packing:hbox#add ();
    let bi_intensity = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox31#add () in
    GMisc.label ~text:"D Intensity (%):" ~packing:hbox#add ();
    let bi_dintensity = GEdit.entry ~packing:hbox#add () in
    let f32 = GBin.frame ~label:"Primary Inactive" () in
    let vbox32 = GPack.vbox ~packing:f32#add () in
    let hbox = GPack.hbox ~packing:vbox32#add () in
    GMisc.label ~text:"Function:" ~packing:hbox#add ();
    let pi_function = GMenu.option_menu ~packing:hbox#add () in
    let pi_menu = GMenu.menu ~packing:pi_function#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:pi_menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
    let hbox = GPack.hbox ~packing:vbox32#add () in
    GMisc.label ~text:"Period:" ~packing:hbox#add ();
    let pi_period = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox32#add () in
    GMisc.label ~text:"D Period:" ~packing:hbox#add ();
    let pi_dperiod = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox32#add () in
    GMisc.label ~text:"Intensity (%):" ~packing:hbox#add ();
    let pi_intensity = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox32#add () in
    GMisc.label ~text:"D Intensity (%):" ~packing:hbox#add ();
    let pi_dintensity = GEdit.entry ~packing:hbox#add () in
    table#attach ~left:1 ~top:2 (f32#coerce);
    let f33 = GBin.frame ~label:"Secondary Inactive" () in
    let vbox33 = GPack.vbox ~packing:f33#add () in
    let hbox = GPack.hbox ~packing:vbox33#add () in
    GMisc.label ~text:"Function:" ~packing:hbox#add ();
    let si_function = GMenu.option_menu ~packing:hbox#add () in
    let si_menu = GMenu.menu ~packing:si_function#set_menu () in
    CamlExt.iter_indexed (fun label index ->
        ignore (GMenu.menu_item ~label ~packing:si_menu#append ()))
        ["Constant"; "Linear"; "Smooth"; "Flicker"];
    let hbox = GPack.hbox ~packing:vbox33#add () in
    GMisc.label ~text:"Period:" ~packing:hbox#add ();
    let si_period = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox33#add () in
    GMisc.label ~text:"D Period:" ~packing:hbox#add ();
    let si_dperiod = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox33#add () in
    GMisc.label ~text:"Intensity (%):" ~packing:hbox#add ();
    let si_intensity = GEdit.entry ~packing:hbox#add () in
    let hbox = GPack.hbox ~packing:vbox33#add () in
    GMisc.label ~text:"D Intensity (%):" ~packing:hbox#add ();
    let si_dintensity = GEdit.entry ~packing:hbox#add () in
    table#attach ~left:2 ~top:2 (f33#coerce);
    w#add_button_stock `OK `OK;
    begin match w#run () with
    |_ -> () end;
    w#destroy ()

let make_light map =
    let l = new MapTypes.light in
    edit_light l;
    map#add_light l
