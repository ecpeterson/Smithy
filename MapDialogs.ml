open MapTypes
open CamlExt

(* utility that constructs an edit box with a prefixed caption *)
let labeled_entry ~label ~text ~packing =
    let hbox = GPack.hbox ~packing () in
    GMisc.label ~packing:hbox#add ~text:label ();
    GEdit.entry ~packing:hbox#add ~text ()

let point_dialog point =
    let px, py = point#vertex () in
    let px, py = ref (string_of_int px), ref (string_of_int py) in
    let descriptor = [
        `V [
            `H [
                `L "X Coord:";
                `E px ];
            `H [
                `L "Y Coord:";
                `E py ] ] ] in
    GenerateDialog.generate_dialog descriptor "Edit Point";
    point#set_vertex (int_of_string !px, int_of_string !py)

let line_dialog line =
    let solid = ref (List.mem SOLID (line#flags ())) in
    let transparent = ref (List.mem TRANSPARENT (line#flags ())) in
    let (cw, ccw) = line#cw_poly_side_index (), line#ccw_poly_side_index () in
    let empty = ref (cw = -1 && ccw = -1) in
    let descriptor = [
        `V [
            `C ("Solid", solid);
            `C ("Transparent", transparent);
            `C ("Empty", empty) ] ] in
    GenerateDialog.generate_dialog descriptor "Edit Line";
    let flags = if !solid then [SOLID] else [] in
    let flags = if !transparent then TRANSPARENT :: flags else flags in
    line#set_flags flags;
    if !empty then begin
        let (cw, ccw) = (max cw ccw, min cw ccw) in
        if cw <> -1 then MapFormat.delete_side cw;
        line#set_cw_poly_side_index (-1);
        if ccw <> -1 then MapFormat.delete_side ccw;
        line#set_ccw_poly_side_index (-1)
    end

let platform_dialog plat =
    print_endline "Missing platform dialog!"

let poly_dialog poly =
    let old_kind = poly#kind () in
    let kind = ref (CamlExt.to_enum MapTypes.poly_kind_descriptor old_kind) in
    let media_index = ref (string_of_int (poly#media_index ())) in
    let descriptor = [
        `V [
            `H [
                `L "Type:";
                `M (["Normal"; "Item Impassable"; "Monster & Item Impassable";
                     "Hill"; "Base"; "Platform"; "Light On Trigger";
                     "Platform On Trigger"; "Light Off Trigger";
                     "Platform Off Trigger"; "Teleporter"; "Zone Border";
                     "Goal"; "Visible Monster Trigger";
                     "Invisible Monster Trigger"; "Dual Monster Trigger";
                     "Item Trigger"; "Must be Explored"; "Automatic Exit";
                     "Minor Ouch"; "Major Ouch"; "Glue"; "Glue Trigger";
                     "Superglue"], kind) ];
            `H [
                `L "Liquid";
                `E media_index ] ] ] in
    GenerateDialog.generate_dialog descriptor "Edit Polygon";
    let kind = CamlExt.of_enum MapTypes.poly_kind_descriptor !kind in
    let media_index = int_of_string !media_index in
    begin match old_kind, kind with
        (* do we just want to open the platform dialog again? *)
        |(Platform, Platform) ->
            let plat = !MapFormat.platforms.(poly#permutation ()) in
            platform_dialog plat
        (* do we want to trash an old platform? *)
        |(Platform, _) ->
            MapFormat.delete_platform (poly#permutation ())
        (* do we want to create a new platform? *)
        |(_, Platform) ->
            let plat = new MapTypes.platform in
            platform_dialog plat;
            let plat_idx = MapFormat.add_platform plat in
            poly#set_permutation plat_idx
        (* do we need to take no special action? *)
        |_ -> ()
    end;
    poly#set_kind kind;
    poly#set_media_index media_index

let obj_dialog obj =
    let group = ref (CamlExt.to_enum MapTypes.object_kind_descriptor
                                    (obj#kind ())) in
    let descriptor = [
        `H [
            `L "Group:";
            `M (["Monster"; "Scenery"; "Object"; "Player"; "Goal"; "Sound"],
                group) ] ] in
    GenerateDialog.generate_dialog descriptor "Edit Object";
    let group = CamlExt.of_enum MapTypes.object_kind_descriptor !group in
    obj#set_kind group;
    (* launch secondary dialog *)
    let kind = ref (obj#index ()) in
    let scenerykind = ref (match !MapFormat.environment_code with
        |MapFormat.Lava -> !kind
        |MapFormat.Water -> !kind - 13
        |MapFormat.Sewage -> !kind - 28
        |MapFormat.Pfhor -> !kind - 39
        |MapFormat.Jjaro -> !kind - 50 ) in
    let goalkind = ref (string_of_int (obj#index ())) in
    let height = ref (string_of_int ((fun (_, _, x) -> x) (obj#point ()))) in
    let hangs = ref (List.mem Hangs_From_Ceiling (obj#flags ())) in
    let teleports_in = ref (List.mem Invisible_Or_Platform (obj#flags ())) in
    let floats = ref (List.mem Floats (obj#flags ())) in
    let facing = ref (obj#facing ()) in
    let soundfacing = ref (string_of_int (obj#facing ())) in
    let blind = ref (List.mem Blind (obj#flags ())) in
    let deaf = ref (List.mem Deaf (obj#flags ())) in
    let network_only = ref (List.mem Network_Only (obj#flags ())) in
    let light_vol = ref (!facing <= 0) in
    begin match group with
        |Monster ->
            let descriptor = [
                `V [
                    `H [
                        `L "Type:";
                        `M (ItemStrings.monster_strings, kind) ];
                    `H [
                        `L "Activated By:";
                        `M ([], ref 0) ];
                    `S facing;
                    `L "Facing";
                    `H [
                        `L "Height Offset:";
                        `E height ];
                    `H [
                        `V [
                            `C ("Teleports In", teleports_in);
                            `C ("From Ceiling", hangs);
                            `C ("Teleports Out", floats) ];
                        `V [
                            `C ("Is Blind", blind);
                            `C ("Is Deaf", deaf) ] ] ] ] in
            GenerateDialog.generate_dialog descriptor "Edit Monster"
        |Scenery ->
            let strings = match !MapFormat.environment_code with
                |MapFormat.Lava   -> ItemStrings.scenery_strings_lava
                |MapFormat.Water  -> ItemStrings.scenery_strings_water
                |MapFormat.Sewage -> ItemStrings.scenery_strings_sewage
                |MapFormat.Pfhor  -> ItemStrings.scenery_strings_pfhor
                |MapFormat.Jjaro  -> ItemStrings.scenery_strings_jjaro in
            let descriptor = [
                `V [
                    `H [
                        `L "Type:";
                        `M (strings, scenerykind) ];
                    `H [
                        `L "Height Offset:";
                        `E height ];
                    `C ("From Ceiling", hangs) ] ] in
            GenerateDialog.generate_dialog descriptor "Edit Scenery"
        |Item ->
            let descriptor = [
                `V [
                    `H [
                        `L "Type:";
                        `M (ItemStrings.item_strings, kind) ];
                    `H [
                        `L "Height Offset:";
                        `E height ];
                    `H [
                        `V [
                            `C ("Teleports In", teleports_in);
                            `C ("From Ceiling", hangs) ];
                        `V [`C ("Network Only", network_only) ] ] ] ] in
            GenerateDialog.generate_dialog descriptor "Edit Item"
        |Player ->
            let descriptor = [
                `V [
                    `S facing;
                    `L "Facing";
                    `H [
                        `L "Height Offset:";
                        `E height ];
                    `C ("From Ceiling", hangs) ] ] in
            GenerateDialog.generate_dialog descriptor "Edit Player"
        |Goal ->
            let descriptor = [
                `H [
                    `L "Type:";
                    `E goalkind ] ] in
            GenerateDialog.generate_dialog descriptor "Edit Goal"
        |Sound_Source ->
            let descriptor = [
                `V [
                    `H [
                        `L "Type:";
                        `M (ItemStrings.sound_strings, kind) ];
                    `H [
                        `L "Volume / Parent Light:";
                        `E soundfacing ];
                    `H [
                        `L "Height Offset:";
                        `E height ];
                    `H [
                        `V [
                            `C ("Is On Platform", teleports_in);
                            `C ("From Ceiling", hangs) ];
                        `V [
                            `C ("Floats", floats);
                            `C ("Use Light For Volume", light_vol) ] ] ] ] in
            GenerateDialog.generate_dialog descriptor "Edit Sound"
    end;
    let kind = match group with
        |Goal -> int_of_string !goalkind
        |Scenery -> begin match !MapFormat.environment_code with
            |MapFormat.Lava -> !kind
            |MapFormat.Water -> !kind + 13
            |MapFormat.Sewage -> !kind + 28
            |MapFormat.Pfhor -> !kind + 39
            |MapFormat.Jjaro -> !kind + 50 end
        |_ -> !kind in
    let facing = match group with
        |Sound_Source -> if !light_vol then int_of_string !soundfacing
                         else int_of_string !soundfacing * -1 + 1
        |_ -> !facing in
    let flags = List.fold_left2 (fun mask (desc, _) flag ->
            if flag then mask lor desc else mask)
        0 MapTypes.object_flags_descriptor
        [!teleports_in; !hangs; !blind; !deaf; !floats; !network_only] in
    obj#set_index kind;
    let (x, y, z) = obj#point () in
    obj#set_point (x, y, int_of_string !height);
    obj#set_flags (CamlExt.of_bitflag MapTypes.object_flags_descriptor flags);
    obj#set_facing facing


let info_dialog () =
    let w = GWindow.dialog ~title:"Level Parameters" () in
    let level_name = labeled_entry ~label:"Level Name:"
        ~text:!MapFormat.level_name ~packing:w#vbox#add in
    let twobox = GPack.hbox ~packing:w#vbox#add () in
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
    let threebox = GPack.hbox ~packing:w#vbox#add () in
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
    |`OK -> begin
        MapFormat.level_name := level_name#text
    end
    |_ -> () end;
    w#destroy ()

let media_dialog media =
    (* set up the dialog *)
    let kind = ref (media#kind ()) in
    let light_parameter = ref (string_of_int (media#light_index ())) in
    let direction = ref (media#direction ()) in
    let flow_strength = ref (string_of_int (media#magnitude ())) in
    let low_tide = ref (string_of_int (media#low ())) in
    let high_tide = ref (string_of_int (media#high ())) in
    let obstructed = ref (List.mem MapTypes.Liquid_Obstructs_Sounds
                                   (media#flags ())) in
    let descriptor = [
        `V [
            `H [
                `L "Type";
                `M (["Water";"Lava";"Alien Goo";"Sewage";"Jjaro Goo"], kind) ];
            `H [
                `L "Tide Parameter:";
                `E light_parameter ];
            `H [
                `V [
                    `S direction;
                    `L "Flow Direction" ];
                `V [
                    `H [
                        `L "Flow Strength:";
                        `E flow_strength ];
                    `H [
                        `L "Low Tide:";
                        `E low_tide ];
                    `H [
                        `L "High Tide:";
                        `E high_tide ] ] ];
            `C ("Liquid's sound obstructed by floor", obstructed) ] ] in
    (* run the dialog *)
    GenerateDialog.generate_dialog descriptor "Liquid";
    (* convert all values so we don't have a partial commit *)
    let low_tide = int_of_string !low_tide in
    let high_tide = int_of_string !high_tide in
    let light_parameter = int_of_string !light_parameter in
    let flow_strength = int_of_string !flow_strength in
    (* commit to the liquid *)
    media#set_kind !kind;
    media#set_light_index light_parameter;
    media#set_direction !direction;
    media#set_magnitude flow_strength;
    media#set_low low_tide;
    media#set_high high_tide;
    media#set_flags (if !obstructed then [Liquid_Obstructs_Sounds] else [])

let make_media () =
    let m = new MapTypes.media in
    media_dialog m;
    MapFormat.add_media m

let light_dialog light =
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
    let table = GPack.table ~columns:3 ~rows:3 ~packing:w#vbox#add () in
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

let make_light () =
    let l = new MapTypes.light in
    light_dialog l;
    MapFormat.add_light l

let map_manager gl () =
    let w = GWindow.dialog ~title:"Map Manager" () in
    let display_grid = GButton.check_button ~label:"Display Grid"
                                            ~packing:w#vbox#add () in
    let constrain_grid = GButton.check_button ~label:"Constrain Grid"
                                              ~packing:w#vbox#add () in
    let monsters = GButton.check_button ~label:"Show Monsters"
                                        ~packing:w#vbox#add () in
    let objects = GButton.check_button ~label:"Show Objects"
                                       ~packing:w#vbox#add () in
    let scenery = GButton.check_button ~label:"Show Scenery"
                                       ~packing:w#vbox#add () in
    let players = GButton.check_button ~label:"Show Players"
                                       ~packing:w#vbox#add () in
    let goals = GButton.check_button ~label:"Show Goals"
                                     ~packing:w#vbox#add () in
    let sounds = GButton.check_button ~label:"Show Sounds"
                                      ~packing:w#vbox#add () in
    let annotations = GButton.check_button ~label:"Show Annotations"
                                        ~packing:w#vbox#add () in
    let crosshairs = GButton.check_button ~label:"Visual Mode Crosshairs"
                                          ~packing:w#vbox#add () in
    w#add_button_stock `OK `OK;
    (* TODO: make this dialog work *)
    w#run ();
    w#destroy ()
