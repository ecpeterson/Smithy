(*** MapDialogs.ml contains all the GTK user interfaces for map geometry
 * property editing. ***)

open MapTypes
open CamlExt
open DrawModeSettings

let point_dialog point =
    let px, py = point#vertex () in
    let px, py = ref (string_of_int px), ref (string_of_int py) in
    let descriptor = [
        `H [
            `V [`L "X Coord";
                `L "Y Coord"; ];
            `V [`E px;
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

open MapTypes
let platform_dialog plat =
    let kind = ref (plat#kind ()) in
    let speed = ref (plat#speed () |> string_of_int) in
    let delay = ref (plat#delay () |> string_of_int) in
    let automin = ref (plat#minimum_height () = -1) in
    let minheight = ref (plat#minimum_height () |> string_of_int) in
    let automax = ref (plat#maximum_height () = -1) in
    let maxheight = ref (plat#maximum_height () |> string_of_int) in
    let flags = plat#flags () in
    let is_door = ref (List.mem Plat_Door flags) in
    let initially_active = ref(List.mem Plat_Initially_Active flags) in
    let initially_extended = ref (List.mem Plat_Initially_Extended flags) in
    let player_control = ref (List.mem Plat_Controlled_By_Player flags) in
    let alien_control = ref (List.mem Plat_Controlled_By_Aliens flags) in
    let damaging = ref (List.mem Plat_Damages flags) in
    let reverse_on_bump = ref (List.mem Plat_Reverses_On_Bump flags) in
    let extends_from_floor = ref (List.mem Plat_From_Floor flags &&
                                  not (List.mem Plat_From_Ceiling flags)) in
    let extends_from_ceiling = ref (List.mem Plat_From_Ceiling flags &&
                                    not (List.mem Plat_From_Floor flags)) in
    let extends_from_both = ref (List.mem Plat_From_Ceiling flags &&
                                 List.mem Plat_From_Floor flags) in
    let floor_to_ceiling = ref (List.mem Plat_Floor_To_Ceiling flags) in
    let one_shot = ref (List.mem Plat_One_Shot flags) in
    let a_to_a_lights = ref (List.mem Plat_Activates_Light flags) in
    let a_to_a_adj_plats =
        ref (List.mem Plat_Activates_Adj_Plats_On_Activation flags) in
    let a_to_d_adj_plats =
        ref (List.mem Plat_Deactivates_Adj_Plats_On_Activation flags) in
    let adjacent_at_each_level =
        ref (List.mem Plat_Activates_Adj_Plats_At_Each_Level flags) in
    let tag = ref (plat#tag () |> string_of_int) in
    let deactivates_at_each_level =
        ref (List.mem Plat_Deactivates_At_Each_Level flags) in
    let deactivates_at_initial_level =
        ref (List.mem Plat_Deactivates_At_Initial_Level flags) in
    let deactivates_never = ref (not !deactivates_at_initial_level &&
                                 not !deactivates_at_each_level) in
    let d_to_d_lights = ref (List.mem Plat_Deactivates_Light flags) in
    let d_to_d_adj_plats =
        ref (List.mem Plat_Deactivates_Adj_Plats_On_Deactivation flags) in
    let d_to_a_adj_plats =
        ref (List.mem Plat_Activates_Adj_Plats_On_Deactivation flags) in
    let cant_deactivate = ref (List.mem Plat_Cant_Be_Activated flags) in
    let uses_native_heights = ref (List.mem Plat_Uses_Native_Heights flags) in
    let delay_before_active = ref (List.mem Plat_Delay_Before_Active flags) in
    let doesnt_activate_parent =
        ref (List.mem Plat_Does_Not_Activate_Parent flags) in
    let contracts_slower = ref (List.mem Plat_Contracts_Slowly flags) in
    let locked = ref (List.mem Plat_Locked flags) in
    let secret = ref (List.mem Plat_Secret flags) in
    let descriptor = [
        `V [
            `H [
                `V [
                    `H [`L "Type: ";
                        `M (["S'pht Door"; "S'pht Door Split";
                             "S'pht Door Locked"; "S'pht Platform Silent";
                             "S'pht Platform"; "S'pht Door Heavy"; "Pfhor Door";
                             "S'pht Platform Heavy"; "Pfhor Platform"], kind) ];
                    `H [`L "Speed: ";
                        `E speed ];
                    `H [`L "Delay: ";
                        `E delay ] ];
                `V [`C ("Autocalculate Minimum Height", automin);
                    `H [`L "Minimum Height";
                        `E minheight ];
                    `C ("Autocalculate Maximum Height", automax);
                    `H [`L "Maximum Height";
                        `E maxheight ] ];
                `V [`C ("Platform is a Door", is_door)]];
            `H [
                `F ("Initially", [
                    `C ("Active", initially_active);
                    `C ("Extended", initially_extended) ]);
                `F ("Controllable By:", [
                    `C ("Players", player_control);
                    `C ("Aliens", alien_control); ]);
                `F ("When It Hits An Obstruction It:", [
                    `C ("Causes Damage", damaging);
                    `C ("Reverses Direction", reverse_on_bump) ]);
                `F ("Extends:", [
                    `R ["From Floor", extends_from_floor;
                        "From Ceiling", extends_from_ceiling;
                        "From Both", extends_from_both];
                        `C ("Floor to Ceiling", floor_to_ceiling)])];
            `H [
                `V [
                    `F ("Activates:", [
                        `C ("Only Once", one_shot);
                        `C ("Activates Polygon Lights", a_to_a_lights);
                        `C ("Activates Adjacent Platform", a_to_a_adj_plats);
                        `C ("Deactivates Adjacent Platform", a_to_d_adj_plats);
                        `C ("Adjacent at Each Level", adjacent_at_each_level)]);
                    `H [`L "Tags: ";
                        `E tag] ];
                `F ("Deactivates:", [
                    `R ["Never", deactivates_never;
                        "At Each Level", deactivates_at_each_level;
                        "At Initial Level", deactivates_at_initial_level];
                    `C ("Deactivates Polygon Lights", d_to_d_lights);
                    `C ("Deactivates Adjacent Platform", d_to_d_adj_plats);
                    `C ("Activates Adjacent Platform", d_to_a_adj_plats)]);
                `F ("Miscellaneous:", [
                    `C ("Can't Deactivate Externally", cant_deactivate);
                    `C ("Uses Native Polygon Heights", uses_native_heights);
                    `C ("Delay Before Activation", delay_before_active);
                    `C ("Doesn't Activate Parent", doesnt_activate_parent);
                    `C ("Contracts Slower", contracts_slower);
                    `C ("Locked Door", locked);
                    `C ("Secret", secret) ]) ] ] ] in
    GenerateDialog.generate_dialog descriptor "Platform Properties";
    let speed = int_of_string !speed in
    let delay = int_of_string !delay in
    let minheight = int_of_string !minheight in
    let maxheight = int_of_string !maxheight in
    let tag = int_of_string !tag in
    plat#set_kind !kind;
    plat#set_speed speed;
    plat#set_delay delay;
    plat#set_maximum_height (if !automax then -1 else maxheight);
    plat#set_minimum_height (if !automin then -1 else minheight);
    plat#set_tag tag;
    plat#set_flags (List.fold_left2 (fun build_mask (_, new_mask) flag ->
        if flag then new_mask :: build_mask else build_mask)
        [] platform_flags_descriptor
        [!initially_active; !initially_extended; !deactivates_at_each_level;
         !deactivates_at_initial_level; !d_to_a_adj_plats; !floor_to_ceiling;
         !extends_from_floor || !extends_from_both;
         !extends_from_ceiling || !extends_from_both; !damaging;
         !doesnt_activate_parent; !one_shot; !a_to_a_lights; !d_to_d_lights;
         !player_control; !alien_control; !reverse_on_bump; !cant_deactivate;
         !uses_native_heights; !delay_before_active; !a_to_a_adj_plats;
         !a_to_d_adj_plats; !d_to_d_adj_plats; !contracts_slower;
         !adjacent_at_each_level; !locked; !secret; !is_door])

let poly_dialog poly =
    let old_kind = poly#kind () in
    let kind = ref (CamlExt.to_enum MapTypes.poly_kind_descriptor old_kind) in
    let media_index = ref (string_of_int (poly#media_index ())) in
    let descriptor = [
        `H [
            `V [`L "Type";
                `L "Liquid"; ];
            `V [`M (ItemStrings.polygon_types, kind);
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
            plat#set_polygon_index
                (CamlExt.find_in_array !MapFormat.polygons poly);
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
    let monster_kind = ref (obj#index ()) in
    let monster_facing = ref (obj#facing ()) in
    let monster_height = ref (string_of_int
                                    ((fun (_, _, x) -> x) (obj#point ()))) in
    let monster_teleports_in = ref (List.mem Invisible_Or_Platform
                                             (obj#flags ())) in
    let monster_hangs = ref (List.mem Hangs_From_Ceiling (obj#flags ())) in
    let monster_floats = ref (List.mem Floats (obj#flags ())) in
    let monster_blind = ref (List.mem Blind (obj#flags ())) in
    let monster_deaf = ref (List.mem Deaf (obj#flags ())) in
    let scenerystrings = match !MapFormat.environment_code with
        |MapFormat.Lava   -> ItemStrings.scenery_strings_lava
        |MapFormat.Water  -> ItemStrings.scenery_strings_water
        |MapFormat.Sewage -> ItemStrings.scenery_strings_sewage
        |MapFormat.Pfhor  -> ItemStrings.scenery_strings_pfhor
        |MapFormat.Jjaro  -> ItemStrings.scenery_strings_jjaro in
    let scenery_kind = ref (match !MapFormat.environment_code with
        |MapFormat.Lava -> !monster_kind
        |MapFormat.Water -> !monster_kind - 13
        |MapFormat.Sewage -> !monster_kind - 28
        |MapFormat.Pfhor -> !monster_kind - 39
        |MapFormat.Jjaro -> !monster_kind - 50 ) in
    let scenery_height = ref !monster_height in
    let scenery_hangs = ref !monster_hangs in
    let item_kind = ref !monster_kind in
    let item_height = ref !monster_height in
    let item_teleports_in = ref !monster_teleports_in in
    let item_hangs = ref !monster_hangs in
    let item_network_only = ref (List.mem Network_Only (obj#flags ())) in
    let player_facing = ref !monster_facing in
    let player_height = ref !monster_height in
    let player_hangs = ref !monster_hangs in
    let goal_kind = ref (string_of_int (obj#index ())) in
    let sound_facing = ref (string_of_int (obj#facing ())) in
    let sound_kind = ref !monster_kind in
    let sound_height = ref !monster_height in
    let sound_teleports_in = ref !monster_teleports_in in
    let sound_hangs = ref !monster_hangs in
    let sound_floats = ref !monster_floats in
    let sound_light_vol = ref (!monster_facing <= 0) in
    let descriptor = [
        `N([("Monster", [
                `H [
                    `V [`L "Type";
                        `L "Activated By"; ];
                    `V [`M (ItemStrings.monster_strings, monster_kind);
                        (* TODO: ??? *)
                        `M ([], ref 0)] ];
                `S monster_facing;
                `L "Facing";
                `H [`L "Height Offset";
                    `E monster_height ];
                `H [`V [`C ("Teleports In", monster_teleports_in);
                        `C ("From Ceiling", monster_hangs);
                        `C ("Teleports Out", monster_floats)];
                    `V [`C ("Is Blind", monster_blind);
                        `C ("Is Deaf", monster_deaf) ] ] ] );
            ("Scenery", [
                `H [
                    `V [`L "Type";
                        `L "Height Offset"; ];
                    `V [`M (scenerystrings, scenery_kind);
                        `E scenery_height ];
                ];
                `C ("From Ceiling", scenery_hangs) ] );
            ("Item", [
                `H [
                    `V [`L "Type";
                        `L "Height Offset"; ];
                    `V [`M (ItemStrings.item_strings, item_kind);
                        `E item_height ];
                ];
                `H [
                    `V [
                        `C ("Teleports In", item_teleports_in);
                        `C ("From Ceiling", item_hangs) ];
                    `V [`C ("Network Only", item_network_only) ] ] ] );
            ("Player", [
                `S player_facing;
                `L "Facing";
                `H [`L "Height Offset";
                    `E player_height ];
                `C ("From Ceiling", player_hangs) ] );
            ("Goal", [
                `H [
                    `L "Type";
                    `E goal_kind ] ] );
            ("Sound", [
                `H [
                    `V [`L "Type";
                        `L "Volume / Parent Light";
                        `L "Height Offset"; ];
                    `V [`M (ItemStrings.sound_strings, sound_kind);
                        `E sound_facing;
                        `E sound_height; ];
                ];
                `H [
                    `V [
                        `C ("Is On Platform", sound_teleports_in);
                        `C ("From Ceiling", sound_hangs) ];
                    `V [
                        `C ("Floats", sound_floats);
                        `C ("Use Light For Volume", sound_light_vol) ] ] ] ) ],
            group) ] in
    GenerateDialog.generate_dialog descriptor "Edit Object";
    let update_height h =
        let (x, y, z) = obj#point () in
        obj#set_point (x, y, int_of_string h) in
    let update_flags list =
            let flags = List.fold_left2 (fun mask (desc, _) flag ->
                    if flag then mask lor desc else mask)
                0 MapTypes.object_flags_descriptor list in
            obj#set_flags (CamlExt.of_bitflag
                                    MapTypes.object_flags_descriptor flags) in
    obj#set_kind (CamlExt.of_enum MapTypes.object_kind_descriptor !group);
    match obj#kind () with
        |Monster ->
            obj#set_index !monster_kind;
            obj#set_facing !monster_facing;
            update_height !monster_height;
            let flags = List.fold_left2 (fun mask (desc, _) flag ->
                    if flag then mask lor desc else mask)
                0 MapTypes.object_flags_descriptor
                [!monster_teleports_in; !monster_hangs; !monster_blind;
                 !monster_deaf; false; false] in
            obj#set_flags (CamlExt.of_bitflag
                                    MapTypes.object_flags_descriptor flags)
        |Scenery ->
            obj#set_index
                (match !MapFormat.environment_code with
                    |MapFormat.Lava -> !scenery_kind
                    |MapFormat.Water -> !scenery_kind + 13
                    |MapFormat.Sewage -> !scenery_kind + 28
                    |MapFormat.Pfhor -> !scenery_kind + 39
                    |MapFormat.Jjaro -> !scenery_kind + 50);
            update_height !scenery_height;
            update_flags [false; !scenery_hangs; false; false; false; false]
        |Item ->
            obj#set_index !item_kind;
            update_height !item_height;
            update_flags [!item_teleports_in; !item_hangs; false; false; false;
                          !item_network_only]
        |Player ->
            obj#set_facing !player_facing;
            update_height !player_height;
            update_flags [false; !player_hangs; false; false; false; false]
        |Goal ->
            obj#set_index (int_of_string !goal_kind)
        |Sound_Source ->
            obj#set_index !sound_kind;
            update_height !sound_height;
            update_flags [!sound_teleports_in; !sound_hangs; false; false;
                          !sound_floats; false];
            obj#set_facing (
                if !sound_light_vol then int_of_string !sound_facing
                                    else int_of_string !sound_facing * -1 + 1)
        |_ -> ()

(*
let obj_dialog obj =
    let group = ref (CamlExt.to_enum MapTypes.object_kind_descriptor
                                    (obj#kind ())) in
    let descriptor = [
        `H [
            `L "Group:";
            `M (["Monster"; "Scenery"; "Object"; "Player"; "Goal"; "Sound"],
                group) ] ] in
    GenerateDialog.generate_dialog descriptor "Edit Object";
    (* we also need to decrement the obj's old placement chunk! *)
    GeomEdit.decrement_obj obj;
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
    obj#set_facing facing;
    GeomEdit.increment_obj obj
*)

let anno_dialog anno =
    let px, py = anno#location () in
    let px, py = (string_of_int px), (string_of_int py) in
    let poly = anno#polygon_index () in
    let poly = string_of_int poly in
    let pos_str = "At X,Y: " ^ px ^ "," ^ py ^ " (Polygon: " ^ poly ^ ")" in
    let text = anno#text () in
    let text = ref text in
    let descriptor = [
        `V [
            `L pos_str;
            `E text ];
    ] in
    GenerateDialog.generate_dialog descriptor "Annotation";
    anno#set_text !text

let info_dialog _ =
    let level_name = ref !MapFormat.level_name in
    let environment_code =
        ref (CamlExt.to_enum MapFormat.environment_descriptor
                            !MapFormat.environment_code) in
    let landscape = ref !MapFormat.landscape in
    let solo = ref (List.mem MapFormat.Solo !MapFormat.entry_point_flags) in
    let coop = ref (List.mem MapFormat.Coop !MapFormat.entry_point_flags) in
    let emfh = ref (List.mem MapFormat.EMFH !MapFormat.entry_point_flags) in
    let koth = ref (List.mem MapFormat.KOTH !MapFormat.entry_point_flags) in
    let ktmwtb = ref (List.mem MapFormat.KTMWTB !MapFormat.entry_point_flags) in
    let vacuum = ref (List.mem MapFormat.Vacuum !MapFormat.environment_flags) in
    let rebellion = ref (List.mem MapFormat.Rebellion
                                  !MapFormat.environment_flags) in
    let low_gravity = ref (List.mem MapFormat.Low_Gravity
                                    !MapFormat.environment_flags) in
    let magnetic = ref (List.mem MapFormat.Magnetic
                                 !MapFormat.environment_flags) in
    let extermination = ref (List.mem MapFormat.Extermination
                                      !MapFormat.mission_flags) in
    let exploration = ref (List.mem MapFormat.Exploration
                                    !MapFormat.mission_flags) in
    let retrieval = ref (List.mem MapFormat.Retrieval
                                  !MapFormat.mission_flags) in
    let repair = ref (List.mem MapFormat.Repair !MapFormat.mission_flags) in
    let rescue = ref (List.mem MapFormat.Rescue !MapFormat.mission_flags) in
    let descriptor = [
        `V [
            `H [
                `L "Level Name";
                `E level_name ];
            `V [
                `H [
                    `H [
                        `V [`L "Environment";
                            `L "Landscape"; ];
                        `V [`M (["Water"; "Lava"; "Sewage"; "Jjaro"; "Pfhor"],
                                environment_code);
                            `M (ItemStrings.landscape_strings, landscape); ]; ];
                    `V [
                        `F ("Environment Type", [
                            `V [
                                `C ("Vacuum", vacuum);
                                `C ("Rebellion", rebellion);
                                `C ("Low Gravity", low_gravity);
                                `C ("Magnetic", magnetic) ] ] ); ] ];
                `H [
                    `F ("Game Type", [
                        `V [
                            `C ("Single Player", solo);
                            `C ("Multiplayer Cooperative", coop);
                            `C ("Multiplayer Carnage", emfh);
                            `C ("King of the Hill", koth);
                            `C ("Kill the Man with the Ball", ktmwtb) ] ] );
                    `F ("Mission Type", [
                        `V [
                            `C ("Extermination", extermination);
                            `C ("Exploration", exploration);
                            `C ("Retrieval", retrieval);
                            `C ("Repair", repair);
                            `C ("Rescue", rescue) ] ] ) ] ] ] ] in
    GenerateDialog.generate_dialog descriptor "Level Parameters";
    MapFormat.level_name := !level_name;
    MapFormat.environment_code :=
        CamlExt.of_enum MapFormat.environment_descriptor !environment_code;
    MapFormat.landscape := !landscape;
    MapFormat.entry_point_flags := List.fold_left2
        (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
        MapFormat.entry_point_descriptor
        [!solo; !coop; !emfh; !ktmwtb; !koth; false; false; false] |>
        CamlExt.of_bitflag MapFormat.entry_point_descriptor;
    MapFormat.environment_flags := List.fold_left2
        (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
        MapFormat.env_flags_descriptor
        [!vacuum; !magnetic; !rebellion; !low_gravity] |>
        CamlExt.of_bitflag MapFormat.env_flags_descriptor;
    MapFormat.mission_flags := List.fold_left2
        (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
        MapFormat.mission_descriptor
        [!extermination; !exploration; !retrieval; !repair; !rescue] |>
        CamlExt.of_bitflag MapFormat.mission_descriptor

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
    let preset = ref (CamlExt.to_enum light_kind_descriptor (light#kind ())) in
    let phase = ref (string_of_int (light#phase ())) in
    let stateless = ref (List.mem MapTypes.Stateless_Light (light#flags ())) in
    let active = ref (List.mem MapTypes.Active_Light (light#flags ())) in
    let (ba_fn, ba_period, ba_dperiod, ba_intensity, ba_dintensity) =
        light#becoming_active () in
    let (ba_fn, ba_period, ba_dperiod, ba_intensity, ba_dintensity) =
        ref ba_fn, ref (string_of_int ba_period),
        ref (string_of_int ba_dperiod), ref (string_of_float ba_intensity),
        ref (string_of_float ba_dintensity) in
    let (pa_fn, pa_period, pa_dperiod, pa_intensity, pa_dintensity) =
        light#primary_active () in
    let (pa_fn, pa_period, pa_dperiod, pa_intensity, pa_dintensity) =
        ref pa_fn, ref (string_of_int pa_period),
        ref (string_of_int pa_dperiod), ref (string_of_float pa_intensity),
        ref (string_of_float pa_dintensity) in
    let (sa_fn, sa_period, sa_dperiod, sa_intensity, sa_dintensity) =
        light#secondary_active () in
    let (sa_fn, sa_period, sa_dperiod, sa_intensity, sa_dintensity) =
        ref sa_fn, ref (string_of_int sa_period),
        ref (string_of_int sa_dperiod), ref (string_of_float sa_intensity),
        ref (string_of_float sa_dintensity) in
    let (bi_fn, bi_period, bi_dperiod, bi_intensity, bi_dintensity) =
        light#becoming_inactive () in
    let (bi_fn, bi_period, bi_dperiod, bi_intensity, bi_dintensity) =
        ref bi_fn, ref (string_of_int bi_period),
        ref (string_of_int bi_dperiod), ref (string_of_float bi_intensity),
        ref (string_of_float bi_dintensity) in
    let (pi_fn, pi_period, pi_dperiod, pi_intensity, pi_dintensity) =
        light#primary_inactive () in
    let (pi_fn, pi_period, pi_dperiod, pi_intensity, pi_dintensity) =
        ref pi_fn, ref (string_of_int pi_period),
        ref (string_of_int pi_dperiod), ref (string_of_float pi_intensity),
        ref (string_of_float pi_dintensity) in
    let (si_fn, si_period, si_dperiod, si_intensity, si_dintensity) =
        light#secondary_inactive () in
    let (si_fn, si_period, si_dperiod, si_intensity, si_dintensity) =
        ref si_fn, ref (string_of_int si_period),
        ref (string_of_int si_dperiod), ref (string_of_float si_intensity),
        ref (string_of_float si_dintensity) in
    let frame_descriptor title fn period dperiod intensity dintensity =
        `F (title, [
            `H [
                `V [`L "Function";
                    `L "Period";
                    `L "D Period";
                    `L "Intensity (%)";
                    `L "D Intensity (%)"; ];
                `V [`M (["Constant"; "Linear"; "Smooth"; "Flicker"], fn);
                    `E period;
                    `E dperiod;
                    `E intensity;
                    `E dintensity ] ] ] ) in
    let (descriptor : GenerateDialog.component list) = [
        `V [
            `H [
                `H [
                    `V [`L "Preset";
                        `L "Phase"; ];
                    `V [`M (["Normal"; "Strobe"; "Liquid Tide"], preset);
                        `E phase ] ];
                `V [
                    `C ("Stateless", stateless);
                    `C ("Initially Active", active) ] ];
            `H [
                frame_descriptor "Becoming Active" ba_fn ba_period ba_dperiod
                                 ba_intensity ba_dintensity;
                frame_descriptor "Primary Active" pa_fn pa_period pa_dperiod
                                 pa_intensity pa_dintensity;
                frame_descriptor "Secondary Active" sa_fn sa_period sa_dperiod
                                 sa_intensity sa_dintensity ];
            `H [
                frame_descriptor "Becoming Inactive" bi_fn bi_period bi_dperiod
                                 bi_intensity bi_dintensity;
                frame_descriptor "Primary Inactive" pi_fn pi_period pi_dperiod
                                 pi_intensity pi_dintensity;
                frame_descriptor "Secondary Inactive" si_fn si_period si_dperiod
                                 si_intensity si_dintensity ] ] ] in
    GenerateDialog.generate_dialog descriptor "Light Parameters";
    if ((int_of_string !pa_period) = 0 &&
        (int_of_string !sa_period) = 0) ||
       ((int_of_string !pi_period) = 0 &&
        (int_of_string !si_period) = 0) then begin
        let dialog = GWindow.message_dialog
                                ~message:"Can't set both periods to zero!"
                                ~message_type:`ERROR
                                ~buttons:GWindow.Buttons.close
                                ~modal:true ~title:Resources.warning () in
        dialog#run ();
        dialog#destroy () end else begin
    light#set_kind (CamlExt.of_enum light_kind_descriptor !preset);
    light#set_becoming_active (!ba_fn, int_of_string !ba_period,
        int_of_string !ba_dperiod, float_of_string !ba_intensity,
        float_of_string !ba_dintensity);
    light#set_primary_active (!pa_fn, int_of_string !pa_period,
        int_of_string !pa_dperiod, float_of_string !pa_intensity,
        float_of_string !pa_dintensity);
    light#set_secondary_active (!sa_fn, int_of_string !sa_period,
        int_of_string !sa_dperiod, float_of_string !sa_intensity,
        float_of_string !sa_dintensity);
    light#set_becoming_inactive (!bi_fn, int_of_string !bi_period,
        int_of_string !bi_dperiod, float_of_string !bi_intensity,
        float_of_string !bi_dintensity);
    light#set_primary_inactive (!pi_fn, int_of_string !pi_period,
        int_of_string !pi_dperiod, float_of_string !pi_intensity,
        float_of_string !pi_dintensity);
    light#set_secondary_inactive (!si_fn, int_of_string !si_period,
        int_of_string !si_dperiod, float_of_string !si_intensity,
        float_of_string !si_dintensity);
    light#set_phase (int_of_string !phase);
    List.fold_left2
        (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
        MapTypes.light_flag_descriptor [!active; false; !stateless]
        |> CamlExt.of_bitflag MapTypes.light_flag_descriptor
        |> light#set_flags
    end

let make_light () =
    let l = new MapTypes.light in
    light_dialog l;
    MapFormat.add_light l

let map_manager drawer _ =
    let descriptor = [
        `V [
            `H [
                `L "Grid Size:";
                `M (["2 WU"; "1 WU"; "1/2 WU"; "1/4 WU"; "1/8 WU"],
                    grid_factor) ];
            `C ("Display Grid", display_grid);
            `C ("Constrain to Grid", constrain_to_grid);
            `C ("Show Monsters", show_monsters);
            `C ("Show Objects", show_objects);
            `C ("Show Scenery", show_scenery);
            `C ("Show Players", show_players);
            `C ("Show Goals", show_goals);
            `C ("Show Sounds", show_sounds);
            `C ("Show Annotations", show_annotations);
            `C ("Visual Mode Crosshairs", vm_crosshair) ] ] in
    GenerateDialog.generate_dialog descriptor "Map Manager";
    drawer#draw ()

let random_dialog random =
    let index = ref (random#index ()) in
    let volume = ref (random#volume () |> string_of_int) in
    let dvolume = ref (random#dvolume () |> string_of_int) in
    let period = ref (random#period () |> string_of_int) in
    let dperiod = ref (random#dperiod () |> string_of_int) in
    let pitch = ref (random#pitch () |> string_of_int) in
    let dpitch = ref (random#dpitch () |> string_of_int) in
    let nondirectional = ref (random#direction () = -1) in
    let direction = ref (random#direction ()) in
    let ddirection = ref (random#ddirection ()) in
    let descriptor = [
        `V [
            `H [`L "Type";
                `M (ItemStrings.random_sound_strings, index) ];
            `H [
                `H [
                    `V [`L "Volume";
                        `L "D Volume";
                        `L "Period";
                        `L "D Period";
                        `L "Pitch";
                        `L "D Pitch"; ];
                    `V [`E volume;
                        `E dvolume;
                        `E period;
                        `E dperiod;
                        `E pitch;
                        `E dpitch]];
                `V [
                    `C ("Non-Directional", nondirectional);
                    `S direction;
                    `L "Direction";
                    `S ddirection;
                    `L "D Direction" ] ] ] ] in
    GenerateDialog.generate_dialog descriptor "Random Sound Parameters";
    let volume = int_of_string !volume in
    let dvolume = int_of_string !dvolume in
    let period = int_of_string !period in
    let dperiod = int_of_string !dperiod in
    let pitch = int_of_string !pitch in
    let dpitch = int_of_string !dpitch in
    random#set_volume volume;
    random#set_dvolume dvolume;
    random#set_period period;
    random#set_dperiod dperiod;
    random#set_pitch pitch;
    random#set_dpitch dpitch;
    if !nondirectional then
        random#set_direction (-1)
    else
        random#set_direction !direction;
    random#set_ddirection !ddirection
    (* TODO: this dialog doesn't have a spot for pitch.  hmm. *)

let make_random () =
    let random = new MapTypes.random in
    random_dialog random;
    MapFormat.add_random random

let ambient_dialog ambient =
    let index = ref (ambient#index ()) in
    let volume = ref (ambient#volume () |> string_of_int) in
    let descriptor = [
        `H [
            `V [`L "Type ";
                `L "Volume "; ];
            `V [`M (ItemStrings.sound_strings, index);
                `E volume ] ] ] in
    GenerateDialog.generate_dialog descriptor "Ambient Sound Parameters";
    ambient#set_volume (int_of_string !volume);
    ambient#set_index !index

let make_ambient () =
    let ambient = new MapTypes.ambient in
    ambient_dialog ambient;
    MapFormat.add_ambient ambient

let goto drawer _ =
    let dialog = GWindow.dialog ~title:"Goto" () in
    let kind, id =
        let hbox = GPack.hbox ~packing:dialog#vbox#add () in
        GMisc.label ~text:"Type: " ~packing:hbox#add ();
        let cb, _ = GEdit.combo_box_text ~packing:hbox#add
                                    ~strings:["Point"; "Line"; "Polygon"] () in
        cb#set_active 2;
        let hbox = GPack.hbox ~packing:dialog#vbox#add () in
        GMisc.label ~text:"ID: " ~packing:hbox#add ();
        let entry = GEdit.entry ~packing:hbox#add ~text:"0" () in
        (cb, entry) in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_button_stock `OK `OK;
    begin match dialog#run () with
    |`OK ->
        let kind, id = kind#active, int_of_string id#text in
        begin match kind with
        |0 ->
            let p = !MapFormat.points.(id) in
            DrawModeSettings.highlight := Point [id];
            drawer#center_on (p#vertex ())
        |1 ->
            let l = !MapFormat.lines.(id) in
            DrawModeSettings.highlight := Line [id];
            let p0, p1 = l#endpoints () in
            let p0x, p0y = !MapFormat.points.(p0)#vertex () in
            let p1x, p1y = !MapFormat.points.(p1)#vertex () in
            drawer#center_on ((p0x + p1x)/2, (p0y + p1y)/2)
        |2 ->
            let p = !MapFormat.polygons.(id) in
            DrawModeSettings.highlight := Poly [id];
            Array.sub (p#endpoint_indices ()) 0 (p#vertex_count ()) |>
                GeomEdit.point_center |>
                drawer#center_on
        |_ -> raise (Failure "Invalid Goto kind!") end
    |_ -> () end;
    dialog#destroy ()

let map_height_window = ref None
let map_height_dlg drawer _ =
    if !map_height_window = None then begin
    let fadj = GData.adjustment ~value:!DrawModeSettings.floor_cutoff
                                ~lower:(-9.0) ~upper:(9.01) ~step_incr:0.01
                                ~page_incr:18.0 ~page_size:0.01 () in
    let cadj = GData.adjustment ~value:!DrawModeSettings.ceiling_cutoff
                                ~lower:(-9.0) ~upper:(9.01) ~step_incr:0.01
                                ~page_incr:18.0 ~page_size:0.01 () in
    fadj#connect#value_changed ~callback:(fun _ ->
        DrawModeSettings.floor_cutoff := fadj#value;
        drawer#draw ());
    cadj#connect#value_changed ~callback:(fun _ ->
        DrawModeSettings.ceiling_cutoff := cadj#value;
        drawer#draw ());
    let dialog = GWindow.window ~title:"Height Window" ~height:300
                                ~show:false ~border_width:5 () in
    dialog#event#connect#delete ~callback:(fun _ ->
        map_height_window := None; false);
    let hbox = GPack.hbox ~packing:dialog#add () in
    let vbox = GPack.vbox ~packing:hbox#add () in
    let floor_slider = GRange.scale `VERTICAL ~adjustment:fadj
            ~value_pos:`LEFT ~digits:2 ~packing:vbox#add ~inverted:true () in
    GMisc.label ~text:"Floor" ~packing:vbox#pack ();
    let vbox = GPack.vbox ~packing:hbox#add () in
    let ceiling_slider = GRange.scale `VERTICAL ~adjustment:cadj
            ~value_pos:`RIGHT ~digits:2 ~packing:vbox#add ~inverted:true () in
    GMisc.label ~text:"Ceiling" ~packing:vbox#pack ();
    map_height_window := Some dialog;
    dialog#show () end else
        let Some dialog = !map_height_window in dialog#present ()

let color_prefs_dialog drawer _ =
    let thickness = ref (string_of_int
                        !DrawModeSettings.highlighted_point_thickness) in
    let looseness = ref (string_of_float !DrawModeSettings.pixel_epsilon) in
    let saturation = ref (string_of_float !Colors.poly_type_saturation) in
    let value = ref (string_of_float !Colors.poly_type_value) in
    let descriptor = [
        `H [
            `V [
                `L "Background color";
                `L "Grid color";
                `L "Anchor point color";
                `L "Point color";
                `L "Solid line color";
                `L "Transparent line color";
                `L "Passable line color";
                `L "Polygon color";
                `L "Invalid polygon color";
                `L "Highlight color";
                `L "Highlight thickness";
                `L "Click looseness";
                `L "Polygon Type Saturation";
                `L "Polygon Type Value";
            ];
            `V [
                `O Colors.background_color;
                `O Colors.grid_color;
                `O Colors.anchor_point_color;
                `O Colors.point_color;
                `O Colors.solid_line_color;
                `O Colors.transparent_line_color;
                `O Colors.passable_line_color;
                `O Colors.polygon_color;
                `O Colors.invalid_polygon;
                `O Colors.highlight_color;
                `E thickness;
                `E looseness;
                `E saturation;
                `E value ] ] ] in
    GenerateDialog.generate_dialog descriptor "Color Preferences";
    DrawModeSettings.highlighted_point_thickness := int_of_string !thickness;
    DrawModeSettings.pixel_epsilon := float_of_string !looseness;
    Colors.poly_type_saturation := float_of_string !saturation;
    Colors.poly_type_value := float_of_string !value;
    drawer#draw ()
