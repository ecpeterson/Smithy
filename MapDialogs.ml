(*** MapDialogs.ml contains all the GTK user interfaces for map geometry
 * property editing. ***)
open CamlExt
open MapTypes
open DrawModeSettings

let point_dialog point redraw =
    let px, py = point#vertex in
    let px, py = ref (string_of_float px), ref (string_of_float py) in
    let descriptor = [
        `H [
            `V [`L "X Coord";
                `L "Y Coord"; ];
            `V [`E px;
                `E py ] ] ] in
    let apply _ =
        point#set_vertex (float_of_string !px, float_of_string !py);
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Edit Point"

let line_dialog line redraw =
    let solid = ref (List.mem SOLID line#flags) in
    let transparent = ref (List.mem TRANSPARENT line#flags) in
    let cw, ccw = line#cw_poly_side_index, line#ccw_poly_side_index in
    let empty = ref (cw = -1 && ccw = -1) in
    let descriptor = [
        `V [
            `C ("Solid", solid);
            `C ("Transparent", transparent);
            `C ("Empty", empty) ] ] in
    let apply _ =
        let flags = if !solid then [SOLID] else [] in
        let flags = if !transparent then TRANSPARENT :: flags else flags in
        line#set_flags flags;
        if !empty then begin
            let (cw, ccw) = (max cw ccw, min cw ccw) in
            if cw <> -1 then MapFormat.delete_side cw;
            line#set_cw_poly_side_index (-1);
            if ccw <> -1 then MapFormat.delete_side ccw;
            line#set_ccw_poly_side_index (-1)
        end;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Edit Line"

let platform_dialog plat redraw =
    let kind = ref plat#kind in
    let speed = ref (string_of_float plat#speed) in
    let delay = ref (string_of_float plat#delay) in
    let automin = ref (plat#minimum_height = -1.0) in
    let minheight = ref (string_of_float plat#minimum_height) in
    let automax = ref (plat#maximum_height = -1.0) in
    let maxheight = ref (string_of_float plat#maximum_height) in
    let flags = plat#flags in
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
    let tag = ref (string_of_int plat#tag) in
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
                `H [
                    `V [`L "Type";
                        `L "Speed";
                        `L "Delay"; ];
                    `V [`M (["S'pht Door"; "S'pht Door Split";
                             "S'pht Door Locked"; "S'pht Platform Silent";
                             "S'pht Platform"; "S'pht Door Heavy"; "Pfhor Door";
                             "S'pht Platform Heavy"; "Pfhor Platform"], kind);
                        `E speed;
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
                `F ("Controllable By", [
                    `C ("Players", player_control);
                    `C ("Aliens", alien_control); ]);
                `F ("When It Hits An Obstruction It", [
                    `C ("Causes Damage", damaging);
                    `C ("Reverses Direction", reverse_on_bump) ]);
                `F ("Extends", [
                    `R ["From Floor", extends_from_floor;
                        "From Ceiling", extends_from_ceiling;
                        "From Both", extends_from_both];
                        `C ("Floor to Ceiling", floor_to_ceiling)])];
            `H [
                `V [
                    `F ("Activates", [
                        `C ("Only Once", one_shot);
                        `C ("Activates Polygon Lights", a_to_a_lights);
                        `C ("Activates Adjacent Platform", a_to_a_adj_plats);
                        `C ("Deactivates Adjacent Platform", a_to_d_adj_plats);
                        `C ("Adjacent at Each Level", adjacent_at_each_level)]);
                    `H [`L "Tags ";
                        `E tag] ];
                `F ("Deactivates", [
                    `R ["Never", deactivates_never;
                        "At Each Level", deactivates_at_each_level;
                        "At Initial Level", deactivates_at_initial_level];
                    `C ("Deactivates Polygon Lights", d_to_d_lights);
                    `C ("Deactivates Adjacent Platform", d_to_d_adj_plats);
                    `C ("Activates Adjacent Platform", d_to_a_adj_plats)]);
                `F ("Miscellaneous", [
                    `C ("Can't Deactivate Externally", cant_deactivate);
                    `C ("Uses Native Polygon Heights", uses_native_heights);
                    `C ("Delay Before Activation", delay_before_active);
                    `C ("Doesn't Activate Parent", doesnt_activate_parent);
                    `C ("Contracts Slower", contracts_slower);
                    `C ("Locked Door", locked);
                    `C ("Secret", secret) ]) ] ] ] in
    let apply _ =
        let speed = float_of_string !speed in
        let delay = float_of_string !delay in
        let minheight = float_of_string !minheight in
        let maxheight = float_of_string !maxheight in
        let tag = int_of_string !tag in
        plat#set_kind !kind;
        plat#set_speed speed;
        plat#set_delay delay;
        plat#set_maximum_height (if !automax then -1.0 else maxheight);
        plat#set_minimum_height (if !automin then -1.0 else minheight);
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
            !adjacent_at_each_level; !locked; !secret; !is_door]);
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Platform Properties"

let poly_dialog poly redraw =
    let old_kind = poly#kind in
    let kind = ref (to_enum MapTypes.poly_kind_descriptor old_kind) in
    let media_index = ref (string_of_int poly#media_index) in
    let permutation = ref (string_of_int poly#permutation) in
    let descriptor = [
        `H [
            `V [`L "Type";
                `L "Liquid";
                `L "Permutation"];
            `V [`M (ItemStrings.polygon_types, kind);
                `E media_index;
                `E permutation] ] ] in
    let apply _ =
        let kind = of_enum MapTypes.poly_kind_descriptor !kind in
        let media_index = int_of_string !media_index in
        let permutation = int_of_string !permutation in
        begin match old_kind, kind with
            (* do we just want to open the platform dialog again? *)
            |(Platform, Platform) ->
                let plat = !MapFormat.platforms.(poly#permutation) in
                platform_dialog plat redraw;
                ()
            (* do we want to trash an old platform? *)
            |(Platform, _) ->
                MapFormat.delete_platform poly#permutation
            (* do we want to create a new platform? *)
            |(_, Platform) ->
                let plat = new MapTypes.platform in
                plat#set_polygon_index
                    (find_in_array !MapFormat.polygons poly);
                platform_dialog plat redraw;
                let plat_idx = MapFormat.add_platform plat in
                poly#set_permutation plat_idx
            (* do we need to take no special action? *)
            |_ -> ()
        end;
        poly#set_permutation permutation;
        poly#set_kind kind;
        poly#set_media_index media_index;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Edit Polygon"

let obj_dialog obj redraw =
    let group = ref (to_enum MapTypes.object_kind_descriptor obj#kind) in
    let monster_kind = ref obj#index in
    let monster_facing = ref obj#facing in
    let monster_height = ref (string_of_float
                                    ((fun (_, _, x) -> x) obj#point)) in
    let monster_teleports_in = ref (List.mem Invisible_Or_Platform
                                             obj#flags) in
    let monster_hangs = ref (List.mem Hangs_From_Ceiling obj#flags) in
    let monster_floats = ref (List.mem Floats obj#flags) in
    let monster_blind = ref (List.mem Blind obj#flags) in
    let monster_deaf = ref (List.mem Deaf obj#flags) in
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
    let item_network_only = ref (List.mem Network_Only obj#flags) in
    let player_facing = ref !monster_facing in
    let player_height = ref !monster_height in
    let player_hangs = ref !monster_hangs in
    let goal_kind = ref (string_of_int obj#index) in
    let sound_facing = ref (string_of_float obj#facing) in
    let sound_kind = ref !monster_kind in
    let sound_height = ref !monster_height in
    let sound_teleports_in = ref !monster_teleports_in in
    let sound_hangs = ref !monster_hangs in
    let sound_floats = ref !monster_floats in
    let sound_light_vol = ref (obj#facing <= 0.0) in
    let activation_idx = ref
        (match CamlExt.of_bitflag object_flags_descriptor (0xf000 land
            (CamlExt.to_bitflag object_flags_descriptor obj#flags)) with
            |[Activate_On_Player] -> 0
            |[Activate_On_Nearest_Hostile] -> 1
            |[Activate_On_Goal] -> 2
            |[Activate_Randomly] -> 3
            |_ -> 0) in
    let descriptor = [
        `N([("Monster", [
                `H [
                    `V [`L "Type";
                        `L "Activated By"; ];
                    `V [`M (ItemStrings.monster_strings, monster_kind);
                        (* TODO: ??? *)
                        `M (ItemStrings.activation_strings, activation_idx)] ];
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
    let apply _ =
        let update_height h =
            let (x, y, z) = obj#point in
            obj#set_point (x, y, float_of_string h) in
        let update_flags list =
            let flags = List.fold_left2
                (fun mask (desc, _) flag ->
                    if flag then mask lor desc else mask)
                0 MapTypes.object_flags_descriptor list in
            obj#set_flags (of_bitflag
                MapTypes.object_flags_descriptor flags) in
        obj#set_kind (of_enum MapTypes.object_kind_descriptor !group);
        begin match obj#kind with
            |Monster ->
                obj#set_index !monster_kind;
                obj#set_facing !monster_facing;
                update_height !monster_height;
                let flags = List.fold_left2 (fun mask (desc, _) flag ->
                        if flag then mask lor desc else mask)
                    0 MapTypes.object_flags_descriptor
                    [!monster_teleports_in; !monster_hangs; !monster_blind;
                     !monster_deaf; false; false; !activation_idx = 0;
                     !activation_idx = 1; !activation_idx = 2;
                     !activation_idx = 3] in
                obj#set_flags
                    (of_bitflag MapTypes.object_flags_descriptor flags)
            |Scenery ->
                obj#set_index
                    (match !MapFormat.environment_code with
                        |MapFormat.Lava -> !scenery_kind
                        |MapFormat.Water -> !scenery_kind + 13
                        |MapFormat.Sewage -> !scenery_kind + 28
                        |MapFormat.Pfhor -> !scenery_kind + 39
                        |MapFormat.Jjaro -> !scenery_kind + 50);
                update_height !scenery_height;
                update_flags [false; !scenery_hangs; false; false; false;
                    false; false; false; false; false]
            |Item ->
                obj#set_index !item_kind;
                update_height !item_height;
                update_flags [!item_teleports_in; !item_hangs; false; false;
                          false; !item_network_only; false; false; false; false]
            |Player ->
                obj#set_facing !player_facing;
                update_height !player_height;
                update_flags [false; !player_hangs; false; false; false; false;
                              false; false; false; false]
            |Goal ->
                obj#set_index (int_of_string !goal_kind)
            |Sound_Source ->
                obj#set_index !sound_kind;
                update_height !sound_height;
                update_flags [!sound_teleports_in; !sound_hangs; false; false;
                              !sound_floats; false; false; false; false; false];
                obj#set_facing
                    (if !sound_light_vol then float_of_string !sound_facing
                     else float_of_string !sound_facing *. (-1.) +. 1.)
        end;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Edit Object"

let anno_dialog anno redraw =
    let px, py = anno#location in
    let px, py = (string_of_float px), (string_of_float py) in
    let poly = anno#polygon_index in
    let poly = string_of_int poly in
    let pos_str = "At X,Y: " ^ px ^ "," ^ py ^ " (Polygon: " ^ poly ^ ")" in
    let text = anno#text in
    let text = ref text in
    let descriptor = [
        `V [
            `L pos_str;
            `E text ];
    ] in
    let apply _ =
        anno#set_text !text;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Annotation"

let info_dialog redraw =
    let level_name = ref !MapFormat.level_name in
    let environment_code =
        ref (to_enum MapFormat.environment_descriptor
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
    let apply _ =
        MapFormat.level_name := !level_name;
        MapFormat.environment_code :=
            of_enum MapFormat.environment_descriptor !environment_code;
        MapFormat.landscape := !landscape;
        MapFormat.entry_point_flags := List.fold_left2
            (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
            MapFormat.entry_point_descriptor
            [!solo; !coop; !emfh; !ktmwtb; !koth; false; false; false] |>
            of_bitflag MapFormat.entry_point_descriptor;
        MapFormat.environment_flags := List.fold_left2
            (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
            MapFormat.env_flags_descriptor
            [!vacuum; !magnetic; !rebellion; !low_gravity] |>
            of_bitflag MapFormat.env_flags_descriptor;
        MapFormat.mission_flags := List.fold_left2
            (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
            MapFormat.mission_descriptor
            [!extermination; !exploration; !retrieval; !repair; !rescue] |>
            of_bitflag MapFormat.mission_descriptor;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Level Parameters"

let media_dialog media redraw =
    (* set up the dialog *)
    let kind = ref media#kind in
    let light_parameter = ref (string_of_int media#light_index) in
    let direction = ref media#direction in
    let flow_strength = ref (string_of_float media#magnitude) in
    let low_tide = ref (string_of_float media#low) in
    let high_tide = ref (string_of_float media#high) in
    let obstructed = ref (List.mem MapTypes.Liquid_Obstructs_Sounds
                                   media#flags) in
    let descriptor = [
        `V [
            `H [
                `V [`L "Type";
                    `L "Tide Parameter"; ];
                `V [`M (["Water";"Lava";"Alien Goo";"Sewage";"Jjaro Goo"],
                        kind);
                    `E light_parameter ]; ];
            `H [
                `V [
                    `S direction;
                    `L "Flow Direction" ];
                `H [
                    `V [`L "Flow Strength";
                        `L "Low Tide";
                        `L "High Tide"; ];
                    `V [`E flow_strength;
                        `E low_tide;
                        `E high_tide ] ] ];
            `C ("Liquid's sound obstructed by floor", obstructed) ] ] in
    let apply _ =
        (* convert all values so we don't have a partial commit *)
        let light_parameter = int_of_string !light_parameter in
        let low_tide = float_of_string !low_tide in
        let high_tide = float_of_string !high_tide in
        let flow_strength = float_of_string !flow_strength in
        (* commit to the liquid *)
        media#set_kind !kind;
        media#set_light_index light_parameter;
        media#set_direction (!direction /. twopi);
        media#set_magnitude flow_strength;
        media#set_low low_tide;
        media#set_high high_tide;
        media#set_flags (if !obstructed then [Liquid_Obstructs_Sounds] else []);
        redraw () in
    (* run the dialog *)
    GenerateDialog.generate_dialog descriptor apply "Liquid"

let make_media redraw =
    let m = new MapTypes.media in
    media_dialog m redraw;
    MapFormat.add_media m

let light_dialog light redraw =
    let preset = ref (to_enum light_kind_descriptor light#kind) in
    let phase = ref (string_of_float light#phase) in
    let stateless = ref (List.mem MapTypes.Stateless_Light light#flags) in
    let active = ref (List.mem MapTypes.Active_Light light#flags) in
    let (ba_fn, ba_period, ba_dperiod, ba_intensity, ba_dintensity) =
        light#becoming_active in
    let (ba_fn, ba_period, ba_dperiod, ba_intensity, ba_dintensity) =
        ref ba_fn, ref (string_of_float ba_period),
        ref (string_of_float ba_dperiod), ref (string_of_float ba_intensity),
        ref (string_of_float ba_dintensity) in
    let (pa_fn, pa_period, pa_dperiod, pa_intensity, pa_dintensity) =
        light#primary_active in
    let (pa_fn, pa_period, pa_dperiod, pa_intensity, pa_dintensity) =
        ref pa_fn, ref (string_of_float pa_period),
        ref (string_of_float pa_dperiod), ref (string_of_float pa_intensity),
        ref (string_of_float pa_dintensity) in
    let (sa_fn, sa_period, sa_dperiod, sa_intensity, sa_dintensity) =
        light#secondary_active in
    let (sa_fn, sa_period, sa_dperiod, sa_intensity, sa_dintensity) =
        ref sa_fn, ref (string_of_float sa_period),
        ref (string_of_float sa_dperiod), ref (string_of_float sa_intensity),
        ref (string_of_float sa_dintensity) in
    let (bi_fn, bi_period, bi_dperiod, bi_intensity, bi_dintensity) =
        light#becoming_inactive in
    let (bi_fn, bi_period, bi_dperiod, bi_intensity, bi_dintensity) =
        ref bi_fn, ref (string_of_float bi_period),
        ref (string_of_float bi_dperiod), ref (string_of_float bi_intensity),
        ref (string_of_float bi_dintensity) in
    let (pi_fn, pi_period, pi_dperiod, pi_intensity, pi_dintensity) =
        light#primary_inactive in
    let (pi_fn, pi_period, pi_dperiod, pi_intensity, pi_dintensity) =
        ref pi_fn, ref (string_of_float pi_period),
        ref (string_of_float pi_dperiod), ref (string_of_float pi_intensity),
        ref (string_of_float pi_dintensity) in
    let (si_fn, si_period, si_dperiod, si_intensity, si_dintensity) =
        light#secondary_inactive in
    let (si_fn, si_period, si_dperiod, si_intensity, si_dintensity) =
        ref si_fn, ref (string_of_float si_period),
        ref (string_of_float si_dperiod), ref (string_of_float si_intensity),
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
    let apply _ =
        if ((float_of_string !pa_period) = 0.0 &&
            (float_of_string !sa_period) = 0.0) ||
        ((float_of_string !pi_period) = 0.0 &&
            (float_of_string !si_period) = 0.0) then begin
            let dialog = GWindow.message_dialog
                                    ~message:"Can't set both periods to zero!"
                                    ~message_type:`ERROR
                                    ~buttons:GWindow.Buttons.close
                                    ~modal:true ~title:Resources.warning () in
            dialog#run ();
            dialog#destroy () end else begin
        light#set_kind (of_enum light_kind_descriptor !preset);
        light#set_becoming_active (!ba_fn, float_of_string !ba_period,
            float_of_string !ba_dperiod, float_of_string !ba_intensity,
            float_of_string !ba_dintensity);
        light#set_primary_active (!pa_fn, float_of_string !pa_period,
            float_of_string !pa_dperiod, float_of_string !pa_intensity,
            float_of_string !pa_dintensity);
        light#set_secondary_active (!sa_fn, float_of_string !sa_period,
            float_of_string !sa_dperiod, float_of_string !sa_intensity,
            float_of_string !sa_dintensity);
        light#set_becoming_inactive (!bi_fn, float_of_string !bi_period,
            float_of_string !bi_dperiod, float_of_string !bi_intensity,
            float_of_string !bi_dintensity);
        light#set_primary_inactive (!pi_fn, float_of_string !pi_period,
            float_of_string !pi_dperiod, float_of_string !pi_intensity,
            float_of_string !pi_dintensity);
        light#set_secondary_inactive (!si_fn, float_of_string !si_period,
            float_of_string !si_dperiod, float_of_string !si_intensity,
            float_of_string !si_dintensity);
        light#set_phase (float_of_string !phase);
        List.fold_left2
            (fun mask (desc, _) flag -> if flag then mask lor desc else mask) 0
            MapTypes.light_flag_descriptor [!active; false; !stateless]
            |> of_bitflag MapTypes.light_flag_descriptor
            |> light#set_flags
        end;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Light Parameters"

let make_light redraw =
    let l = new MapTypes.light in
    light_dialog l redraw;
    MapFormat.add_light l

let map_manager redraw =
    let lg x = (log x) /. (log 2.0) in
    let gf = ref (1 - (int_of_float (lg !grid_factor))) in
    let descriptor = [
        `V [
            `H [
                `L "Grid Size";
                `M (["2 WU"; "1 WU"; "1/2 WU"; "1/4 WU"; "1/8 WU"], gf) ];
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
    GenerateDialog.generate_dialog descriptor (fun _ ->
            grid_factor := 2.0 ** (1.0 -. (float !gf)); redraw ())
        "Map Manager"

let random_dialog random redraw =
    let index = ref random#index in
    let volume = ref (string_of_float random#volume) in
    let dvolume = ref (string_of_float random#dvolume) in
    let period = ref (string_of_float random#period) in
    let dperiod = ref (string_of_float random#dperiod) in
    let pitch = ref (string_of_float random#pitch) in
    let dpitch = ref (string_of_float random#dpitch) in
    let nondirectional = ref (random#direction = -1.0) in
    let direction = ref random#direction in
    let ddirection = ref random#ddirection in
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
    let apply _ =
        let volume = float_of_string !volume in
        let dvolume = float_of_string !dvolume in
        let period = float_of_string !period in
        let dperiod = float_of_string !dperiod in
        let pitch = float_of_string !pitch in
        let dpitch = float_of_string !dpitch in
        random#set_index !index;
        random#set_volume volume;
        random#set_dvolume dvolume;
        random#set_period period;
        random#set_dperiod dperiod;
        random#set_pitch pitch;
        random#set_dpitch dpitch;
        if !nondirectional then random#set_direction (-1.0)
        else random#set_direction !direction;
        random#set_ddirection !ddirection;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply
        "Random Sound Parameters"
    (* TODO: this dialog doesn't have a spot for pitch.  hmm. *)

let make_random redraw =
    let random = new MapTypes.random in
    random_dialog random redraw;
    MapFormat.add_random random

let ambient_dialog ambient redraw =
    let index = ref ambient#index in
    let volume = ref (string_of_float ambient#volume) in
    let descriptor = [
        `H [
            `V [`L "Type";
                `L "Volume"; ];
            `V [`M (ItemStrings.sound_strings, index);
                `E volume ] ] ] in
    let apply _ =
        ambient#set_volume (float_of_string !volume);
        ambient#set_index !index;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply
        "Ambient Sound Parameters"

let make_ambient redraw =
    let ambient = new MapTypes.ambient in
    ambient_dialog ambient redraw;
    MapFormat.add_ambient ambient

let goto recenter =
    let kind = ref 2 in
    let id = ref "0" in
    let descriptor = [
        `H [`V [`L "Type";
                `L "ID" ];
            `V [`M (["Point"; "Line"; "Polygon"], kind);
                `E id ] ] ] in
    let apply _ =
        try
            let id = int_of_string !id in
            let center = begin match !kind with
            |0 ->
                if id >= Array.length !MapFormat.points || id < 0 then
                    raise (Failure "Goto point out of bounds!") else (
                let p = !MapFormat.points.(id) in
                DrawModeSettings.highlight := Point [id];
                Some p#vertex )
            |1 ->
                if id >= Array.length !MapFormat.lines || id < 0 then
                    raise (Failure "Goto line out of bounds!") else (
                let l = !MapFormat.lines.(id) in
                DrawModeSettings.highlight := Line [id];
                let p0, p1 = l#endpoints in
                let p0x, p0y = !MapFormat.points.(p0)#vertex in
                let p1x, p1y = !MapFormat.points.(p1)#vertex in
                Some ((p0x +. p1x) /. 2., (p0y +. p1y) /. 2.) )
            |2 ->
                if id >= Array.length !MapFormat.polygons || id < 0 then
                    raise (Failure "Goto polygon out of bounds!") else (
                let p = !MapFormat.polygons.(id) in
                DrawModeSettings.highlight := Poly [id];
                Some (GeomEdit.point_center
                    (Array.sub p#endpoint_indices 0 p#vertex_count)) )
            |_ -> raise (Failure "Invalid Goto kind!") end in
            match center with
            |Some (px, py) -> recenter (px, py)
            |None -> ()
        with |_ -> () in
    GenerateDialog.generate_dialog descriptor apply "Goto"

let map_height_dlg redraw =
    let w = GWindow.window ~title:"Height Window" ~show:true
                           ~allow_shrink:false () in
    let hbox = GPack.hbox ~packing:w#add ~spacing:2 () in
    let vbox1 = GPack.vbox ~packing:hbox#add ~spacing:2 () in
    let adjf = GData.adjustment ~value:(!DrawModeSettings.floor_cutoff)
                                ~lower:(-9.0) ~upper:(9.0 +. 0.01)
                                ~step_incr:0.01 ~page_size:0.01
                                ~page_incr:(18.0) () in
    let alignf = GBin.alignment ~height:300 ~packing:vbox1#add () in
    GRange.scale `VERTICAL ~adjustment:adjf ~value_pos:`BOTTOM ~digits:2
                 ~packing:alignf#add ~inverted:true ();
    GMisc.label ~text:"Floor" ~xpad:2 ~packing:vbox1#add ~justify:`FILL ();
    adjf#connect#value_changed ~callback:(fun _ ->
        DrawModeSettings.floor_cutoff := adjf#value;
        redraw (); ());
    let vbox2 = GPack.vbox ~packing:hbox#add ~spacing:2 () in
    let adjc = GData.adjustment ~value:!DrawModeSettings.ceiling_cutoff
                                ~lower:(-9.0) ~upper:(9.0 +. 0.01)
                                ~step_incr:0.01 ~page_size:0.01
                                ~page_incr:(18.0) () in
    let alignc = GBin.alignment ~height:300 ~packing:vbox2#add () in
    GRange.scale `VERTICAL ~adjustment:adjc ~value_pos:`BOTTOM ~digits:2
                 ~packing:alignc#add ~inverted:true ();
    GMisc.label ~text:"Ceiling" ~xpad:2 ~packing:vbox2#add ~justify:`FILL ();
    adjf#connect#value_changed ~callback:(fun _ ->
        DrawModeSettings.ceiling_cutoff := adjc#value;
        redraw (); ())

let color_prefs_dialog redraw =
    let thickness = ref (string_of_int
                        !DrawModeSettings.highlighted_point_thickness) in
    let looseness = ref (string_of_float !DrawModeSettings.pixel_epsilon) in
    let saturation = ref (string_of_float !Colors.poly_type_saturation) in
    let value = ref (string_of_float !Colors.poly_type_value) in
    let descriptor = [
        `V [
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
                    `O Colors.highlight_color ] ];
            `H [
                `V [
                    `L "Highlight thickness";
                    `L "Click looseness";
                    `L "Polygon Type Saturation";
                    `L "Polygon Type Value";
                ];
                `V [
                    `E thickness;
                    `E looseness;
                    `E saturation;
                    `E value ] ] ] ]; in
    let apply _ =
        DrawModeSettings.highlighted_point_thickness :=
            int_of_string !thickness;
        DrawModeSettings.pixel_epsilon := float_of_string !looseness;
        Colors.poly_type_saturation := float_of_string !saturation;
        Colors.poly_type_value := float_of_string !value;
        redraw () in
    GenerateDialog.generate_dialog descriptor apply "Color Preferences"

let plac_chunk_dialog what strings_list plac_list =
    let columns = new GTree.column_list in
    let obj_type    = columns#add Gobject.Data.string in
    let init_count  = columns#add Gobject.Data.int in
    let min_count   = columns#add Gobject.Data.int in
    let max_count   = columns#add Gobject.Data.int in
    let total_avail = columns#add Gobject.Data.int in
    let appearance  = columns#add Gobject.Data.float in
    let inf_avail   = columns#add Gobject.Data.boolean in
    let random_loc  = columns#add Gobject.Data.boolean in
    let ls = GTree.list_store columns in
    List.iter2 (fun str plac -> let row = ls#append () in
                          ls#set ~row ~column:obj_type    str;
                          ls#set ~row ~column:init_count  plac#initial_count;
                          ls#set ~row ~column:min_count   plac#minimum_count;
                          ls#set ~row ~column:max_count   plac#maximum_count;
                          ls#set ~row ~column:total_avail plac#random_count;
                          ls#set ~row ~column:appearance  plac#random_chance;
                          ls#set ~row ~column:inf_avail   (plac#random_count = -1);
                          ls#set ~row ~column:random_loc  plac#flags)
              strings_list plac_list;
    let dlg = GWindow.dialog ~title:(what^" Parameters") ~border_width:2
                             ~resizable:false () in
    let scroll = GBin.scrolled_window ~packing:dlg#vbox#add
                                      ~height:200 ~hpolicy:`NEVER () in
    let view = GTree.view ~model:ls ~packing:scroll#add
                          ~rules_hint:true () in

    let col = GTree.view_column ~title:(what^" Name")
        ~renderer:(GTree.cell_renderer_text [],
                                            ["text", obj_type]) () in
    view#append_column col;

    let install_gen_column title column convertor =
        let renderer = GTree.cell_renderer_text [`EDITABLE true] in
        let vcol = GTree.view_column ~title
            ~renderer:(renderer, ["text", column]) () in
        renderer#connect#edited (fun loc str ->
            ls#set ~row:(ls#get_iter loc) ~column (convertor str));
        view#append_column vcol in
    let install_int_column title column =
        install_gen_column title column int_of_string in
    let install_float_column title column =
        install_gen_column title column float_of_string in
    let install_bool_column title column =
        let renderer = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
        let vcol = GTree.view_column ~title
            ~renderer:(renderer, ["active", column]) () in
        renderer#connect#toggled (fun loc ->
            let old_value = ls#get ~row:(ls#get_iter loc) ~column in
            ls#set ~row:(ls#get_iter loc) ~column (not old_value));
        view#append_column vcol in
    install_int_column   "Initial Count"      init_count;
    install_int_column   "Minimum"            min_count;
    install_int_column   "Maximum"            max_count;
    install_int_column   "Total Available"    total_avail;
    install_float_column "Appearance"         appearance;
    install_bool_column  "Infinite Available" inf_avail;
    install_bool_column  "Random Location"    random_loc;

    dlg#add_button_stock `APPLY `APPLY;
    dlg#add_button_stock `CANCEL `CANCEL;
    dlg#add_button_stock `OK `OK;
    dlg#set_default_response `OK;
    let apply _ =
        ls#foreach (fun path row ->
            let row_index = (GTree.Path.get_indices path).(0) in
            let plac = List.nth plac_list row_index in
            plac#set_initial_count (ls#get ~row ~column:init_count);
            plac#set_minimum_count (ls#get ~row ~column:min_count);
            plac#set_maximum_count (ls#get ~row ~column:max_count);
            plac#set_random_count (ls#get ~row ~column:total_avail);
            plac#set_random_chance (ls#get ~row ~column:appearance);
            if (ls#get ~row ~column:inf_avail) then
                plac#set_random_count (-1);
            plac#set_flags (ls#get ~row ~column:random_loc);
            false) in
    let rec run _ =
        match dlg#run () with
        |`OK -> apply ()
        |`APPLY -> apply (); run ()
        |_ -> () in
    run ();
    dlg#destroy ()

let item_parameters_dialog _ =
    plac_chunk_dialog "Item" ItemStrings.item_strings
        (Array.to_list (Array.sub !MapFormat.placements 0
                                    (List.length ItemStrings.item_strings)))

let monster_parameters_dialog _ =
    plac_chunk_dialog "Monster" ItemStrings.monster_strings
        (Array.to_list (Array.sub !MapFormat.placements 63
                                    (List.length ItemStrings.monster_strings)))
