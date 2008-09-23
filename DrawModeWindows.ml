(*** DrawModeWindows.ml contains the definitions of the actual GTK interface
 * and hooks used while in draw mode. ***)
open DrawModeSettings

let menu_xml =
"<ui>\
    <menubar name='MenuBar'>\
        <menu action='FileMenu'>\
        <menuitem action='New'/>\
        <menuitem action='Open'/>\
        <menuitem action='Save'/>\
        <menuitem action='SaveAs'/>\
        <separator/>\
        <menuitem action='MergeLevels'/>\
        <menuitem action='ExportLevel'/>\
        <separator/>\
        <menuitem action='Quit'/>\
        </menu>\
        <menu action='ViewMenu'>\
        <menuitem action='DrawMode'/>\
        <menuitem action='VisualMode'/>\
        <separator/>\
        <menu action='ElevationMenu'>\
            <menuitem action='ElevationFloor'/>\
            <menuitem action='ElevationCeiling'/>\
        </menu>\
        <menu action='TextureMenu'>\
            <menuitem action='TextureFloor'/>\
            <menuitem action='TextureCeiling'/>\
        </menu>\
        <menuitem action='PolygonTypes'/>\
        <separator/>\
        <menu action='LightsMenu'>\
            <menuitem action='LightsFloor'/>\
            <menuitem action='LightsCeiling'/>\
            <menuitem action='LightsLiquid'/>\
        </menu>\
        <menuitem action='Liquids'/>\
        <menu action='SoundsMenu'>\
            <menuitem action='SoundsAmbient'/>\
            <menuitem action='SoundsRandom'/>\
        </menu>\
        </menu>\
        <menu action='SpecialMenu'>\
        <menuitem action='ZoomIn'/>\
        <menuitem action='ZoomOut'/>\
        <separator/>\
        <menuitem action='MapManager'/>\
        <menuitem action='ViewHeightWindow'/>\
        <menuitem action='Goto'/>\
        <separator/>\
        <menuitem action='SetLevelParams'/>\
        <menuitem action='SetItemParams'/>\
        <menuitem action='SetMonsterParams'/>\
        <menuitem action='EditMapItemParams'/>\
        <separator/>\
        <menuitem action='RecenterLevel'/>\
        <menuitem action='Pave'/>\
        <menuitem action='Nuke'/>\
        <menuitem action='NukeAndPave'/>\
        </menu>\
        <menu action='SmithyMenu'>\
        <menu action='LightLibMenu'>\
            <menuitem action='AppendLightLib'/>\
            <menuitem action='ReplaceLightLib'/>\
            <menuitem action='SaveLightLib'/>\
        </menu>\
        <menuitem action='MergePoints'/>\
        <menuitem action='GarbageCollect'/>\
        <menuitem action='ColorPreferences'/>\
        <menuitem action='BitchAboutFeatures'/>\
        </menu>\
    </menubar>\
</ui>"
let accel_xml =
"<ui>\
    <accelerator action='LineTool'/>\
    <accelerator action='ArrowTool'/>\
    <accelerator action='FillTool'/>\
    <accelerator action='PolyTool'/>\
    <accelerator action='ZoomTool'/>\
    <accelerator action='PanTool'/>\
    <accelerator action='ObjTool'/>\
    <accelerator action='TextTool'/>\
    <accelerator action='Delete'/>\
    <accelerator action='Backspace'/>\
    <accelerator action='Grid_1_8'/>\
    <accelerator action='Grid_1_4'/>\
    <accelerator action='Grid_1_2'/>\
    <accelerator action='Grid_1'/>\
    <accelerator action='Grid_2'/>\
</ui>"

class drawModeWindow ~width ~height ~title ~show _ =
object (self)
    (* widgets *)
    val mutable window      = Obj.magic ()
    val mutable orthodrawer = Obj.magic ()
    val mutable toolbar     = Obj.magic ()
    val mutable status      = Obj.magic ()

    (* actual data *)

    (* accessors *)
    method window = window
    method orthodrawer = orthodrawer
    method toolbar = toolbar
    method set_title title =
        window#set_title title
    method set_status x =
        status#pop ();
        status#push x

    (* constructor *)
    initializer
        window <- GWindow.window ~width ~height ~title ~show
                                 ~allow_shrink:true ();
        let menu_actions = self#initialize_menu_bar window in
        let accel_actions = self#initialize_accelerators in
        orthodrawer <- self#initialize_orthodrawer window;
        let vbox = GPack.vbox ~packing:window#add () in
        let hbox = GPack.hbox ~packing:(vbox#pack ~expand:true) () in
        hbox#pack ~expand:true orthodrawer#widget;
        let sb = GMisc.statusbar ~packing:vbox#pack () in
        status <- sb#new_context ~name:"Status";
        let ui = GAction.ui_manager () in
        ui#insert_action_group menu_actions 0;
        ui#insert_action_group accel_actions 1;
        window#add_accel_group ui#get_accel_group;
        ui#add_ui_from_string menu_xml;
        ui#add_ui_from_string accel_xml;
        let menu_bar = ui#get_widget "/MenuBar" in
        vbox#pack menu_bar;
        vbox#reorder_child menu_bar 0;
        toolbar <- new Toolbar.toolbar window "Smithy Toolbox" true ();
        ()

    (* private methods *)
    method initialize_menu_bar window =
        let a = GAction.add_action in
        let rg = GAction.group_radio_actions in
        let r = GAction.add_radio_action in
        let menu_actions = GAction.action_group ~name:"Smithy-menu" () in
        GAction.add_actions menu_actions [
            a "FileMenu"    ~label:"_File";
            a "ViewMenu"    ~label:"_View";
            a "SpecialMenu" ~label:"_Special";
            a "SmithyMenu"  ~label:"S_mithy";

            a "New"         ~stock:`NEW
                            ~callback:(fun _ ->
                                    FileDialogs.new_map
                                        orthodrawer#draw);
            a "Open"        ~stock:`OPEN
                            ~callback:(fun _ ->
                                FileDialogs.open_map_dialog self#set_title
                                    orthodrawer#draw);
            a "Save"        ~stock:`SAVE
                            ~callback:(FileDialogs.silent_save
                                           self#set_title);
            a "SaveAs"      ~stock:`SAVE_AS
                            ~callback:(FileDialogs.save_map_dialog
                                           self#set_title);
            a "MergeLevels" ~label:"_Merge Levels...";
            a "ExportLevel" ~label:"_Export Level...";
            a "Quit"        ~stock:`QUIT
                            ~callback:(fun _ -> GMain.Main.quit ());

            a "ElevationMenu" ~label:"_Elevation";
            a "TextureMenu"   ~label:"_Textures";
            a "LightsMenu"    ~label:"_Lights";
            a "SoundsMenu"    ~label:"_Sounds";

            rg ~init_value:0 [
                r "DrawMode"         0 ~label:"_Draw Mode"
                                    ~accel:"<Ctrl>d";
                r "VisualMode"       1 ~label:"_Visual Mode"
                                    ~accel:"<Ctrl>1";
                r "PolygonTypes"     2 ~label:"_Polygon Types";
                r "Liquids"          3 ~label:"Li_quids";

                r "ElevationFloor"   4 ~label:"_Floor";
                r "ElevationCeiling" 5 ~label:"_Ceiling";

                r "TextureFloor"     6 ~label:"_Floor";
                r "TextureCeiling"   7 ~label:"_Ceiling";

                r "LightsFloor"      8 ~label:"_Floor";
                r "LightsCeiling"    9 ~label:"_Ceiling";
                r "LightsLiquid"    10 ~label:"_Liquid";

                r "SoundsAmbient"   11 ~label:"_Ambient Sounds";
                r "SoundsRandom"    12 ~label:"_Random Sounds";
            ] ~callback:(fun state ->
                self#toolbar#change_editor_state state;
                orthodrawer#draw ());

            a "Pave"        ~label:"_Pave Level"
                            ~callback:MapFormat.pave;
            a "Nuke"        ~label:"_Nuke Objects Only..."
                            ~callback:MapFormat.nuke;
            a "NukeAndPave" ~label:"N_uke and Pave Level..."
                            ~callback:MapFormat.nuke_and_pave;

            a "ZoomIn"            ~label:"Zoom _In"
                                ~accel:"<Ctrl>equal"
                                ~callback:(fun _ -> orthodrawer#zoom 2.0);
            a "ZoomOut"           ~label:"Zoom _Out"
                                ~accel:"<Ctrl>minus"
                                ~callback:(fun _ -> orthodrawer#zoom 0.5);
            a "MapManager"        ~label:"M_ap Manager"
                                ~callback:(fun _ ->
                                    MapDialogs.map_manager orthodrawer#draw);
            a "ViewHeightWindow"  ~label:"View _Height Window"
                                ~accel:"<Ctrl>h"
                                (* XXX: move the redrawing stuff back to here,
                                 * don't pass orthodrawer into the dialog *)
                                ~callback:(MapDialogs.map_height_dlg
                                               orthodrawer);
            a "Goto"              ~label:"_Goto..."
                                ~callback:(fun _ ->
                                    let Some center = MapDialogs.goto () in
                                    orthodrawer#center_on center);
            a "SetLevelParams"    ~label:"Set _Level Parameters..."
                                ~callback:(fun _ ->
                                    MapDialogs.info_dialog orthodrawer#draw)
                                ~accel:"<Ctrl>m";
            a "SetItemParams"     ~label:"Set _Item Parameters..."
                                ~accel:"<Ctrl>i";
            a "SetMonsterParams"  ~label:"Set _Monster Parameters...";
            a "EditMapItemParams" ~label:"Edit Map Item _Parameters...";
            a "RecenterLevel"     ~label:"_Recenter Level";

            a "MergePoints"      ~label:"_Merge Selected Points"
                                ~callback:(fun _ ->
                                    match !highlight with
                                    |Point ns ->
                                        GeomEdit.merge_points ns ();
                                        highlight := No_Highlight;
                                        orthodrawer#draw ()
                                    |_ -> ());
            a "GarbageCollect"   ~label:"_Garbage Collect"
                                ~callback:(fun _ -> Gc.full_major ());
            a "ColorPreferences" ~label:"Color _Preferences"
                                ~callback:(fun _ ->
                                    MapDialogs.color_prefs_dialog
                                        orthodrawer#draw);
            a "LightLibMenu"     ~label:"_Light Libraries";
            a "AppendLightLib"   ~label:"Load and _Append Light Library"
                                ~callback:(fun _ ->
                                    FileDialogs.load_and_append_light_lib
                                        orthodrawer#draw);
            a "ReplaceLightLib" ~label:"Load and _Replace Light Library"
                                ~callback:(fun _ ->
                                    FileDialogs.load_and_replace_light_lib
                                        orthodrawer#draw);
            a "SaveLightLib"     ~label:"Save Light Library"
                                ~callback:FileDialogs.save_light_lib;
            a "BitchAboutFeatures" ~label:"Bitch about Features"
                ~callback:(fun _ ->
                    let dialog = GWindow.message_dialog
                        ~message:"-v" ~message_type:`ERROR
                        ~buttons:GWindow.Buttons.close ~modal:true
                        ~title:Resources.warning () in
                    dialog#run (); dialog#destroy ())
        ];
        menu_actions

    method initialize_orthodrawer window =
        new OrthoDrawer.orthoDrawer
            ~xmin:(0.0 -. float MapFormat.half_map_width)
            ~xmax:(float MapFormat.half_map_width)
            ~ymin:(0.0 -. float MapFormat.half_map_width)
            ~ymax:(float MapFormat.half_map_width) ()

    method initialize_accelerators =
        let tool_cb tool _ =
            let button = toolbar#button_of_tool tool in
            toolbar#clicked tool;
            button#clicked ();
            () in
        let delete_cb _ =
            (* dispatch for deleting a highlighted map item *)
            begin match !highlight with
            |Point  (n :: ns) ->
                List.iter MapFormat.delete_point (n :: ns);
                highlight := (if n > 0 then Point  [n] else No_Highlight)
            |Line   (n :: ns) ->
                List.iter MapFormat.delete_line  (n :: ns);
                highlight := (if n > 0 then Line   [n] else No_Highlight)
            |Poly   (n :: ns) ->
                List.iter MapFormat.delete_poly  (n :: ns);
                highlight := (if n > 0 then Poly   [n] else No_Highlight)
            |Object (n :: ns) ->
                List.iter MapFormat.delete_obj   (n :: ns);
                highlight := (if n > 0 then Object [n] else No_Highlight)
            |Annotation ns ->
                List.iter MapFormat.delete_annotation ns;
                highlight := No_Highlight
            |No_Highlight |_ -> () end;
            orthodrawer#draw (); () in
        let grid_cb factor _ =
            grid_factor := factor;
            orthodrawer#draw () in
        let accel_actions = GAction.action_group ~name:"Smithy-accels" () in
        let a = GAction.add_action in
        GAction.add_actions accel_actions [
            a "LineTool"  ~accel:"l" ~callback:(tool_cb LineTool);
            a "ArrowTool" ~accel:"a" ~callback:(tool_cb ArrowTool);
            a "FillTool"  ~accel:"f" ~callback:(tool_cb FillTool);
            a "PolyTool"  ~accel:"p" ~callback:(tool_cb PolyTool);
            a "ZoomTool"  ~accel:"z" ~callback:(tool_cb ZoomTool);
            a "PanTool"   ~accel:"h" ~callback:(tool_cb PanTool);
            a "ObjTool"   ~accel:"o" ~callback:(tool_cb ObjTool);
            a "TextTool"  ~accel:"t" ~callback:(tool_cb TextTool);

            a "Delete"    ~accel:"Delete"    ~callback:delete_cb;
            a "Backspace" ~accel:"BackSpace" ~callback:delete_cb;

            a "Grid_1_8"  ~accel:"1" ~callback:(grid_cb 4);
            a "Grid_1_4"  ~accel:"2" ~callback:(grid_cb 3);
            a "Grid_1_2"  ~accel:"3" ~callback:(grid_cb 2);
            a "Grid_1"    ~accel:"4" ~callback:(grid_cb 1);
            a "Grid_2"    ~accel:"5" ~callback:(grid_cb 0);
        ];
        accel_actions

    (* other public methods *)
end
