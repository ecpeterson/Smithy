(*** DrawModeWindows.ml contains the definitions of the actual GTK interface
 * and hooks used while in draw mode. ***)
open CamlExt
open DrawModeSettings

let color_prefs_dialog drawer _ =
    let thickness = ref (string_of_int
                        !DrawModeSettings.highlighted_point_thickness) in
    let looseness = ref (string_of_float !DrawModeSettings.pixel_epsilon) in
    let saturation = ref (string_of_float !Colors.poly_type_saturation) in
    let value = ref (string_of_float !Colors.poly_type_value) in
    let descriptor = [
        `V [
            `H [`L "Background color: ";
                `O Colors.background_color ];
            `H [`L "Grid color: ";
                `O Colors.grid_color ];
            `H [`L "Anchor point color: ";
                `O Colors.anchor_point_color ];
            `H [`L "Point color: ";
                `O Colors.point_color ];
            `H [`L "Solid line color: ";
                `O Colors.solid_line_color ];
            `H [`L "Transparent line color: ";
                `O Colors.transparent_line_color ];
            `H [`L "Passable line color: ";
                `O Colors.passable_line_color ];
            `H [`L "Polygon color: ";
                `O Colors.polygon_color ];
            `H [`L "Invalid polygon color: ";
                `O Colors.invalid_polygon ];
            `H [`L "Highlight color: ";
                `O Colors.highlight_color ];
            `H [`L "Highlight thickness: ";
                `E thickness ];
            `H [`L "Click looseness: ";
                `E looseness ];
            `H [`L "Polygon Type Saturation: ";
                `E saturation ];
            `H [`L "Polygon Type Value: ";
                `E value ] ] ] in
    GenerateDialog.generate_dialog descriptor "Color Preferences";
    DrawModeSettings.highlighted_point_thickness := int_of_string !thickness;
    DrawModeSettings.pixel_epsilon := float_of_string !looseness;
    Colors.poly_type_saturation := float_of_string !saturation;
    Colors.poly_type_value := float_of_string !value;
    drawer#draw ()

(* set up the drawing window *)
let drawmode_window = GWindow.window ~width:500 ~height:300 ~title:"Smithy"
    ~allow_shrink:true ~show:true ()
let set_title = drawmode_window#set_title

let draw_toolbar = GWindow.window ~title:"Smithy Toolkit" ~show:true
                                  ~height:120 ~width:60 ()
let _ = draw_toolbar#set_transient_for drawmode_window#as_window
let buttonline, buttonarrow, buttonfill, buttonpoly,
    buttonzoom, buttonpan, buttonobj, buttontext =
        let vbox = GPack.vbox ~packing:draw_toolbar#add () in
        let hbox1 = GPack.hbox ~packing:vbox#pack () in
        let hbox2 = GPack.hbox ~packing:vbox#pack () in
        let hbox3 = GPack.hbox ~packing:vbox#pack () in
        let hbox4 = GPack.hbox ~packing:vbox#pack () in
        GButton.toggle_button ~packing:hbox1#pack (),
        GButton.toggle_button ~packing:hbox1#pack ~active:true (),
        GButton.toggle_button ~packing:hbox2#pack (),
        GButton.toggle_button ~packing:hbox2#pack (),
        GButton.toggle_button ~packing:hbox3#pack (),
        GButton.toggle_button ~packing:hbox3#pack (),
        GButton.toggle_button ~packing:hbox4#pack (),
        GButton.toggle_button ~packing:hbox4#pack ()
let _ =
    GMisc.image ~file:Resources.arrowfile ~packing:buttonarrow#add ();
    GMisc.image ~file:Resources.linefile  ~packing:buttonline#add  ();
    GMisc.image ~file:Resources.polyfile  ~packing:buttonpoly#add  ();
    GMisc.image ~file:Resources.fillfile  ~packing:buttonfill#add  ();
    GMisc.image ~file:Resources.panfile   ~packing:buttonpan#add   ();
    GMisc.image ~file:Resources.zoomfile  ~packing:buttonzoom#add  ();
    GMisc.image ~file:Resources.textfile  ~packing:buttontext#add  ();
    GMisc.image ~file:Resources.objfile   ~packing:buttonobj#add   ()
let buttons = [(buttonarrow, ArrowTool);
               (buttonline,  LineTool);
               (buttonpoly,  PolyTool);
               (buttonfill,  FillTool);
               (buttonpan,   PanTool);
               (buttonzoom,  ZoomTool);
               (buttontext,  TextTool);
               (buttonobj,   ObjTool)]

let toolbar_clicked tool =
    List.iter (fun x -> (fst x)#set_active false) buttons;
    active_tool := tool;
    false
let _ =
    List.iter (fun obj -> match obj with (button, tool) ->
            button#event#connect#button_press ~callback:(fun _ ->
                toolbar_clicked tool)
        |> ignore) buttons

(* and the alternative toolbar *)
let entry_toolbar, entry_label, numeric_entry, mediabox, newbutton, editbutton,
    ptype_cb =
    let entry_toolbar = GWindow.window ~title:"Smithy" ~show:false () in
    entry_toolbar#set_transient_for drawmode_window#as_window;
    let vbox = GPack.vbox ~packing:entry_toolbar#add () in
    let hbox = GPack.hbox ~packing:vbox#add () in
    let entry_label = GMisc.label ~text:"Height: " ~packing:hbox#add () in
    let numeric_entry = GEdit.entry ~packing:hbox#add () in
    let mediabox = GPack.hbox ~packing:vbox#add () in
    let editbutton = GButton.button ~label:"Edit Media..."
                                    ~packing:mediabox#add () in
    let newbutton  = GButton.button ~label:"New Media..."
                                    ~packing:mediabox#add () in
    let cb, _ = GEdit.combo_box_text ~packing:vbox#add
                                     ~strings:ItemStrings.polygon_types () in
    cb#set_active 0;
    (entry_toolbar, entry_label, numeric_entry, mediabox, newbutton,
     editbutton, cb)
let _ =
    ignore (draw_toolbar#event#connect#delete ~callback:(fun _ -> true));
    ignore (entry_toolbar#event#connect#delete ~callback:(fun _ -> true))

(* the various editor mode types *)
type modes = Draw_Mode | Visual_Mode | Elevation_Floor | Elevation_Ceiling |
             Textures_Floor | Textures_Ceiling | Polygon_Types | Lights_Floor |
             Lights_Ceiling | Lights_Liquid | Liquids | Sounds_Ambient |
             Sounds_Random
let mode_descriptor = 0, [Draw_Mode; Visual_Mode; Polygon_Types;
    Liquids; Elevation_Floor; Elevation_Ceiling; Textures_Floor;
    Textures_Ceiling; Lights_Floor; Lights_Ceiling; Lights_Liquid;
    Sounds_Ambient; Sounds_Random]
let mode = ref Draw_Mode
let change_editor_state state =
    (* when we change between renderer modes, the GTK toolkits have to be 
     * modified and hidden/shown appropriately.  change_mode is an abstraction 
     * of this process, and the to_*_mode functions contain data to pass to 
     * change_mode *)
    let set_mode box entry_window buttons button_text1 button_text2 label_text
                 menu entry =
        if box then draw_toolbar#show ()
            else draw_toolbar#misc#hide ();
        if entry_window then entry_toolbar#show ()
            else entry_toolbar#misc#hide ();
        if buttons then mediabox#misc#show ()
            else mediabox#misc#hide ();
        if menu then ptype_cb#misc#show () else ptype_cb#misc#hide ();
        if entry then numeric_entry#misc#show ()
            else numeric_entry#misc#hide ();
        entry_label#set_text label_text;
        newbutton#set_label button_text1;
        editbutton#set_label button_text2 in
    mode := of_enum mode_descriptor state;
    match !mode with
    |Polygon_Types -> set_mode false true false "" "" "" true false
    |Draw_Mode -> set_mode true false false "" "" "" false false
    |Elevation_Floor
    |Elevation_Ceiling -> set_mode false true false "" "" "Height:" false true
    |Lights_Liquid
    |Lights_Floor
    |Lights_Ceiling ->
        set_mode false true true "New Light..." "Edit Light..." "Light:"
                 false true
    |Liquids ->
        set_mode false true true "New Media..." "Edit Media..." "Media:"
                 false true
    |Sounds_Random
    |Sounds_Ambient ->
        set_mode false true true "New Sound..." "Edit Sound..." "Sound:"
        false true
    |_ -> ()

(* set up the drawing window, try not to pollute the namespace *)
let menu_bar, orthodrawer, status =
    let vbox = GPack.vbox ~packing:drawmode_window#add () in
    let hbox = GPack.hbox ~packing:(vbox#pack ~expand:true) () in
    let orthodrawer = new OrthoDrawer.orthoDrawer
        ~xmin:(0.0 -. float MapFormat.half_map_width)
        ~xmax:(float MapFormat.half_map_width)
        ~ymin:(0.0 -. float MapFormat.half_map_width)
        ~ymax:(float MapFormat.half_map_width)
        ~packing:(hbox#pack ~expand:true) () in
    let sb = GMisc.statusbar ~packing:vbox#pack () in
    let sbc = sb#new_context ~name:"Status" in
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
        </menu>\
      </menubar>\
    </ui>" in
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
                                   MapFormat.reset_structures ();
                                   orthodrawer#draw ());
        a "Open"        ~stock:`OPEN
                        ~callback:(FileDialogs.open_map_dialog set_title
                                                                orthodrawer);
        a "Save"        ~stock:`SAVE
                        ~callback:(FileDialogs.silent_save set_title);
        a "SaveAs"      ~stock:`SAVE_AS
                        ~callback:(FileDialogs.save_map_dialog set_title);
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
            change_editor_state state;
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
                              ~callback:(MapDialogs.map_manager orthodrawer);
        a "ViewHeightWindow"  ~label:"View _Height Window"
                              ~accel:"<Ctrl>h"
                              ~callback:(MapDialogs.map_height_dlg orthodrawer);
        a "Goto"              ~label:"_Goto..."
                              ~callback:(MapDialogs.goto orthodrawer);
        a "SetLevelParams"    ~label:"Set _Level Parameters..."
                              ~callback:MapDialogs.info_dialog
                              ~accel:"<Ctrl>m";
        a "SetItemParams"     ~label:"Set _Item Parameters..."
                              ~accel:"<Ctrl>i";
        a "SetMonsterParams"  ~label:"Set _Monster Parameters...";
        a "EditMapItemParams" ~label:"Edit Map Item _Parameters...";
        a "RecenterLevel"     ~label:"_Recenter Level";

        a "MergePoints"      ~label:"_Merge Selected Points"
                             ~callback:(fun _ ->
                                 match !highlight with Point ns ->
                                    GeomEdit.merge_points ns ();
                                    highlight := No_Highlight;
                                    orthodrawer#draw ()
                                    |_ -> ());
        a "GarbageCollect"   ~label:"_Garbage Collect"
                             ~callback:(fun _ -> Gc.full_major ());
        a "ColorPreferences" ~label:"Color _Preferences"
                             ~callback:(color_prefs_dialog orthodrawer);
        a "LightLibMenu"     ~label:"_Light Libraries";
        a "AppendLightLib"   ~label:"Load and _Append Light Library"
                             ~callback:(FileDialogs.load_and_append_light_lib
                                                                   orthodrawer);
        a "ReplaceLightLib"  ~label:"Load and _Replace Light Library"
                             ~callback:(FileDialogs.load_and_replace_light_lib
                                                                   orthodrawer);
        a "SaveLightLib"     ~label:"Save Light Library"
                             ~callback:FileDialogs.save_light_lib
    ];
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
    </ui>" in
    let tool_cb button _ =
        toolbar_clicked (List.assoc button buttons);
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
    GAction.add_actions accel_actions [
        a "LineTool"  ~accel:"l" ~callback:(tool_cb buttonline);
        a "ArrowTool" ~accel:"a" ~callback:(tool_cb buttonarrow);
        a "FillTool"  ~accel:"f" ~callback:(tool_cb buttonfill);
        a "PolyTool"  ~accel:"p" ~callback:(tool_cb buttonpoly);
        a "ZoomTool"  ~accel:"z" ~callback:(tool_cb buttonzoom);
        a "PanTool"   ~accel:"h" ~callback:(tool_cb buttonpan);
        a "ObjTool"   ~accel:"o" ~callback:(tool_cb buttonobj);
        a "TextTool"  ~accel:"t" ~callback:(tool_cb buttontext);

        a "Delete"    ~accel:"Delete"    ~callback:delete_cb;
        a "Backspace" ~accel:"BackSpace" ~callback:delete_cb;

        a "Grid_1_8"  ~accel:"1" ~callback:(grid_cb 4);
        a "Grid_1_4"  ~accel:"2" ~callback:(grid_cb 3);
        a "Grid_1_2"  ~accel:"3" ~callback:(grid_cb 2);
        a "Grid_1"    ~accel:"4" ~callback:(grid_cb 1);
        a "Grid_2"    ~accel:"5" ~callback:(grid_cb 0);
    ];
    let ui = GAction.ui_manager () in
    ui#insert_action_group menu_actions 0;
    ui#insert_action_group accel_actions 1;
    drawmode_window#add_accel_group ui#get_accel_group;
    ui#add_ui_from_string menu_xml;
    ui#add_ui_from_string accel_xml;
    let menu_bar = ui#get_widget "/MenuBar" in
    vbox#pack menu_bar;
    vbox#reorder_child menu_bar 0;
    menu_bar, orthodrawer, sbc
let set_status x = status#pop (); status#push x
