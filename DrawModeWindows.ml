open CamlExt

(* we keep track of what item is highlighted as part of the drawing / interface
 * object, and we have an enumerative type to match across *)
type highlighted_component =  No_Highlight       |
                              Point of int list  |
                              Line of int list   |
                              Poly of int list   |
                              Object of int list
(* some stateful data *)
let highlight = ref No_Highlight

(* the drawing object has a bunch of different states that it can be in that
 * affects what information is displayed, here we have an enumerative type that
 * describes the modes *)
type renderer_mode = Draw | Floor_Height | Ceiling_Height | Media |
                     Floor_Light | Ceiling_Light | Media_Light | Polygon_Type
(* some more stateful data *)
let mode = ref Draw

(* set up the drawing window *)
let drawmode_window = GWindow.window ~width:500 ~height:300 ~title:"Smithy"
    ~allow_shrink:true ~show:true ()

let draw_toolbar = GWindow.window ~title:"Smithy Toolkit" ~show:true
                                  ~height:120 ~width:60 ()
let buttonarrow, buttonline, buttonpoly, buttonfill,
    buttonpan, buttonzoom, buttontext, buttonobj =
        let vbox = GPack.vbox ~packing:draw_toolbar#add () in
        let hbox1 = GPack.hbox ~packing:vbox#pack () in
        let hbox2 = GPack.hbox ~packing:vbox#pack () in
        let hbox3 = GPack.hbox ~packing:vbox#pack () in
        let hbox4 = GPack.hbox ~packing:vbox#pack () in
        GButton.toggle_button ~packing:hbox1#pack ~active:true (),
        GButton.toggle_button ~packing:hbox1#pack (),
        GButton.toggle_button ~packing:hbox2#pack (),
        GButton.toggle_button ~packing:hbox2#pack (),
        GButton.toggle_button ~packing:hbox3#pack (),
        GButton.toggle_button ~packing:hbox3#pack (),
        GButton.toggle_button ~packing:hbox4#pack (),
        GButton.toggle_button ~packing:hbox4#pack ()
let _ = GMisc.pixmap (GDraw.pixmap_from_xpm Resources.arrowfile ())
                     ~packing:buttonarrow#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.linefile ())
                     ~packing:buttonline#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.polyfile ())
                     ~packing:buttonpoly#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.fillfile ())
                     ~packing:buttonfill#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.panfile ())
                     ~packing:buttonpan#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.zoomfile ())
                     ~packing:buttonzoom#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.textfile ())
                     ~packing:buttontext#add () |> ignore;
        GMisc.pixmap (GDraw.pixmap_from_xpm Resources.objfile ())
                     ~packing:buttonobj#add () |> ignore
let buttons = [buttonarrow; buttonline; buttonpoly; buttonfill;
               buttonpan; buttonzoom; buttontext; buttonobj]

let active_tool () =
    let rec a_t_aux lst =
        if (List.hd lst)#active
            then List.hd lst
            else a_t_aux (List.tl lst) in
    a_t_aux buttons

let toolbar_clicked _ =
    List.iter (fun x -> x#set_active false) buttons;
    false
let _ =
    List.iter (fun obj ->
            obj#event#connect#button_press ~callback:toolbar_clicked
        |> ignore) buttons

(* set up the drawing window, try not to pollute the namespace *)
let menu_bar, orthodrawer, vadj, hadj, status =
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
          <menuitem action='MergePoints'/>\
          <menuitem action='GarbageCollect'/>\
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

        a "New"         ~stock:`NEW;
        a "Open"        ~stock:`OPEN;
        a "Save"        ~stock:`SAVE;
        a "SaveAs"      ~stock:`SAVE_AS;
        a "MergeLevels" ~label:"_Merge Levels...";
        a "ExportLevel" ~label:"_Export Level...";
        a "Quit"        ~stock:`QUIT
                        ~callback:(fun _ -> GMain.Main.quit ());

        a "ElevationMenu" ~label:"_Elevation";
        a "TextureMenu"   ~label:"_Textures";
        a "LightsMenu"    ~label:"_Lights";
        a "SoundsMenu"    ~label:"_Sounds";

        rg ~init_value:0 [
            r "DrawMode"         0 ~label:"_Draw Mode";
            r "VisualMode"       1 ~label:"_Visual Mode";
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
        ];

        a "Pave"        ~label:"_Pave Level";
        a "Nuke"        ~label:"_Nuke Objects Only...";
        a "NukeAndPave" ~label:"N_uke and Pave Level...";

        a "ZoomIn"            ~label:"Zoom _In"
                              ~accel:"<Ctrl>equal";
        a "ZoomOut"           ~label:"Zoom _Out"
                              ~accel:"<Ctrl>minus";
        a "MapManager"        ~label:"M_ap Manager";
        a "ViewHeightWindow"  ~label:"View _Height Window";
        a "Goto"              ~label:"_Goto...";
        a "SetLevelParams"    ~label:"Set _Level Parameters...";
        a "SetItemParams"     ~label:"Set _Item Parameters...";
        a "SetMonsterParams"  ~label:"Set _Monster Parameters...";
        a "EditMapItemParams" ~label:"Edit Map Item _Parameters...";
        a "RecenterLevel"     ~label:"_Recenter Level";

        a "MergePoints"    ~label:"_Merge Selected Points";
        a "GarbageCollect" ~label:"_Garbage Collect"
                           ~callback:(fun _ -> Gc.full_major ());
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
    </ui>" in
    let tool_cb button _ =
        toolbar_clicked ();
        button#clicked ();
        () in
    let vbox = GPack.vbox ~packing:drawmode_window#add () in
    let hbox = GPack.hbox ~packing:(vbox#pack ~expand:true) () in
    let orthodrawer = new OrthoDrawer.orthoDrawer (hbox#pack ~expand:true) in
    let delete_cb _ =
        (* dispatch for deleting a highlighted map item *)
        begin match !highlight with
        |Point n -> List.iter (fun n -> MapFormat.delete_point n) n
        |Line  n -> List.iter (fun n -> MapFormat.delete_line n)  n
        |Poly  n -> List.iter (fun n -> MapFormat.delete_poly n)  n
        |No_Highlight |_ -> ()
        end;
        orthodrawer#draw (); () in
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
    let vadj = GData.adjustment ~value:0.0
                                ~lower:(0.0 -. float MapFormat.half_map_width)
                                ~upper:(float MapFormat.half_map_width)
                                ~step_incr:16.0 () in
    let hadj = GData.adjustment ~value:0.0
                                ~lower:(0.0 -. float MapFormat.half_map_width)
                                ~upper:(float MapFormat.half_map_width)
                                ~step_incr:16.0 () in
    let vscroll = GRange.scrollbar `VERTICAL ~packing:hbox#pack
                                             ~adjustment:vadj () in
    let hscroll = GRange.scrollbar `HORIZONTAL ~packing:vbox#pack
                                               ~adjustment:hadj () in
    let sb = GMisc.statusbar ~packing:vbox#pack () in
    let sbc = sb#new_context ~name:"Status" in
    menu_bar, orthodrawer, vadj, hadj, sbc
let set_status x = status#pop (); status#push x

(* and the alternative toolbar *)
let entry_toolbar = GWindow.window ~type_hint:`TOOLBAR ~title:"Smithy"
                                   ~show:false ()
let vbox = GPack.vbox ~packing:entry_toolbar#add ()
let hbox = GPack.hbox ~packing:vbox#add ()
let entry_label = GMisc.label ~text:"Height: " ~packing:hbox#add ()
let numeric_entry = GEdit.entry ~packing:hbox#add ()
let mediabox = GPack.hbox ~packing:vbox#add ()
let editbutton = GButton.button ~label:"Edit Media..." ~packing:mediabox#add ()
let newbutton = GButton.button ~label:"New Media..." ~packing:mediabox#add ()
