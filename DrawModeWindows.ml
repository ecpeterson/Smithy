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
(* set up the drawing window, try not to pollute the namespace *)
let menu_bar, orthodrawer, vadj, hadj, status =
    let vbox = GPack.vbox ~packing:drawmode_window#add () in
    (* this menu will be controlled in Smithy.ml *)
    let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in
    let hbox = GPack.hbox ~packing:vbox#add () in
    let orthodrawer = new OrthoDrawer.orthoDrawer hbox#add in
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

(* menus, to be passed to GToolkit. *)
let file_menu_toolkit =
    [`I ("_New Level...", CamlExt.id);
     `I ("_Open...", CamlExt.id);
     `I ("_Save Level", CamlExt.id);
     `I ("Save Level _As...", CamlExt.id);
     `S;
     `I ("_Merge Levels...", CamlExt.id);
     `I ("_Export Level...", CamlExt.id);
     `S;
     `I ("_Quit", GMain.Main.quit)]

let view_menu_toolkit =
    [`I ("_Draw Mode", CamlExt.id);
     `I ("_Visual Mode", CamlExt.id);
     `S;
     `M ("_Elevation", [`I ("Floor", CamlExt.id);
                        `I ("Ceiling", CamlExt.id)]);
     `M ("_Textures",  [`I ("Floor", CamlExt.id); `I ("Ceiling", CamlExt.id);]);
     `I ("_Polygon Types", CamlExt.id);
     `S;
     `M ("_Lights",    [`I ("Floor", CamlExt.id);
                        `I ("Ceiling", CamlExt.id);
                        `I ("Liquids", CamlExt.id)]);
     `I ("Li_quids", CamlExt.id);
     `M ("_Sounds",    [`I ("Ambient Sounds", CamlExt.id);
                       `I ("Random Sounds", CamlExt.id)])]

let special_menu_toolkit =
    [`I ("Zoom In", CamlExt.id);
     `I ("Zoom Out", CamlExt.id);
     `S;
     `I ("Map M_anager", CamlExt.id);
     `I ("View _Height Window", CamlExt.id);
     `I ("_Goto...", CamlExt.id);
     `S;
     `I ("Set _Level Parameters...", CamlExt.id);
     `I ("Set _Item Parameters...", CamlExt.id);
     `I ("Set _Monster Parameters...", CamlExt.id);
     `I ("Edit Map Item _Parameters...", CamlExt.id);
     `S;
     `I ("_Recenter Level", CamlExt.id);
     `I ("Pave Level", CamlExt.id);
     `I ("Nuke Objects Only...", CamlExt.id);
     `I ("Nuke and Pave Level...", CamlExt.id)]

let smithy_menu_toolkit =
    (* TODO: again, does merge_points really need access to orthodrawer? *)
    [`I ("Merge Selected Points", CamlExt.id);
     `I ("Garbage Collect", Gc.full_major)]

(* set up the application menus *)
let _ =
    let create_menu label menubar =
        let item = GMenu.menu_item ~label ~packing:menubar#append () in
        GMenu.menu ~packing:item#set_submenu () in
    let file_menu    = create_menu "File"    menu_bar in
    let view_menu    = create_menu "View"    menu_bar in
    let special_menu = create_menu "Special" menu_bar in
    let smithy_menu  = create_menu "Smithy"  menu_bar in
    GToolbox.build_menu file_menu    ~entries:file_menu_toolkit;
    GToolbox.build_menu view_menu    ~entries:view_menu_toolkit;
    GToolbox.build_menu special_menu ~entries:special_menu_toolkit;
    GToolbox.build_menu smithy_menu  ~entries:smithy_menu_toolkit

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

let toolbar_clicked which _ =
    List.iter (fun x -> x#set_active false) buttons;
    false
let _ =
    List.iter (fun obj ->
            obj#event#connect#button_press ~callback:(toolbar_clicked obj)
        |> ignore) buttons

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
