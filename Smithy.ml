(* called when we want to quit the application *)
let quit_application _ = GMain.Main.quit ()
(* inserted as a callback, gets called /when/ we quit the application *)
let deleting_window _ = false

(* menus, to be passed to GToolkit.  not really stylistically proper to be
 * defined in this file at this location, but because the pairs have to refer to
 * the helper functions, I can't think of a better place to put them for the
 * moment.  I'll figure it out eventually. *)
let file_menu_toolkit =
    [`I ("_New Level...", CamlExt.id);
     `I ("_Open...", CamlExt.id);
     `I ("_Save Level", CamlExt.id);
     `I ("Save Level _As...", CamlExt.id);
     `S;
     `I ("_Merge Levels...", CamlExt.id);
     `I ("_Export Level...", CamlExt.id);
     `S;
     `I ("_Quit", quit_application)]

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
    let file_menu    = create_menu "File"    DrawModeWindows.menu_bar in
    let view_menu    = create_menu "View"    DrawModeWindows.menu_bar in
    let special_menu = create_menu "Special" DrawModeWindows.menu_bar in
    let smithy_menu  = create_menu "Smithy"  DrawModeWindows.menu_bar in
    GToolbox.build_menu file_menu    ~entries:file_menu_toolkit;
    GToolbox.build_menu view_menu    ~entries:view_menu_toolkit;
    GToolbox.build_menu special_menu ~entries:special_menu_toolkit;
    GToolbox.build_menu smithy_menu  ~entries:smithy_menu_toolkit

(* honest to god entry point for the program *)
let _ =
    let main_window = DrawModeWindows.drawmode_window in
    ignore (main_window#event#connect#delete ~callback:deleting_window);
    ignore (main_window#connect#destroy ~callback:quit_application);
    (* callback hooks for mouse/keyboard events go here *)
    let args = Sys.argv in
    if Array.length args > 1 then
        MapFormat.read_from_file args.(Array.length args - 1);
    GMain.Main.main ()
