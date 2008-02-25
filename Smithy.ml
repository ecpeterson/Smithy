(*** Smithy.ml houses the main Smithy application and contains all of the glue
 * and interface functionality. ***)

(* include things *)
open MapFormat
open Resources
open DrawController

(* a function that kills the application, will eventually check for things like
 * altered documents and request saves *)
let quit_application () =
    GMain.Main.quit ()

(* creates a menu in a menubar *)
let create_menu label menubar =
    let item = GMenu.menu_item ~label ~packing:menubar#append () in
    GMenu.menu ~packing:item#set_submenu ()

(** set up GTK widgets **)
(* the main draw window *)
let window = GWindow.window ~width:500 ~height:300 ~title:"Smithy"
    ~allow_shrink:true ~show:false ()
let vbox = GPack.vbox ~packing:window#add ()
let menu_bar = GMenu.menu_bar ~packing:vbox#pack ~height:20 ()
let file_menu = create_menu "File" menu_bar
let view_menu = create_menu "View" menu_bar
let special_menu = create_menu "Special" menu_bar
let smithy_menu = create_menu "Smithy" menu_bar
let hbox = GPack.hbox ~packing:vbox#add ()
let eventbox = GBin.event_box ~packing:hbox#add ()
let ar = GlGtk.area [`USE_GL;`RGBA;`DOUBLEBUFFER]
                    ~packing:eventbox#add ~show:true ()
let vadj = GData.adjustment ~value:0.0 ~lower:(0.0 -. half_map_width)
                            ~upper:half_map_width ~step_incr:16.0 ()
let hadj = GData.adjustment ~value:0.0 ~lower:(0.0 -. half_map_width)
                            ~upper:half_map_width ~step_incr:16.0 ()
let vscroll = GRange.scrollbar `VERTICAL ~packing:hbox#pack
    ~adjustment:vadj ()
let hscroll = GRange.scrollbar `HORIZONTAL ~packing:vbox#pack
    ~adjustment:hadj ()
let gl = new DrawController.glcontroller ar vscroll hscroll
let sb = GMisc.statusbar ~packing:vbox#pack ()
let sbc = sb#new_context ~name:"Test"
let set_status x = sbc#pop (); sbc#push x

(* and the map object we'll be recycling *)
let map = new map

(* if we need to do any cleanup work when the window/app gets destroyed, this is
 * the place to do it *)
let deleting_window _ =
    false

(* a mnemonic to set the title of the window *)
let set_title x = window#set_title x

(* TODO: this gets called when the user wants a fresh map *)
let new_map () =
    ()

(* when we change between renderer modes, the GTK toolkits have to be modified
 * and hidden/shown appropriately.  change_mode is an abstraction of this
 * process, and the to_*_mode functions contain data to pass to change_mode *)
let change_mode box entry buttons button_text1 button_text2 label_text mode =
    if box then GlFlatDraw.toolbar#show ()
        else GlFlatDraw.toolbar#misc#hide ();
    if entry then GlFlatDraw.entry_toolbar#show ()
        else GlFlatDraw.entry_toolbar#misc#hide ();
    if buttons then GlFlatDraw.mediabox#misc#show ()
        else GlFlatDraw.mediabox#misc#hide ();
    GlFlatDraw.entry_label#set_text label_text;
    GlFlatDraw.newbutton#set_label button_text1;
    GlFlatDraw.editbutton#set_label button_text2;
    gl#set_mode DrawController.Flat_Draw;
    gl#gldrawer#set_mode mode
let to_poly_type_mode () =
    change_mode false false false "" "" "" GlFlatDraw.Polygon_Type
let to_draw_mode () =
    change_mode true false false "" "" "" GlFlatDraw.Draw
let to_floor_height_mode () =
    change_mode false true false "" "" "Height:" GlFlatDraw.Floor_Height
let to_ceiling_height_mode () =
    change_mode false true false "" "" "Height:" GlFlatDraw.Ceiling_Height
let to_media_light_mode () =
    change_mode false true true "New Light..." "Edit Light..." "Light:" GlFlatDraw.Media_Light
let to_floor_light_mode () =
    change_mode false true true "New Light..." "Edit Light..." "Light:" GlFlatDraw.Floor_Light
let to_ceiling_light_mode () =
    change_mode false true true "New Light..." "Edit Light..." "Light:" GlFlatDraw.Ceiling_Light
let to_media_mode () =
    change_mode false true true "New Media..." "Edit Media..." "Media:" GlFlatDraw.Media

(* menus, to be passed to GToolkit.  not really stylistically proper to be
 * defined in this file at this location, but because the pairs have to refer to
 * the helper functions, I can't think of a better place to put them for the
 * moment.  I'll figure it out eventually. *)
let file_menu_toolkit =
    [`I ("_New Level...", new_map);
     `I ("_Open...", FileDialogs.open_file_dialog map set_title gl);
     `I ("_Save Level", FileDialogs.silent_save map set_title);
     `I ("Save Level _As...", FileDialogs.save_file_dialog map set_title);
     `S;
     `I ("_Merge Levels...", CamlExt.id);
     `I ("_Export Level...", CamlExt.id);
     `S;
     `I ("_Quit", quit_application)]

let view_menu_toolkit =
    [`I ("_Draw Mode", to_draw_mode);
     `I ("_Visual Mode", (fun () -> gl#set_mode DrawController.VISUAL_MODE));
     `S;
     `M ("_Elevation", [`I ("Floor", to_floor_height_mode);
                        `I ("Ceiling", to_ceiling_height_mode)]);
     `M ("_Textures",  [`I ("Floor", CamlExt.id); `I ("Ceiling", CamlExt.id);]);
     `I ("_Polygon Types", to_poly_type_mode);
     `S;
     `M ("_Lights",    [`I ("Floor", to_floor_light_mode);
                        `I ("Ceiling", to_ceiling_light_mode);
                        `I ("Liquids", to_media_light_mode)]);
     `I ("Li_quids", to_media_mode);
     `M ("_Sounds",    [`I ("Ambient Sounds", CamlExt.id);
                       `I ("Random Sounds", CamlExt.id)])]

let special_menu_toolkit =
    [`I ("Zoom In", gl#gldrawer#zoom_in);
     `I ("Zoom Out", gl#gldrawer#zoom_out);
     `S;
     `I ("Map M_anager", MapDialogs.map_manager gl#gldrawer);
     `I ("View _Height Window", CamlExt.id);
     `I ("_Goto...", CamlExt.id);
     `S;
     `I ("Set _Level Parameters...", MapDialogs.info_dialog map);
     `I ("Set _Item Parameters...", CamlExt.id);
     `I ("Set _Monster Parameters...", CamlExt.id);
     `I ("Edit Map Item _Parameters...", CamlExt.id);
     `S;
     `I ("_Recenter Level", CamlExt.id);
     `I ("Pave Level", map#pave);
     `I ("Nuke Objects Only...", map#nuke);
     `I ("Nuke and Pave Level...", map#nuke_and_pave)]

let smithy_menu_toolkit =
    (* TODO: again, does merge_points really need access to gldrawer? *)
    [`I ("Merge Selected Points", GeomEdit.merge_points gl#gldrawer map);
     `I ("Garbage Collect", Gc.full_major)]

(* entry point for the application proper *)
let _ =
    window#event#connect#delete ~callback:deleting_window;
    window#connect#destroy ~callback:quit_application;
    window#event#connect#key_press ~callback:gl#send_key;
    eventbox#event#connect#button_press ~callback:gl#send_mousedown;
    eventbox#event#connect#motion_notify ~callback:gl#send_mousedrag;
    eventbox#event#connect#button_release ~callback:gl#send_mouseup;
    gl#gldrawer#set_map map;
    GToolbox.build_menu file_menu ~entries:file_menu_toolkit;
    GToolbox.build_menu view_menu ~entries:view_menu_toolkit;
    GToolbox.build_menu special_menu ~entries:special_menu_toolkit;
    GToolbox.build_menu smithy_menu ~entries:smithy_menu_toolkit;
    window#show ();
    GlFlatDraw.toolbar#show ();
    window#present ();
    let args = Sys.argv in
    if Array.length args > 1 then
        map#read_from_file args.(Array.length args - 1);
    GMain.Main.main ()
