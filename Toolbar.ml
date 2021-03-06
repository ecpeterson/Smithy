open CamlExt
open DrawModeSettings

class toolbar ~main_window ~title () =
object (self)
    val mutable window = Obj.magic ()
    val mutable vbox   = Obj.magic ()

    method window = window

    initializer
        window <- GWindow.window ~title ~show:false ~type_hint:`MENU
                                 ~resizable:false ();
        window#set_transient_for main_window#window#as_window;
        window#set_position `CENTER_ON_PARENT;
        window#event#connect#delete ~callback:(fun _ -> true);
        vbox <- GPack.vbox ~border_width:2 ~packing:window#add ();
        ()
end

class errorToolbar ~main_window ~title () =
object (self) inherit toolbar ~main_window ~title ()
    initializer
        GMisc.label ~text:"Unimplemented" ~packing:vbox#add ();
        ()
end

class drawToolbar ~main_window ~title () =
object (self) inherit toolbar ~main_window ~title ()
    val mutable buttons = Obj.magic ()

    method button_of_tool tool = List.assoc tool buttons

    initializer
        let buttonline, buttonarrow, buttonfill, buttonpoly,
            buttonzoom, buttonpan, buttonobj, buttontext =
            let hbox1 = GPack.hbox ~packing:vbox#pack () in
            let hbox2 = GPack.hbox ~packing:vbox#pack () in
            let hbox3 = GPack.hbox ~packing:vbox#pack () in
            let hbox4 = GPack.hbox ~packing:vbox#pack () in
            GButton.toggle_button  ~packing:hbox1#pack (),
            GButton.toggle_button  ~packing:hbox1#pack ~active:true (),
            GButton.toggle_button  ~packing:hbox2#pack (),
            GButton.toggle_button  ~packing:hbox2#pack (),
            GButton.toggle_button  ~packing:hbox3#pack (),
            GButton.toggle_button  ~packing:hbox3#pack (),
            GButton.toggle_button  ~packing:hbox4#pack (),
            GButton.toggle_button  ~packing:hbox4#pack () in
        GMisc.image ~file:Resources.arrowfile ~packing:buttonarrow#add ();
        GMisc.image ~file:Resources.linefile  ~packing:buttonline#add  ();
        GMisc.image ~file:Resources.polyfile  ~packing:buttonpoly#add  ();
        GMisc.image ~file:Resources.fillfile  ~packing:buttonfill#add  ();
        GMisc.image ~file:Resources.panfile   ~packing:buttonpan#add   ();
        GMisc.image ~file:Resources.zoomfile  ~packing:buttonzoom#add  ();
        GMisc.image ~file:Resources.textfile  ~packing:buttontext#add  ();
        GMisc.image ~file:Resources.objfile   ~packing:buttonobj#add   ();
        buttons <- [(ArrowTool, buttonarrow); (LineTool,  buttonline);
                    (PolyTool,  buttonpoly);  (FillTool,  buttonfill);
                    (PanTool,   buttonpan);   (ZoomTool,  buttonzoom);
                    (TextTool,  buttontext);  (ObjTool,   buttonobj)];
        List.iter (fun obj -> match obj with (tool, button) ->
                button#event#connect#button_press ~callback:(fun _ ->
                    self#clicked tool)
            |> ignore) buttons;
        ()

    method clicked tool =
        List.iter (fun x -> (snd x)#set_active false) buttons;
        active_tool := tool;
        main_window#orthodrawer#set_cursor (List.assoc tool tool_cursors);
        false
end

class entryToolbar ~main_window ~title ~label ~updater () =
object (self) inherit toolbar ~main_window ~title ()
    val mutable cb_descriptor = Obj.magic ()

    method entry = fst cb_descriptor
    method float_entry =
        try float_of_string self#entry#entry#text
        with _ -> 0.0
    method set_float f =
        self#entry#entry#set_text (string_of_float f)

    initializer
        let hbox = GPack.hbox ~border_width:2 ~packing:vbox#add () in
        GMisc.label ~text:label ~packing:hbox#add ();
        cb_descriptor <- GEdit.combo_box_entry_text ~packing:hbox#add ();
        self#update_list;
        ()

    method update_list =
        let (entry, (store, column)) = cb_descriptor in
        store#clear ();
        List.iter (fun str ->
            let row = store#append () in
            store#set ~row ~column str) (updater ())
end

class selectionToolbar ~main_window ~title ~label ~strings () =
object (self) inherit toolbar ~main_window ~title ()
    val mutable cb = Obj.magic ()

    method cb_index = cb#active
    method set_cb_index i = cb#set_active i

    initializer
        let hbox = GPack.hbox ~border_width:2 ~packing:vbox#add () in
        GMisc.label ~text:label ~packing:hbox#add ();
        cb <- fst (GEdit.combo_box_text ~packing:hbox#add ~strings ());
        cb#set_active 0;
        ()
end

class creationToolbar ~main_window ~title ~label ~updater () =
object (self) inherit entryToolbar ~main_window ~title ~label ~updater ()
    val mutable newbutton  = Obj.magic ()
    val mutable editbutton = Obj.magic ()

    method newbutton = newbutton
    method editbutton = editbutton
    method int_entry =
        try int_of_string self#entry#entry#text
        with _ -> 0
    method set_int i =
        self#entry#entry#set_text (string_of_int i)

    initializer
        let hbox = GPack.hbox ~border_width:2 ~packing:vbox#add () in
        newbutton  <- GButton.button ~label:"New"  ~packing:hbox#add ();
        editbutton <- GButton.button ~label:"Edit" ~packing:hbox#add ();
        ()
end

class toolbarHandler ~main_window ~title ~show () =
object (self)
    val mutable entry_toolbar     = Obj.magic ()
    val mutable selection_toolbar = Obj.magic ()
    val mutable creation_toolbar  = Obj.magic ()
    val mutable draw_toolbar      = Obj.magic ()
    val mutable error_toolbar     = Obj.magic ()
    val mutable position          = (-1, -1)
    val mutable need_init_pos     = true
    val mutable reposition        = false
    val mutable new_callback      = (fun _ -> ())
    val mutable edit_callback     = (fun _ -> ())

    method window =
        match !mode with
        |Elevation_Floor
        |Elevation_Ceiling ->
            entry_toolbar#window
        |Polygon_Types ->
            selection_toolbar#window
        |Lights_Floor
        |Lights_Ceiling
        |Lights_Liquid
        |Liquids
        |Sounds_Ambient
        |Sounds_Random ->
            creation_toolbar#window
        |Draw_Mode ->
            draw_toolbar#window
        |_ ->
            error_toolbar#window
    method entry_toolbar     = entry_toolbar
    method selection_toolbar = selection_toolbar
    method creation_toolbar  = creation_toolbar
    method draw_toolbar      = draw_toolbar
    method error_toolbar     = error_toolbar

    method update_list =
        match !mode with
        |Elevation_Floor
        |Elevation_Ceiling ->
            entry_toolbar#update_list
        |Lights_Floor
        |Lights_Ceiling
        |Lights_Liquid
        |Liquids
        |Sounds_Ambient
        |Sounds_Random ->
            creation_toolbar#update_list
        |_ -> ()
    method float_entry    = entry_toolbar#float_entry
    method set_float      = entry_toolbar#set_float
    method cb_index       = selection_toolbar#cb_index
    method set_cb_index   = selection_toolbar#set_cb_index
    method int_entry      = creation_toolbar#int_entry
    method set_int        = creation_toolbar#set_int
    method button_of_tool = draw_toolbar#button_of_tool
    method clicked        = draw_toolbar#clicked

    method connect_new f  = new_callback <- f
    method connect_edit f = edit_callback <- f

    initializer
        entry_toolbar     <- new entryToolbar ~main_window ~title ~label:""
                                              ~updater:(fun _ -> []) ();
        selection_toolbar <- new selectionToolbar ~main_window ~title ~label:""
                                                  ~strings:[] ();
        creation_toolbar  <- new creationToolbar ~main_window ~title ~label:""
                                                 ~updater:(fun _ -> []) ();
        draw_toolbar      <- new drawToolbar ~main_window ~title ();
        error_toolbar     <- new errorToolbar ~main_window ~title ();
        if show then self#show ();
        ()

    method change_editor_state new_mode =
        self#window#misc#hide ();
        begin match new_mode with
        |Elevation_Floor ->
            entry_toolbar <- new entryToolbar
                ~main_window ~title ~label:"Floor Elevation"
                ~updater:MapFormat.floor_heights ()
        |Elevation_Ceiling ->
            entry_toolbar <- new entryToolbar
                ~main_window ~title ~label:"Ceiling Elevation"
                ~updater:MapFormat.ceiling_heights ()
        |Polygon_Types ->
            selection_toolbar <- new selectionToolbar
                ~main_window ~title ~label:"Polygon Type"
                ~strings:ItemStrings.polygon_types ()
        |Lights_Floor ->
            creation_toolbar <- new creationToolbar
                ~main_window ~title ~label:"Floor Light"
                ~updater:MapFormat.floor_lights ()
        |Lights_Ceiling ->
            creation_toolbar <- new creationToolbar
                ~main_window ~title ~label:"Ceiling Light"
                ~updater:MapFormat.ceiling_lights ()
        |Lights_Liquid ->
            creation_toolbar <- new creationToolbar
                ~main_window ~title ~label:"Liquid Light"
                ~updater:MapFormat.liquid_lights ()
        |Liquids ->
            creation_toolbar <- new creationToolbar
                ~main_window ~title ~label:"Liquid"
                ~updater:MapFormat.liquids ()
        |Sounds_Ambient ->
            creation_toolbar <- new creationToolbar
                ~main_window ~title ~label:"Ambient Sound"
                ~updater:MapFormat.ambient_sounds ()
        |Sounds_Random ->
            creation_toolbar <- new creationToolbar
                ~main_window ~title ~label:"Random Sound"
                ~updater:MapFormat.random_sounds ()
        |Draw_Mode ->
            draw_toolbar <- new drawToolbar ~main_window ~title ()
        |_ ->
            error_toolbar <- new errorToolbar ~main_window ~title () end;
        mode := new_mode;
        self#show ();
        ()

    (* yes, all this is necessary, since gtk has no concept of how big the
     * window manager decorations are *)
    method show _ =
        let configure_cb geom =
            let new_x = GdkEvent.Configure.x geom in
            let new_y = GdkEvent.Configure.y geom in
            let width = GdkEvent.Configure.width geom in
            if need_init_pos then begin
                self#window#move ~x:(new_x + main_window#width / 2 + width / 2)
                                 ~y:new_y;
                need_init_pos <- false end;
            if reposition then begin
                let x, y = position in
                self#window#move ~x:(x - (new_x - x)) ~y:(y - (new_y - y));
                reposition <- false end;
            position <- (new_x, new_y);
            false in
        (* windows apparently sends an extraneous configure event on window
         * creation with meaningless values *)
        if Sys.os_type = "Unix" then
            self#window#event#connect#configure ~callback:configure_cb
        else
            self#window#event#connect#configure ~callback:(fun _ ->
                self#window#event#connect#configure ~callback:configure_cb;
                false);
        main_window#orthodrawer#set_cursor `ARROW;
        if self#window = creation_toolbar#window then begin
            creation_toolbar#newbutton#connect#clicked ~callback:(fun _ ->
                new_callback ());
            creation_toolbar#editbutton#connect#clicked ~callback:(fun _ ->
                edit_callback ());
            () end;
        if not need_init_pos then begin
            self#window#move ~x:(fst position) ~y:(snd position);
            reposition <- true end;
        self#window#show ();
        ()
end
