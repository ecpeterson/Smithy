open CamlExt
open DrawModeSettings

class toolbar ~main_window ~title ~show () =
object (self)
    (* widgets *)
    val mutable draw_toolbar =  Obj.magic ()
    val mutable entry_toolbar = Obj.magic ()
    val mutable entry_label =   Obj.magic ()
    val mutable numeric_entry = Obj.magic ()
    val mutable mediabox =      Obj.magic ()
    val mutable newbutton =     Obj.magic ()
    val mutable editbutton =    Obj.magic ()
    val mutable ptype_cb =      Obj.magic ()
    val mutable buttons =       Obj.magic ()

    (* actual data *)

    (* accessors *)
    method toolbar =
        match !mode with
        |Draw_Mode -> draw_toolbar
        |_         -> entry_toolbar
    method int_entry =
        try int_of_string numeric_entry#text
        with _ -> 0
    method float_entry =
        try float_of_string numeric_entry#text
        with _ -> 0.0
    method set_int v = numeric_entry#set_text (string_of_int v)
    method set_float v = numeric_entry#set_text (string_of_float v)
    method cb_index = ptype_cb#active
    method set_cb_index v = ptype_cb#set_active v
    method button_of_tool tool = List.assoc tool buttons
    method newbutton = newbutton
    method editbutton = editbutton

    (* constructor *)
    initializer
        self#draw_toolbar_init;
        self#entry_toolbar_init;
        if show then
            self#toolbar#show
        ()

    (* private methods *)
    method draw_toolbar_init =
        draw_toolbar <- GWindow.window ~title ~show:false ~type_hint:`MENU
                                       ~resizable:false ();
        draw_toolbar#set_transient_for main_window#as_window;
        ignore (draw_toolbar#event#connect#delete ~callback:(fun _ -> true));
        let buttonline, buttonarrow, buttonfill, buttonpoly,
            buttonzoom, buttonpan, buttonobj, buttontext =
            let vbox  = GPack.vbox ~packing:draw_toolbar#add () in
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

    method entry_toolbar_init =
        entry_toolbar <- GWindow.window ~title ~show:false ~border_width:2
                                        ~resizable:false ();
        entry_toolbar#set_transient_for main_window#as_window;
        ignore (entry_toolbar#event#connect#delete ~callback:(fun _ -> true));
        let vbox = GPack.vbox ~spacing:2 ~packing:entry_toolbar#add () in
        let hbox = GPack.hbox ~spacing:2 ~packing:vbox#add () in
        entry_label <- GMisc.label ~packing:hbox#add ();
        numeric_entry <- GEdit.entry ~packing:hbox#add ();
        mediabox <- GPack.hbox ~packing:vbox#add ();
        editbutton <- GButton.button ~packing:mediabox#add ();
        newbutton <- GButton.button ~packing:mediabox#add ();
        let cb, _ = GEdit.combo_box_text ~packing:vbox#add
                                         ~strings:ItemStrings.polygon_types
                                         () in
        cb#set_active 0;
        ptype_cb <- cb;
        ()

    (* other public methods *)
    method change_editor_state state =
        (* when we change between renderer modes, the GTK toolkits have to be
         * modified and hidden/shown appropriately *)
        let set_mode box entry_window buttons button_text1 button_text2
                     label_text menu entry =
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
            editbutton#set_label button_text2;
            () in
        mode := of_enum mode_descriptor state;
        numeric_entry#set_text "";
        ptype_cb#set_active 0;
        match !mode with
        |Polygon_Types ->
            set_mode false true false "" "" "" true false
        |Draw_Mode ->
            set_mode true false false "" "" "" false false
        |Elevation_Floor
        |Elevation_Ceiling ->
            set_mode false true false "" "" "Height" false true
        |Lights_Liquid
        |Lights_Floor
        |Lights_Ceiling ->
            set_mode false true true "New Light..." "Edit Light..." "Light"
                     false true
        |Liquids ->
            set_mode false true true "New Media..." "Edit Media..." "Media"
                     false true
        |Sounds_Random
        |Sounds_Ambient ->
            set_mode false true true "New Sound..." "Edit Sound..." "Sound"
                     false true
        |_ -> ()

    method clicked tool =
        List.iter (fun x -> (snd x)#set_active false) buttons;
        active_tool := tool;
        false
end
