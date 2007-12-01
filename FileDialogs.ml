(*** FileDialogs.ml contains a handful of GTK routines for handling the opening
 * and closing of files, in particular throwing up dialogs, capturing filenames,
 * passing the filenames to the appropriate MapFormat.map object, and handling
 * resultant errors. ***)

open MapFormat

let fail_dialog () =
    let dialog = GWindow.dialog ~title:Resources.warning ~modal:true () in
    let hb1 = GPack.hbox ~packing:dialog#action_area#add () in
    GMisc.label ~text:"Failed to perform file operation." ~packing:hb1#add ();
    dialog#add_button_stock `OK `OK;
    dialog#run ();
    dialog#destroy ()

let open_file_dialog map_obj set_title () =
    let dialog = GWindow.file_selection ~title:"Open Map" () in
    begin match dialog#run () with
        |`OK ->
            begin try map_obj#read_from_file dialog#filename;
                set_title ("Smithy: " ^ dialog#filename)
            with _ -> fail_dialog () end;
        |_ -> ()
    end;
    dialog#destroy ()

let save_file_dialog map_obj set_title () =
    let dialog = GWindow.file_selection ~title:"Save Map" () in
    begin match dialog#run () with
        |`OK ->
            begin try map_obj#write_to_file dialog#filename;
                set_title ("Smithy: " ^ dialog#filename)
            with _ -> fail_dialog () end
        |_ -> ()
    end;
    dialog#destroy ()

let silent_save map_obj set_title () =
    if map_obj#get_filename () = "" then
        save_file_dialog map_obj set_title ()
    else
        map_obj#write_to_file (map_obj#get_filename ())
