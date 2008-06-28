(*** FileDialogs.ml contains a handful of GTK routines for handling the opening
 * and closing of files, in particular throwing up dialogs, capturing filenames,
 * passing the filenames to the appropriate MapFormat.map object, and handling
 * resultant errors. ***)

(* this stores the path that we were looking at in the file dialog last time, so
 * that we don't keep jumping back to pwd when editing multiple maps *)
let path = ref None

(* this throws up a pretty useless failure dialog. TODO: could easily be
 * enhanced by allowing varying error messages :) *)
let fail_dialog () =
    let dialog = GWindow.dialog ~title:Resources.warning ~modal:true () in
    GMisc.label ~text:"Failed to perform file operation." ~packing:dialog#vbox#add ();
    dialog#add_button_stock `OK `OK;
    dialog#run (); (* don't care about the result *)
    dialog#destroy ()

(* puts up the open dialog *)
let open_file_dialog set_title drawer _ =
    (* actually construct the dialog *)
    let dialog = GWindow.file_selection ~title:"Open Map" () in
    (* read in the path from our last use *)
    begin match !path with None -> () | Some path ->
        dialog#set_filename path end;
    begin match dialog#run () with
        |`OK ->
            (* open the file *)
            begin try
                MapFormat.read_from_file dialog#filename;
                set_title ("Smithy: " ^ dialog#filename)
            with _ -> fail_dialog () end;
        |_ -> () (* we were cancelled :( *)
    end;
    (* store the path for next time *)
    path := Some dialog#filename;
    dialog#destroy ();
    (* refresh the screen with the new map contents *)
    drawer#draw ()

(* almost the same, but for the save dialog *)
let save_file_dialog set_title _ =
    let dialog = GWindow.file_selection ~title:"Save Map" () in
    (* use the path from last time *)
    begin match !path with None -> () | Some path ->
        dialog#set_filename path end;
    begin match dialog#run () with
        |`OK ->
            (* attempt to write the file out to disk *)
            begin try
                MapFormat.write_to_file dialog#filename;
                set_title ("Smithy: " ^ dialog#filename)
            with _ -> fail_dialog () end
        |_ -> ()
    end;
    (* store the path for next time *)
    path := Some dialog#filename;
    dialog#destroy ()

(* for the Save option instead of Save As *)
let silent_save set_title _ =
    if !MapFormat.filename = "" then
        (* if our map has a filename associated with it, just save to that
         * location again *)
        save_file_dialog set_title ()
    else
        (* otherwise, ask the user for such a filename *)
        MapFormat.write_to_file !MapFormat.filename

let new_map drawer _ =
    path := None;
    MapFormat.reset_structures ();
    drawer#draw ()
