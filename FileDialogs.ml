(*** FileDialogs.ml contains a handful of GTK routines for handling the opening
 * and closing of files, in particular throwing up dialogs, capturing filenames,
 * passing the filenames to the appropriate MapFormat.map object, and handling
 * resultant errors. ***)

(* this stores the path that we were looking at in the file dialog last time, so
 * that we don't keep jumping back to pwd when editing multiple maps *)
let path = ref None

(* this throws up a pretty useless failure dialog. TODO: could easily be
 * enhanced by allowing varying error messages :) *)
let fail_dialog str () =
    let dialog =
        GWindow.message_dialog
            ~message:("Failed to perform file operation: " ^ str)
            ~message_type:`ERROR ~buttons:GWindow.Buttons.close
            ~modal:true ~title:Resources.warning () in
    dialog#run (); (* don't care about the result *)
    dialog#destroy ()

(* routines for reading and writing light libraries *)
let load_light_library filename =
    let fh = open_in_bin filename in
    let length = in_channel_length fh in
    let lights = MapFormat.read_chunk fh length MapFormat.light_length
                                      MapTypes.lite_reader in
    close_in fh;
    lights

let save_light_library filename lights =
    let fh = open_out_bin filename in
    Array.iter (MapTypes.lite_writer fh) lights;
    close_out fh

(* generic open/save dialogs *)
let open_file_dialog f title _ =
    (* actually construct the dialog *)
    let dialog = GWindow.file_selection ~title () in
    (* read in the path from our last use *)
    begin match !path with None -> () | Some path ->
        dialog#set_filename path end;
    begin match dialog#run () with
        |`OK ->
            (* open the file *)
            begin try f dialog#filename
            with exn -> fail_dialog (Printexc.to_string exn) () end;
        |_ -> () (* we were cancelled :( *)
    end;
    (* store the path for next time *)
    path := Some dialog#filename;
    dialog#destroy ()

(* almost the same, but for the save dialog *)
let save_file_dialog f title _ =
    let dialog = GWindow.file_selection ~title () in
    (* use the path from last time *)
    begin match !path with None -> () | Some path ->
        dialog#set_filename path end;
    begin match dialog#run () with
        |`OK ->
            (* attempt to write the file out to disk *)
            begin try f dialog#filename
            with exn -> fail_dialog (Printexc.to_string exn) () end
        |_ -> ()
    end;
    (* store the path for next time *)
    path := Some dialog#filename;
    dialog#destroy ()

(* open/save dialogs for maps *)
let open_map_dialog set_title drawer _ =
    open_file_dialog (fun filename ->
        MapFormat.read_from_file filename;
        set_title ("Smithy: " ^ filename);
        drawer#draw ()) "Open Map" ()
let save_map_dialog set_title _ =
    save_file_dialog (fun filename ->
        MapFormat.write_to_file filename;
        set_title ("Smithy: " ^ filename)) "Save Map" ()

(* open/save dialogs for light libraries *)
let load_and_append_light_lib drawer _ =
    open_file_dialog (fun filename ->
            let lights = load_light_library filename in
            MapFormat.lights := Array.append !MapFormat.lights lights;
            drawer#draw ())
        "Load and Append Light Library" ()
let load_and_replace_light_lib drawer _ =
    open_file_dialog (fun filename ->
            let lights = load_light_library filename in
            MapFormat.lights := lights;
            drawer#draw ())
        "Load and Replace Light Library" ()
let save_light_lib _ =
    save_file_dialog (fun filename ->
            save_light_library filename !MapFormat.lights)
        "Save Light Library" ()

(* for the Save option instead of Save As *)
let silent_save set_title _ =
    if !MapFormat.filename = "" then
        (* if our map has a filename associated with it, just save to that
         * location again *)
        save_map_dialog set_title ()
    else
        (* otherwise, ask the user for such a filename *)
        MapFormat.write_to_file !MapFormat.filename

let new_map drawer _ =
    MapFormat.reset_structures ();
    drawer#draw ()
