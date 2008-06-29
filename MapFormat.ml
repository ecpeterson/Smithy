(*** MapFormat.ml contains various bits of constant information about the
 * Marathon map format, along with routines to read and write the information to
 * disk. ***)

open CamlExt
open MapTypes

(* how wide is a map? *)
let map_width = 65536
let half_map_width = map_width / 2

(* more magic numbers *)
let offset_of_first_chunk = 128
let point_length = 4
let line_length = 32
let poly_length = 128
let side_length = 64
let light_length = 100
let obj_length = 16
let info_length = 88
let media_length = 32
let placement_length = 12
let platform_length = 32
let optimized_point_length = 16
let number_of_placements = 128 (* 64 items, 64 monsters *)

(* TODO: is it feasible to move these to MapTypes? *)
type environment_code = Water | Lava | Sewage | Jjaro | Pfhor
let environment_descriptor = 0, [Water; Lava; Sewage; Jjaro; Pfhor]
type environment_flag = Vacuum | Magnetic | Rebellion | Low_Gravity
let env_flags_descriptor = [1, Vacuum; 2, Magnetic; 4, Rebellion;
                            8, Low_Gravity]
type mission_type = Extermination | Exploration | Retrieval | Repair | Rescue
let mission_descriptor = [1, Extermination; 2, Exploration; 4, Retrieval;
                          8, Repair; 16, Rescue]
type entry_point_type = Solo | Coop| EMFH | KTMWTB | KOTH | Defense | Rugby
let entry_point_descriptor = [1, Solo; 2, Coop; 4, EMFH; 8, KTMWTB; 16, KOTH;
                              32, Rugby; 64, Defense]

(* utility to read in all the entries of a chunk, given an entry factory *)
let read_chunk fh chunk_length entry_length (reader: in_channel -> 'a) =
    let array = Array.make (chunk_length / entry_length) (Obj.magic () : 'a) in
    for i = 0 to (chunk_length / entry_length) - 1 do
        array.(i) <- reader fh
    done;
    array

(* utility to write out all the entries of a chunk, given the array of entries *)
let write_chunk fh array entry_length chunk_header writer =
    let length = Array.length array in
    let start = pos_out fh in
    let chunk_length = length * entry_length in
    let writer = writer fh in
    if length > 0 then begin
        output_string fh chunk_header; (* header *)
            (* the +16 here is for the length of the header itself *)
        output_dword fh (start + chunk_length - offset_of_first_chunk + 16);
        output_dword fh chunk_length;
        output_padding fh 4; (* "offset", screw that *)
        Array.iter (fun x -> writer x) array
    end else ()

(*** we don't wrap it in a class because classes are gay ***)
    (* information we've loaded from the map *)
let points = ref (Array.make 0 empty_point)
let lines = ref (Array.make 0 empty_line)
let polygons = ref (Array.make 0 empty_polygon)
let sides = ref (Array.make 0 empty_side)
let lights = ref (Array.make 0 empty_light)
let objs = ref (Array.make 0 empty_obj)
let media = ref (Array.make 0 empty_media)
let placements =
    let arr = Array.make number_of_placements empty_placement in
    for i = 0 to number_of_placements - 1 do
        arr.(i) <- new MapTypes.placement
    done; ref arr
let platforms = ref (Array.make 0 empty_platform)
let environment_code = ref Water
let physics_model = ref 0
let landscape = ref 0
let mission_flags = ref []
let environment_flags = ref []
let level_name = ref (String.make 66 '\000')
let entry_point_flags = ref []
let filename = ref (String.make 0 '\000')

let reset_structures _ =
    points := Array.make 0 empty_point;
    lines := Array.make 0 empty_line;
    polygons := Array.make 0 empty_polygon;
    sides := Array.make 0 empty_side;
    lights := Array.make 0 empty_light;
    objs := Array.make 0 empty_obj;
    media := Array.make 0 empty_media;
    placements := begin
        let arr = Array.make number_of_placements empty_placement in
        for i = 0 to number_of_placements - 1 do
            arr.(i) <- new MapTypes.placement
        done; arr end;
    platforms := Array.make 0 empty_platform;
    level_name := String.make 66 '\000';
    filename := String.make 66 '\000'

    (* read in various chunks *)
let read_points fh length =
    points := read_chunk fh length point_length pnts_reader
let read_lines fh length =
    lines := read_chunk fh length line_length lins_reader
let read_polys fh length =
    polygons := read_chunk fh length poly_length poly_reader
let read_sides fh length =
    sides := read_chunk fh length side_length sids_reader
let read_lights fh length =
    lights := read_chunk fh length light_length lite_reader
let read_objects fh length =
    objs := read_chunk fh length obj_length objs_reader
let read_media fh length =
    media := read_chunk fh length media_length medi_reader
let read_placements fh length =
    placements := read_chunk fh length placement_length plac_reader
let read_platforms fh length =
    platforms := read_chunk fh length platform_length plat_reader
let read_optimized_points fh length =
    points := read_chunk fh length optimized_point_length epnt_reader

    (* write out various chunks *)
let write_points fh = write_chunk fh !points point_length "PNTS" pnts_writer
let write_lines fh = write_chunk fh !lines line_length "LINS" lins_writer
let write_polys fh = write_chunk fh !polygons poly_length "POLY" poly_writer
let write_sides fh = write_chunk fh !sides side_length "SIDS" sids_writer
let write_lights fh = write_chunk fh !lights light_length "LITE" lite_writer
let write_objects fh = write_chunk fh !objs obj_length "OBJS" objs_writer
let write_media fh = write_chunk fh !media media_length "medi" medi_writer
let write_placements fh =
                write_chunk fh !placements placement_length "plac" plac_writer
let write_platforms fh =
                write_chunk fh !platforms platform_length "plat" plat_writer

(* read in a map info chunk *)
let read_info fh length =
    environment_code := of_enum environment_descriptor (input_word fh);
    physics_model := input_word fh;
    landscape := input_word fh;
    mission_flags := of_bitflag mission_descriptor (input_word fh);
    environment_flags := of_bitflag env_flags_descriptor (input_word fh);
    ignore (input_dword fh); ignore (input_dword fh); (* skip 8 bytes *)
    really_input fh !level_name 0 66;
    entry_point_flags := CamlExt.of_bitflag entry_point_descriptor
                                            (input_dword fh)

    (* write out a map info chunk *)
let write_info fh =
    let pos = pos_out fh in
    output_string fh "Minf"; (* chunk header *)
    output_dword fh (pos + info_length + 16 - offset_of_first_chunk);
    output_dword fh info_length;
    output_dword fh 0;
    (* now the actual chunk *)
    output_word fh (to_enum environment_descriptor !environment_code);
    output_word fh !physics_model;
    output_word fh !landscape;
    output_word fh (to_bitflag mission_descriptor !mission_flags);
    output_word fh (to_bitflag env_flags_descriptor !environment_flags);
    output_padding fh 8;
    output_string_n fh !level_name 66;
    output_dword fh (CamlExt.to_bitflag entry_point_descriptor
                                        !entry_point_flags)

(* read in a set of chunks from an initialized file pointer *)
let rec read_chunks fh =
    (* read in chunk header *)
    let chunk_start = pos_in fh in
    let chunk_name = String.make 4 '\000' in
    really_input fh chunk_name 0 4;
    let next_offset = input_dword fh in
    let length = input_dword fh in
    let offset = input_dword fh in
    (* match against chunk type *)
    begin match chunk_name with
        |"PNTS" -> read_points fh length
        |"LINS" -> read_lines fh length
        |"POLY" -> read_polys fh length
        |"SIDS" -> read_sides fh length
        |"Minf" -> read_info fh length
        |"LITE" -> read_lights fh length
        |"OBJS" -> read_objects fh length
        |"plac" -> read_placements fh length
        |"plat" -> read_platforms fh length
        |"medi" -> read_media fh length
        (* and now support for optimized chunks *)
        |"EPNT" -> read_optimized_points fh length
        |_ -> print_endline ("epic fail: " ^ chunk_name)
    end;
    seek_in fh (next_offset + offset_of_first_chunk);
    if next_offset <> 0
        then read_chunks fh
        else ()

(* read in an unmerged map file *)
let read_from_file fname =
    let fh = open_in_bin fname in
    (* read in the map header *)
    let version = input_word fh in
    if version > 4 then raise (Failure "Bad WAD version!") else
    let data_version = input_word fh in
    if data_version > 1 then raise (Failure "Bad data version!") else
    (* forward to the first chunk *)
    seek_in fh offset_of_first_chunk;
    read_chunks fh;
    (* close the file *)
    close_in fh;
    filename := fname

(* write out to an unmerged map file *)
let write_to_file filename =
    let fh = open_out filename in
    output_word fh 2; (* output the wad version, forge uses 2 *)
    output_word fh 1; (* output the data version, forge uses 1 *)
    output_string_n fh "TODO filename" 64; (* string containing the short filename *)
    output_dword fh 0; (* TODO: checksum *)
    output_dword fh 0; (* temporary directory offset *)
    output_word fh 1; (* wad count *)
    output_word fh 0; (* application-specific directory size *)
    output_word fh 16; (* entry header size *)
    output_word fh 10; (* directory entry base size *)
    output_dword fh 0; (* parent checksum, always 0 *)
    output_padding fh 40; (* twenty bytes wasted *)
    (* now write the actual chunks, order is unimportant *)
    write_points fh;
    write_lines fh;
    write_polys fh;
    write_sides fh;
    write_lights fh;
    write_objects fh;
    write_placements fh;
    write_platforms fh;
    write_media fh;
    (* this let block and the code that follows it must wrap the last chunk
     * to be written out, to make sure we crimp the end of the file.  note:
     * we obviously must guarantee that this chunk gets written to disk,
     * since otherwise we might not crimp anything.  the only chunk we're
     * guaranteed to write is Minf, so it belongs here. *)
    let before_last_chunk = pos_out fh in
    write_info fh;
    let jump_to_end = pos_out fh in
    seek_out fh (before_last_chunk + 4);
    output_dword fh 0;
    seek_out fh 72;
    output_dword fh jump_to_end; (* real directory offset *)
    seek_out fh jump_to_end; (* jump to end *)
    output_dword fh offset_of_first_chunk; (* write trailer *)
    output_dword fh (jump_to_end - offset_of_first_chunk);
    output_word fh 0;
    close_out fh

(* allow others to add objects *)
let add_point point =
    let append_array = Array.make 1 point in
    points := Array.append !points append_array;
    Array.length !points - 1

let add_line line =
    let append_array = Array.make 1 line in
    lines := Array.append !lines append_array;
    Array.length !lines - 1

let add_polygon poly =
    let append_array = Array.make 1 poly in
    polygons := Array.append !polygons append_array;
    Array.length !polygons - 1

let add_media m =
    let append_array = Array.make 1 m in
    media := Array.append !media append_array;
    Array.length !media - 1

let add_light light =
    let append_array = Array.make 1 light in
    lights := Array.append !lights append_array;
    Array.length !lights - 1

let add_platform plat =
    let append_array = Array.make 1 plat in
    platforms := Array.append !platforms append_array;
    Array.length !platforms - 1

let add_object obj =
    let append_array = Array.make 1 obj in
    objs := Array.append !objs append_array;
    Array.length !objs - 1

(** geometry selection functions **)
(* gets the closest object to the point (x0, y0) *)
let get_closest_object x0 y0 =
    array_fold_left_indexed (fun (accd, acci) this_obj i ->
        let (xi, yi, _) = this_obj#point () in
        let this_distance = distance (float xi, float yi) (x0, y0) in
        if this_distance < accd then (this_distance, i) else (accd, acci))
            (infinity, 0) !objs

    (* gets the closest map point to the point (x0, y0) *)
let get_closest_point x0 y0 =
    array_fold_left_indexed (fun (accd, acci) this_point i ->
        match this_point#vertex () with (xi, yi) ->
        let this_distance = distance (float xi, float yi) (x0, y0) in
        if this_distance < accd then (this_distance, i) else (accd, acci))
            (infinity, 0) !points

    (* gets the closest line to the point (x0, y0) *)
let get_closest_line x0 y0 =
    let line_distance (e0x, e0y) (e1x, e1y) (x, y) =
        let u = (((x -. e0x) *. (e1x -. e0x)) +. ((y -. e0y) *. (e1y -. e0y)))
                /. ((e1x -. e0x)**2.0 +. (e1y -. e0y)**2.0) in
        if u > 1.0 then distance (x, y) (e1x, e1y) else
        if u < 0.0 then distance (x, y) (e0x, e0y) else
        let ix = e0x +. u *. (e1x -. e0x) in
        let iy = e0y +. u *. (e1y -. e0y) in
        distance (x, y) (ix, iy) in
    array_fold_left_indexed (fun (accd, acci) this_line i ->
            let (p0i, p1i) = this_line#endpoints () in
            let (p0x, p0y) = !points.(p0i)#vertex () in
            let (p1x, p1y) = !points.(p1i)#vertex () in
            let d = line_distance (float p0x, float p0y) (float p1x, float p1y) (x0, y0) in
            if d < accd then (d, i) else (accd, acci))
        (infinity, 0) !lines

(* takes a polygon and manually forms the point loop based on the poly's
 * guaranteed-to-be-generated line loop.  most of the mess comes from
 * ordering the line's vertices, which aren't guaranteed to be in any
 * particular order *)
let get_poly_ring (poly : polygon) =
    let endpoints = Array.map
        (fun x -> !lines.(x)#endpoints ())
        (poly#line_indices ()) in
    let rec loop n acc =
        if n = poly#vertex_count () then acc else
        let (e0, e1) = endpoints.(n) in
        if n = 0 then begin
            let (e2, e3) = endpoints.(1) in
            if e0 = e2 || e0 = e3 then
                loop 1 [e0]
            else
                loop 1 [e1]
        end else if e0 = List.hd acc then
            loop (n+1) (e1 :: acc)
        else
            loop (n+1) (e0 :: acc) in
    loop 0 []

(* gets a polygon that encloses the point (x0, y0) *)
let get_enclosing_poly x0 y0 =
    (* what a useful constant *)
    let pi = asin 1.0 *. 2.0 in
    (* computes the angle between the line from the origin to (x0, y0) and
     * the line from the origin to (x1, y1), returns a value in the range
     * [-pi, pi] *)
    let angle (x0, y0) (x1, y1) =
        let theta1 = atan2 y0 x0 in
        let theta2 = atan2 y1 x1 in
        let dtheta = theta2 -. theta1 in
        let dtheta = mod_float dtheta (2.0 *. pi) in
        if dtheta > pi then
            dtheta -. 2.0 *. pi
        else if dtheta < 0.0 -. pi then
            dtheta +. 2.0 *. pi
        else
            dtheta in
    (* the idea here is that if we triangulate a polygon by taking a bunch
     * of triangles that all share a common point ( (x0, y0) ), if that
     * common point is inside the polygon then these triangles sweep out a
     * signed angle of 2pi and a signed angle of 0 otherwise *)
    let rec sum_angles points (x0, y0) i acc =
        if i >= List.length points then acc else
        let next = (if i = (List.length points) - 1 then 0 else i + 1) in
        let (p0x, p0y) = List.nth points i in
        let (p1x, p1y) = List.nth points next in
        let p0 = (float p0x -. x0, float p0y -. y0) in
        let p1 = (float p1x -. x0, float p1y -. y0) in
        sum_angles points (x0, y0) (i+1) (angle p0 p1 +. acc) in
    (* iterate through the map's polygons until we find one that works *)
    let rec g_e_p_aux x0 y0 i =
        if i = Array.length !polygons then None else
        let poly = !polygons.(i) in
        let poly_ring = get_poly_ring poly in
        let poly_points = List.map (fun x -> !points.(x)#vertex ()) poly_ring in
        let sum = sum_angles poly_points (x0, y0) 0 0.0 in
        if abs_float sum < pi then
            g_e_p_aux x0 y0 (i+1)
        else
            Some i in
    g_e_p_aux (x0 +. 1.0) (y0 +. 1.0) 0

(* when we move a point, we'll want to recalcuate the lengths of lines
 * attached to it.  this takes care of that entire process *)
let recalculate_lengths point =
    let line_length = Array.length !lines in
    let length (x0, y0) (x1, y1) =
        int_of_float (((float x0 -. (float x1))**2.0 +.
                       (float y0 -. (float y1))**2.0)**0.5) in
    let rec line_loop n =
        if n = line_length then () else
        let line = !lines.(n) in
        let (p0, p1) = line#endpoints () in
        if p0 = point || p1 = point then
            let p0 = !points.(p0)#vertex () in
            let p1 = !points.(p1)#vertex () in
            line#set_length (length p0 p1);
        line_loop (n+1) in
    line_loop 0

(* deletes a side (i.e. a texture) and performs cleanup *)
let delete_side n =
    let side = !sides.(n) in
    let parent_poly = !polygons.(side#polygon_index ()) in
    let parent_line = !lines.(side#line_index ()) in
    let poly_side_array = parent_poly#side_indices () in
    (* get rid of this side from the parent array, filling in the empty end
     * slot with a -1 value, signalling emptiness *)
    delete_from_array poly_side_array n (-1);
    (* actually delete the side from the sides array *)
    sides := delete_from_array_and_resize !sides n;
    (* iterate through all the available polys, shifting the indices into
     * the sides array down as needed *)
    Array.iter (fun poly -> destructive_map
            (fun x -> if x > n then x - 1 else x)
            (poly#side_indices ())) !polygons;
    (* iterate through the lines, again shifting indices *)
    Array.iter (fun line ->
        let (cw, ccw) = line#cw_poly_side_index (), line#ccw_poly_side_index () in
        if cw = n then line#set_cw_poly_side_index (-1);
        if cw > n then line#set_cw_poly_side_index (cw - 1);
        if ccw = n then line#set_ccw_poly_side_index (-1);
        if ccw > n then line#set_ccw_poly_side_index (ccw - 1)) !lines

(* deletes a polygon and performs cleanup *)
let delete_poly n =
    let poly = !polygons.(n) in
    (*let lsides = poly#side_indices () in*)
    (* deletes a whole sides array *)
    (*let rec trash_sides m =*)
        (*let side = Array.get (poly#side_indices ()) m in*)
        (*try*)
            (*let side_obj = Array.get sides side in*)
            (*if n = 8 then () else *)
            (*if side_obj#polygon_index () = n then begin*)
                (*self#delete_side side;*)
                (*trash_sides m*)
            (*end else trash_sides (m+1)*)
        (*with _ -> if n < 7 then trash_sides (m+1) else () in*)
    (* delete this poly's sides array *)
    (*trash_sides 0;*)
    (* delete this polygon from the polgons array *)
    polygons := delete_from_array_and_resize !polygons n;
    (* shift down all the line poly owner indices *)
    Array.iter (fun line ->
            let cw_poly_owner = line#cw_poly_owner () in
            let ccw_poly_owner = line#ccw_poly_owner () in
            if (cw_poly_owner = n || ccw_poly_owner = n) then begin
                line#set_flags [SOLID]
            end;
            line#set_cw_poly_owner
                (if cw_poly_owner > n then (cw_poly_owner - 1) else
                 if cw_poly_owner = n then -1 else cw_poly_owner);
            line#set_ccw_poly_owner
                (if ccw_poly_owner > n then (ccw_poly_owner - 1) else
                 if ccw_poly_owner = n then -1 else ccw_poly_owner))
        !lines;
    (* shift down all the side poly own indices *)
    Array.iter (fun side ->
        let pi = side#polygon_index () in
        if pi > n then side#set_polygon_index (pi - 1)) !sides

(* deletes a line and performs cleanup *)
let delete_line n =
    let line = !lines.(n) in
    (* if our line is attached to polygons, delete them *)
    let poly0 = line#cw_poly_owner () in
    let poly1 = line#ccw_poly_owner () in
    let (poly0, poly1) = (max poly0 poly1, min poly0 poly1) in
    if poly0 <> -1 then delete_poly poly0;
    if poly1 <> -1 then delete_poly poly1;
    (* if our line is attached to points, detach this line and then count
     * their owners to see if we should delete them too *)
    (*let (p0, p1) = line#endpoints () in*)
    (*let (p0, p1) = (max p0 p1, min p0 p1) in*)
    (*line#set_endpoints (-1, -1);*)
    (*let p0_count = Array.fold_left (fun x y ->*)
        (*let (tp0, tp1) = y#endpoints () in*)
        (*if p0 = tp0 || p0 = tp1 then x - 1 else x) 0 lines in*)
    (*let p1_count = Array.fold_left (fun x y ->*)
        (*let (tp0, tp1) = y#endpoints () in*)
        (*if p1 = tp0 || p1 = tp1 then x - 1 else x) 0 lines in*)
    (* if they don't have any more references, trash them *)
    (*if p0_count = 0 then self#delete_point p0;*)
    (*if p1_count = 0 then self#delete_point p1;*)
    (* remove ourselves from the lines array *)
    lines := delete_from_array_and_resize !lines n;
    (* loop through the other polys, fix their line indices *)
    Array.iter (fun x ->
            let lines = x#line_indices () in
            destructive_map (fun x -> if x > n then x - 1 else x) lines)
        !polygons;
    (* loop through sides, fix their line owner indices *)
    Array.iter (fun x ->
            let li = x#line_index () in
            x#set_line_index (if li > n then li - 1 else li)) !sides

    (* deletes a point and performs cleanup *)
let rec delete_point n =
    let lines_length = Array.length !lines in
    (* if we have a parent line, find it *)
    let rec aux acc =
        if acc = lines_length then -1 else
        let (p0, p1) = !lines.(acc)#endpoints () in
        if p0 = n || p1 = n then acc else aux (acc + 1) in
    let target_line = aux 0 in
    if target_line <> -1 then begin
        (* if we do in fact have a parent line, then we need to delete it
         * and then try again *)
        delete_line target_line;
        delete_point n
    end else begin
        (* we don't have a parent, so let's delete the point itself.  start
         * by removing it from the points array *)
        points := delete_from_array_and_resize !points n;
        (* fix the endpoints of lines *)
        Array.iter (fun x ->
            let (p0, p1) = x#endpoints () in
            let p0 = if p0 > n then p0 - 1 else p0 in
            let p1 = if p1 > n then p1 - 1 else p1 in
            x#set_endpoints (p0, p1)) !lines;
        (* fix the point arrays in polygons *)
        Array.iter (fun x ->
            destructive_map (fun x -> if x > n then x - 1 else x)
                (x#endpoint_indices ())) !polygons
    end

(* deletes an object and performs cleanup *)
let delete_obj n =
    objs := delete_from_array_and_resize !objs n;
    Array.iter (fun x ->
        let y = x#first_object () in
        if y > n then x#set_first_object (y-1) else
        if y = n then x#set_first_object (-1)) !polygons

(* deletes a platform and performs cleanup *)
let delete_platform n =
    platforms := delete_from_array_and_resize !platforms n;
    Array.iter (fun x ->
        let y = x#permutation () in
        if x#kind () <> Platform then () else
        if y > n then x#set_permutation (y - 1) else
        if y = n then x#set_permutation (-1)) !polygons

(* safely deletes all the objects in a map *)
let rec nuke () =
    if !objs = [||] then () else begin
        delete_obj 0;
        nuke ()
    end

(* safely deletes all the sides in a map *)
(* TODO: wipe floor/ceiling textures too *)
let rec pave () =
    if !sides = [||] then () else begin
        delete_side 0;
        pave ()
    end

(* hee hee *)
let nuke_and_pave () =
    nuke (); pave ()
