(*** MapFormat.ml contains various bits of constant information about the
 * Marathon map format, along with routines to read and write the information to
 * disk. ***)
open CamlExt
open MapTypes

(* how wide is a map? *)
let map_width = 64.
let half_map_width = map_width /. 2.

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
let optimized_platform_length = 140
let ambient_length = 16
let random_length = 32
let annotation_length = 72
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
type entry_point_type = Solo | Coop| EMFH | KTMWTB | KOTH | Defense | Rugby |CTF
let entry_point_descriptor = [1, Solo; 2, Coop; 4, EMFH; 8, KTMWTB; 16, KOTH;
                              32, Rugby; 64, Defense; 128, CTF]

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
let lights = ref (Array.make 1 (new MapTypes.light))
let objs = ref (Array.make 0 empty_obj)
let media = ref (Array.make 0 empty_media)
let placements =
    let arr = Array.make number_of_placements empty_placement in
    for i = 0 to number_of_placements - 1 do
        arr.(i) <- new MapTypes.placement
    done; ref arr
let platforms = ref (Array.make 0 empty_platform)
let ambients = ref (Array.make 0 empty_ambient)
let randoms = ref (Array.make 0 empty_random)
let annotations = ref (Array.make 0 empty_annotation)
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
    lights := Array.make 1 (new MapTypes.light);
    objs := Array.make 0 empty_obj;
    media := Array.make 0 empty_media;
    ambients := Array.make 0 empty_ambient;
    randoms := Array.make 0 empty_random;
    annotations := Array.make 0 empty_annotation;
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
let read_optimized_platforms fh length =
    platforms := read_chunk fh length optimized_platform_length opt_plat_reader
let read_ambients fh length =
    ambients := read_chunk fh length ambient_length ambi_reader
let read_randoms fh length =
    randoms := read_chunk fh length random_length bonk_reader
let read_annotations fh length =
    annotations := read_chunk fh length annotation_length note_reader

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
let write_ambients fh =
                write_chunk fh !ambients ambient_length "ambi" ambi_writer
let write_randoms fh = write_chunk fh !randoms random_length "bonk" bonk_writer
let write_annotations fh =
                write_chunk fh !annotations annotation_length "NOTE" note_writer

(* read in a map info chunk *)
let read_info fh length =
    environment_code := of_enum environment_descriptor (input_word fh);
    physics_model := input_word fh;
    landscape := input_word fh;
    mission_flags := of_bitflag mission_descriptor (input_word fh);
    environment_flags := of_bitflag env_flags_descriptor (input_word fh);
    ignore (input_dword fh); ignore (input_dword fh); (* skip 8 bytes *)
    really_input fh !level_name 0 66;
    entry_point_flags := of_bitflag entry_point_descriptor
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
    output_dword fh (to_bitflag entry_point_descriptor
                                !entry_point_flags)

(* read in a set of chunks from an initialized file pointer *)
let rec read_chunks fh =
    (* read in chunk header *)
    let _chunk_start = pos_in fh in
    let chunk_name = String.make 4 '\000' in
    really_input fh chunk_name 0 4;
    let next_offset = input_dword fh in
    let length = input_dword fh in
    let _offset = input_dword fh in
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
        |"ambi" -> read_ambients fh length
        |"bonk" -> read_randoms fh length
        |"NOTE" -> read_annotations fh length
        (* iidx is something that the game can recalculate, and in addition its
         * existence signals to the game that many things have already /been/
         * precalculated, and screw that!  toss this chunk. *)
        |"iidx" -> ()
        (* and now support for optimized chunks *)
        |"EPNT" -> read_optimized_points fh length
        |"PLAT" -> read_optimized_platforms fh length
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
    write_ambients fh;
    write_randoms fh;
    write_annotations fh;
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
let add_builder a o =
    let append_array = Array.make 1 o in
    a := Array.append !a append_array;
    Array.length !a - 1
let add_point      = add_builder points
let add_line       = add_builder lines
let add_polygon    = add_builder polygons
let add_media      = add_builder media
let add_light      = add_builder lights
let add_platform   = add_builder platforms
let add_object     = add_builder objs
let add_side       = add_builder sides
let add_ambient    = add_builder ambients
let add_random     = add_builder randoms
let add_annotation = add_builder annotations

(** geometry selection functions **)
(* gets the closest object to the point (x0, y0) *)
let get_closest_object x0 y0 =
    array_fold_left_indexed (fun (accd, acci) this_obj i ->
        let (xi, yi, _) = this_obj#point in
        let this_distance = distance (xi, yi) (x0, y0) in
        if this_distance < accd then (this_distance, i) else (accd, acci))
            (infinity, 0) !objs

    (* gets the closest map point to the point (x0, y0) *)
let get_closest_point x0 y0 =
    array_fold_left_indexed (fun (accd, acci) this_point i ->
        match this_point#vertex with (xi, yi) ->
        let this_distance = distance (xi, yi) (x0, y0) in
        if this_distance < accd then (this_distance, i) else (accd, acci))
            (infinity, 0) !points

    (* gets the closest annotation to the point (x0, y0) *)
let get_closest_annotation x0 y0 =
    array_fold_left_indexed (fun (accd, acci) this_anno i ->
        match this_anno#location with (xi, yi) ->
        let this_distance = distance (xi, yi) (x0, y0) in
        if this_distance < accd then (this_distance, i) else (accd, acci))
            (infinity, 0) !annotations

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
            let (p0i, p1i) = this_line#endpoints in
            let (p0x, p0y) = !points.(p0i)#vertex in
            let (p1x, p1y) = !points.(p1i)#vertex in
            let d = line_distance (p0x, p0y) (p1x, p1y) (x0, y0) in
            if d < accd then (d, i) else (accd, acci))
        (infinity, 0) !lines

(* takes a polygon and manually forms the point loop based on the poly's
 * guaranteed-to-be-generated line loop.  most of the mess comes from
 * ordering the line's vertices, which aren't guaranteed to be in any
 * particular order *)
let get_poly_ring (poly : polygon) =
    let endpoints = Array.map
        (fun x -> !lines.(x)#endpoints) poly#line_indices in
    let rec loop n acc =
        if n = poly#vertex_count then acc else
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
        let p0 = (p0x -. x0, p0y -. y0) in
        let p1 = (p1x -. x0, p1y -. y0) in
        sum_angles points (x0, y0) (i+1) (angle p0 p1 +. acc) in
    (* iterate through the map's polygons until we find one that works *)
    let rec g_e_p_aux x0 y0 i =
        if i = Array.length !polygons then None else
        let poly = !polygons.(i) in
        let poly_ring = get_poly_ring poly in
        let poly_points = List.map (fun x -> !points.(x)#vertex) poly_ring in
        let sum = sum_angles poly_points (x0, y0) 0 0.0 in
        if abs_float sum < pi then
            g_e_p_aux x0 y0 (i+1)
        else
            Some i in
    g_e_p_aux x0 y0 0

(* when we move a point, we'll want to recalcuate the lengths of lines
 * attached to it.  this takes care of that entire process *)
let recalculate_lengths point =
    let line_length = Array.length !lines in
    let length (x0, y0) (x1, y1) =
        ((x0 -. x1)**2.0 +. (y0 -. y1)**2.0)**0.5 in
    let rec line_loop n =
        if n = line_length then () else
        let line = !lines.(n) in
        let (p0, p1) = line#endpoints in
        if p0 = point || p1 = point then
            let p0, p1 = !points.(p0)#vertex, !points.(p1)#vertex in
            line#set_length (length p0 p1);
        line_loop (n+1) in
    line_loop 0

(* deletes an object and performs cleanup *)
let delete_obj n =
    objs := delete_from_array_and_resize !objs n;
    Array.iter (fun x ->
        let y = x#first_object in
        if y > n then x#set_first_object (y-1) else
        if y = n then x#set_first_object (-1)) !polygons

(* deletes a platform and performs cleanup *)
let delete_platform n =
    platforms := delete_from_array_and_resize !platforms n;
    Array.iter (fun x ->
        let y = x#permutation in
        if x#kind <> Platform then () else
        if y > n then x#set_permutation (y - 1) else
        if y = n then x#set_permutation (-1)) !polygons

let delete_ambient n =
    ambients := delete_from_array_and_resize !ambients n;
    Array.iter (fun x ->
        let i = x#ambient_sound_image_index in
        if i > n then x#set_ambient_sound_image_index (i - 1)
        else if i = n then x#set_ambient_sound_image_index (-1)) !polygons

let delete_random n =
    randoms := delete_from_array_and_resize !randoms n;
    Array.iter (fun x ->
        let i = x#random_sound_image_index in
        if i > n then x#set_random_sound_image_index (i - 1)
        else if i = n then x#set_random_sound_image_index (-1)) !polygons

let delete_annotation n =
    annotations := delete_from_array_and_resize !annotations n

(* deletes a side (i.e. a texture) and performs cleanup *)
let delete_side n =
    let side = !sides.(n) in
    let parent_poly = !polygons.(side#polygon_index) in
    let poly_side_array = parent_poly#side_indices in
    (* get rid of this side from the parent array, filling in the empty end
     * slot with a -1 value, signalling emptiness *)
    delete_from_array poly_side_array n (-1);
    (* actually delete the side from the sides array *)
    sides := delete_from_array_and_resize !sides n;
    (* iterate through all the available polys, shifting the indices into
     * the sides array down as needed *)
    Array.iter (fun poly -> destructive_map
            (fun x -> if x > n then x - 1 else x) poly#side_indices) !polygons;
    (* iterate through the lines, again shifting indices *)
    Array.iter (fun line ->
        let (cw, ccw) = line#cw_poly_side_index, line#ccw_poly_side_index in
        if cw = n then line#set_cw_poly_side_index (-1);
        if cw > n then line#set_cw_poly_side_index (cw - 1);
        if ccw = n then line#set_ccw_poly_side_index (-1);
        if ccw > n then line#set_ccw_poly_side_index (ccw - 1)) !lines

(* deletes a polygon and performs cleanup *)
let delete_poly n =
    (*let poly = !polygons.(n) in*)
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
            let cw_poly_owner = line#cw_poly_owner in
            let ccw_poly_owner = line#ccw_poly_owner in
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
        let pi = side#polygon_index in
        if pi > n then side#set_polygon_index (pi - 1)) !sides;
    (* clean up the objects *)
    let rec clean_objs () =
        let target = array_find (fun x -> x#polygon = n) !objs in
        if target < Array.length !objs then begin
            delete_obj target;
            clean_objs ()
        end in
    clean_objs ();
    Array.iter (fun x ->
        let i = x#polygon in if i > n then x#set_polygon (i-1)) !objs;
    (* clean up the platforms *)
    let rec clean_platforms () =
        let target = array_find (fun x -> x#polygon_index = n) !platforms in
        if target < Array.length !platforms then begin
            delete_platform target;
            clean_platforms ()
        end in
    clean_platforms ();
    Array.iter (fun x ->
        let i = x#polygon_index in
        if i >= n then x#set_polygon_index (i-1)) !platforms

let delete_point_no_bs n =
    points := delete_from_array_and_resize !points n;
    (* fix the endpoints of lines *)
    Array.iter (fun x ->
        let (tp0, tp1) = x#endpoints in
        let tp0 = if tp0 > n then tp0 - 1 else tp0 in
        let tp1 = if tp1 > n then tp1 - 1 else tp1 in
        x#set_endpoints (tp0, tp1)) !lines;
    (* fix the point arrays in polygons *)
    Array.iter (fun x ->
        destructive_map (fun x -> if x > n then x - 1 else x)
            x#endpoint_indices) !polygons

(* deletes a line and DOES NOT PERFORM POINT CLEANUP *)
let delete_line_no_bs n =
    let line = !lines.(n) in
    (* if our line is attached to polygons, delete them *)
    let poly0 = line#cw_poly_owner in
    let poly1 = line#ccw_poly_owner in
    let (poly0, poly1) = (max poly0 poly1, min poly0 poly1) in
    if poly0 <> -1 then delete_poly poly0;
    if poly1 <> -1 then delete_poly poly1;
    (* actually delete the line *)
    lines := delete_from_array_and_resize !lines n;
    (* loop through the other polys, fix their line indices *)
    Array.iter (fun x ->
            let lines = x#line_indices in
            destructive_map (fun x -> if x > n then x - 1 else x) lines)
        !polygons;
    (* loop through sides, fix their line owner indices *)
    Array.iter (fun x ->
            let li = x#line_index in
            x#set_line_index (if li > n then li - 1 else li)) !sides

(* deletes a line and performs cleanup *)
let delete_line n =
    let line = !lines.(n) in
    (* if our line is attached to polygons, delete them *)
    let poly0 = line#cw_poly_owner in
    let poly1 = line#ccw_poly_owner in
    let (poly0, poly1) = (max poly0 poly1, min poly0 poly1) in
    if poly0 <> -1 then delete_poly poly0;
    if poly1 <> -1 then delete_poly poly1;
    (* actually delete the line *)
    lines := delete_from_array_and_resize !lines n;
    (* loop through the other polys, fix their line indices *)
    Array.iter (fun x ->
            let lines = x#line_indices in
            destructive_map (fun x -> if x > n then x - 1 else x) lines)
        !polygons;
    (* loop through sides, fix their line owner indices *)
    Array.iter (fun x ->
            let li = x#line_index in
            x#set_line_index (if li > n then li - 1 else li)) !sides;
    (** delete unused points **)
    let p0, p1 = line#endpoints in
    let p0, p1 = max p0 p1, min p0 p1 in
    let i0 = array_find (fun x ->
        let np0, np1 = x#endpoints in np0 = p0 || np1 = p0) !lines in
    if i0 = Array.length !lines then
        delete_point_no_bs p0;
    let i1 = array_find (fun x ->
        let np0, np1 = x#endpoints in np0 = p1 || np1 = p1) !lines in
    if i1 = Array.length !lines then
        delete_point_no_bs p1

(* safely delete a point *)
let delete_point n =
    (* if we have a parent line, find it *)
    let rec aux acc =
        if acc = Array.length !lines then -1 else
        let (p0, p1) = !lines.(acc)#endpoints in
        if p0 = n || p1 = n then acc else aux (acc + 1) in
    let target_line = aux 0 in
    (* if we do have a line owner, then just exhaustively delete the lines and
     * let delete_line take care of everything *)
    if target_line <> -1 then begin
        let rec delete_line_loop () =
            let target_line = aux 0 in
            if target_line <> -1 then begin
                let next_target_line =
                    if target_line < Array.length !lines - 1 then
                        aux (target_line + 1) else -1 in
                flush stdout;
                delete_line target_line;
                if next_target_line <> -1 then
                    delete_line_loop ()
            end in
        delete_line_loop ()
    end else delete_point_no_bs n

(* safely deletes all the objects in a map *)
let nuke _ =
    (* this looks unnecessary, but ocaml typing made me do it *)
    let rec obj_loop _ =
        if !objs = [||] then () else begin
            delete_obj 0;
            obj_loop ()
        end in
    obj_loop ()

(* safely deletes all the sides in a map *)
(* TODO: wipe floor/ceiling textures too *)
let pave _ =
    let default_sd =
        match !environment_code with
            |Water  -> (0, 17, 5) |Lava   -> (0, 18, 5) |Sewage -> (0, 19, 5)
            |Jjaro  -> (0, 20, 5) |Pfhor  -> (0, 21, 5) in
    (* first delete all existing sides *)
    let rec trash_sides _ =
        if !sides = [||] then () else begin
            delete_side 0;
            trash_sides ()
        end in
    trash_sides ();
    (* now add in blank sides everywhere *)
    let make_side line line_index this_poly adjacent_poly =
        if this_poly = -1 then -1 else begin
        let side = new MapTypes.side in
        side#set_line_index line_index;
        side#set_polygon_index this_poly;
        side#set_primary_texture ((0, 0), default_sd);
        side#set_secondary_texture ((0, 0), default_sd);
        side#set_kind (
            if adjacent_poly = -1 then MapTypes.Full_Side else
            let tp, ap = !polygons.(this_poly), !polygons.(adjacent_poly) in
            let tpc, apc = tp#ceiling_height, ap#ceiling_height in
            let tpf, apf = tp#floor_height, ap#floor_height in
            let tpp, app = tp#kind = MapTypes.Platform,
                           ap#kind = MapTypes.Platform in
            if (tpc > apc && tpf < apf) ||
               tpp || app then MapTypes.Split_Side else
            if tpc > apc then MapTypes.High_Side else
            if tpf < apf then MapTypes.Low_Side else
            (* if we get here that means this side isn't visible, but for
             * purposes of Lua Visual Mode we want it anyway *)
            MapTypes.High_Side);
        add_side side end in
    array_fold_left_indexed (fun () line idx ->
        let cw_poly, ccw_poly = line#cw_poly_owner, line#ccw_poly_owner in
        line#set_cw_poly_side_index (make_side line idx cw_poly ccw_poly);
        line#set_ccw_poly_side_index (make_side line idx ccw_poly cw_poly))
            () !lines;
    Array.iter (fun x ->
        x#set_floor_texture default_sd;
        x#set_ceiling_texture default_sd) !polygons

(* hee hee *)
let nuke_and_pave _ =
    nuke (); pave ()

let to_strings arr accessor stringizer =
    List.map stringizer
        (nub (List.sort compare (List.map accessor (Array.to_list arr))))

let floor_heights _ =
    to_strings !polygons (fun p -> p#floor_height)            string_of_float
let ceiling_heights _ =
    to_strings !polygons (fun p -> p#ceiling_height)          string_of_float
let floor_lights _ =
    to_strings !lights   (fun l -> find_in_array !lights l)   string_of_int
let ceiling_lights _ =
    to_strings !lights   (fun l -> find_in_array !lights l)   string_of_int
let liquid_lights _ =
    to_strings !lights   (fun l -> find_in_array !lights l)   string_of_int
let liquids _ =
    to_strings !media    (fun m -> find_in_array !media m)    string_of_int
let ambient_sounds _ =
    to_strings !ambients (fun a -> find_in_array !ambients a) string_of_int
let random_sounds _ =
    to_strings !randoms  (fun r -> find_in_array !randoms r)  string_of_int
