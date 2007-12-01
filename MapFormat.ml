(*** MapFormat.ml contains various bits of constant information about the
 * Marathon map format, along with routines to read and write the information to
 * disk. ***)

open CamlExt
open MapTypes

(* how wide is a map? *)
let map_width = 65536.0
let half_map_width = map_width /. 2.0

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

type environment_code = Water | Lava | Sewage | Jjaro | Pfhor
let environment_descriptor = [Water; Lava; Sewage; Jjaro; Pfhor]
type environment_flag = Vacuum | Magnetic | Rebellion | Low_Grav
let env_flags_descriptor = [1, Vacuum; 2, Magnetic; 4, Rebellion; 8, Low_Grav]
type mission_type = Extermination | Exploration | Retrieval | Repair | Rescue
let mission_descriptor = [1, Extermination; 2, Exploration; 4, Retrieval;
                          8, Repair; 16, Rescue]

let read_chunk fh chunk_length entry_length factory =
    let array = Array.make (chunk_length / entry_length) (factory ()) in
    for i = 0 to (chunk_length / entry_length) - 1 do
        let entry = factory () in
        entry#read fh;
        Array.set array i entry
    done;
    array

let write_chunk fh array entry_length chunk_header =
    let length = Array.length array in
    let start = pos_out fh in
    let chunk_length = length * entry_length in
    if length > 0 then begin
        output_string fh chunk_header; (* header *)
            (* the +16 here is for the length of the header itself *)
        output_dword fh (start + chunk_length - offset_of_first_chunk + 16);
        output_dword fh chunk_length;
        output_padding fh 4; (* "offset", screw that *)
        Array.iter (fun x -> x#write fh) array
    end else ()

(* we wrap it in a class 'cause why not *)
class map = object(self)
    (* information we've loaded from the map *)
    val mutable points = Array.make 0 empty_point
    val mutable lines = Array.make 0 empty_line
    val mutable polygons = Array.make 0 empty_polygon
    val mutable sides = Array.make 0 empty_side
    val mutable lights = Array.make 0 empty_light
    val mutable objs = Array.make 0 empty_obj
    val mutable media = Array.make 0 empty_media
    val mutable placements = Array.make 0 empty_placement
    val mutable platforms = Array.make 0 empty_platform
    val mutable environment_code = Water
    val mutable physics_model = 0
    val mutable song_index = 0
    val mutable mission_flags = []
    val mutable environment_flags = []
    val mutable level_name = String.make 66 '\000'
    val mutable entry_point_flags = 0
    val mutable filename = String.make 0 '\000'

    (* read in various chunks *)
    method private read_points fh length =
        points <- read_chunk fh length point_length (fun () -> new point)
    method private read_lines fh length =
        lines <- read_chunk fh length line_length (fun () -> new line)
    method private read_polys fh length =
        polygons <- read_chunk fh length poly_length (fun () -> new polygon)
    method private read_sides fh length =
        sides <- read_chunk fh length side_length (fun () -> new side)
    method private read_lights fh length =
        lights <- read_chunk fh length light_length (fun () -> new light)
    method private read_objects fh length =
        objs <- read_chunk fh length obj_length (fun () -> new obj)
    method private read_media fh length =
        media <- read_chunk fh length media_length (fun () -> new media)
    method private read_placements fh length =
        placements <- read_chunk fh length placement_length
                                 (fun () -> new placement)
    method private read_platforms fh length =
        platforms <- read_chunk fh length platform_length (fun () -> new platform)

    (* write out various chunks *)
    method private write_points fh =
        write_chunk fh points point_length "PNTS"
    method private write_lines fh =
        write_chunk fh lines line_length "LINS"
    method private write_polys fh =
        write_chunk fh polygons poly_length "POLY"
    method private write_sides fh =
        write_chunk fh sides side_length "SIDS"
    method private write_lights fh =
        write_chunk fh lights light_length "LITE"
    method private write_objects fh =
        write_chunk fh objs obj_length "OBJS"
    method private write_media fh =
        write_chunk fh media media_length "medi"
    method private write_placements fh =
        write_chunk fh placements placement_length "plac"
    method private write_platforms fh =
        write_chunk fh platforms platform_length "plat"

    (* read in a map info chunk *)
    method private read_info fh length =
        environment_code <- of_enum environment_descriptor (input_word fh);
        physics_model <- input_word fh;
        song_index <- input_word fh;
        mission_flags <- of_bitflag mission_descriptor (input_word fh);
        environment_flags <- of_bitflag env_flags_descriptor (input_word fh);
        input_dword fh; input_dword fh; (* skip 8 bytes *)
        really_input fh level_name 0 66;
        entry_point_flags <- input_dword fh

    method private write_info fh =
        let pos = pos_out fh in
        output_string fh "Minf"; (* chunk header *)
        output_dword fh (pos + info_length + 16 - offset_of_first_chunk);
        output_dword fh info_length;
        output_dword fh 0;
        (* now the actual chunk *)
        output_word fh (to_enum environment_descriptor environment_code);
        output_word fh physics_model;
        output_word fh song_index;
        output_word fh (to_bitflag mission_descriptor mission_flags);
        output_word fh (to_bitflag env_flags_descriptor environment_flags);
        output_padding fh 8;
        output_string_n fh level_name 66;
        output_dword fh entry_point_flags

    (* read in a set of chunks from an initialized file pointer *)
    method private read_chunks fh =
        (* read in chunk header *)
        let chunk_start = pos_in fh in
        let chunk_name = String.make 4 '\000' in
        really_input fh chunk_name 0 4;
        let next_offset = input_dword fh in
        let length = input_dword fh in
        let offset = input_dword fh in
        (* match against chunk type *)
        begin match chunk_name with
            |"PNTS" -> self#read_points fh length
            |"LINS" -> self#read_lines fh length
            |"POLY" -> self#read_polys fh length
            |"SIDS" -> self#read_sides fh length
            |"Minf" -> self#read_info fh length
            |"LITE" -> self#read_lights fh length
            |"OBJS" -> self#read_objects fh length
            |"plac" -> self#read_placements fh length
            |"plat" -> self#read_platforms fh length
            |"medi" -> self#read_media fh length
            |_ -> print_endline ("epic fail: " ^ chunk_name)
        end;
        seek_in fh (next_offset + offset_of_first_chunk);
        if next_offset != 0
            then self#read_chunks fh
            else ()

    (* read in an unmerged map file *)
    method read_from_file fname =
        let fh = open_in_bin fname in
        (* read in the map header *)
        let version = input_word fh in
        if version > 4 then raise (Failure "Bad WAD version!") else
        let data_version = input_word fh in
        if data_version > 1 then raise (Failure "Bad data version!") else
        (* forward to the first chunk *)
        seek_in fh offset_of_first_chunk;
        self#read_chunks fh;
        (* close the file *)
        close_in fh;
        filename <- fname

    (* write out to an unmerged map file *)
    method write_to_file filename =
        let fh = open_out filename in
        output_word fh 2; (* output the wad version, forge uses 2 *)
        output_word fh 1; (* output the data version, forge uses 1 *)
        output_string_n fh "TODO filename" 64; (* string containing the short filename *)
        output_dword fh 0; (* checksum *)
        output_dword fh 0; (* temporary directory offset *)
        output_word fh 1; (* wad count *)
        output_word fh 0; (* application-specific directory size *)
        output_word fh 16; (* entry header size *)
        output_word fh 10; (* directory entry base size *)
        output_dword fh 0; (* parent checksum, always 0 *)
        output_padding fh 40; (* twenty bytes wasted *)
        (* now write the actual chunks, order is unimportant *)
        self#write_points fh;
        self#write_lines fh;
        self#write_polys fh;
        self#write_sides fh;
        self#write_lights fh;
        self#write_objects fh;
        self#write_placements fh;
        self#write_platforms fh;
        self#write_media fh;
        (* this let block and the code that follows it must wrap the last chunk
         * to be written out, to make sure we crimp the end of the file.  note:
         * we obviously must guarantee that this chunk gets written to disk,
         * since otherwise we might not crimp anything.  the only chunk we're
         * guaranteed to write is Minf, so it belongs here. *)
        let before_last_chunk = pos_out fh in
        self#write_info fh;
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

    (* allow others to access our state *)
    method get_points_array () = points
    method get_lines_array () = lines
    method get_polygons_array () = polygons
    method get_objs_array () = objs
    method get_media_array () = media
    method get_filename () = filename

    (* allow others to add objects *)
    method add_point point =
        let append_array = Array.make 1 point in
        points <- Array.append points append_array;
        Array.length points - 1

    method add_line line =
        let append_array = Array.make 1 line in
        lines <- Array.append lines append_array;
        Array.length lines - 1

    method add_polygon poly =
        let append_array = Array.make 1 poly in
        polygons <- Array.append polygons append_array;
        Array.length polygons - 1

    method add_media m =
        let append_array = Array.make 1 m in
        media <- Array.append media append_array;
        Array.length media - 1

    (* geometry selection functions *)
    method get_closest_object x0 y0 =
        let distance (x0, y0) (x1, y1) =
            ((x0 -. x1)**2.0 +. (y0 -. y1)**2.0)**0.5 in
        let rec g_c_o_aux x0 y0 i accd acci =
            if i = Array.length objs then (accd, acci) else
            let this_obj = Array.get objs i in
            let (xi, yi, _) = this_obj#point () in
            let this_distance = distance (float xi, float yi) (x0, y0) in
            if this_distance < accd then
                g_c_o_aux x0 y0 (i+1) this_distance i
            else
                g_c_o_aux x0 y0 (i+1) accd acci in
        g_c_o_aux x0 y0 0 65536.0 (-1)

    method get_closest_point x0 y0 =
        let distance (x0, y0) (x1, y1) =
            ((x0 -. x1)**2.0 +. (y0 -. y1)**2.0)**0.5 in
        let rec g_c_p_aux x0 y0 i accd acci =
            if i = Array.length points then (accd, acci) else
            let this_point = Array.get points i in
            match this_point#vertex () with (xi, yi) ->
            let this_distance = distance (float xi, float yi) (x0, y0) in
            if this_distance < accd then
                g_c_p_aux x0 y0 (i+1) this_distance i
            else
                g_c_p_aux x0 y0 (i+1) accd acci in
        g_c_p_aux x0 y0 0 65536.0 (-1)

    method get_closest_line x0 y0 =
        let point_distance (x0, y0) (x1, y1) =
            ((x0 -. x1)**2.0 +. (y0 -. y1)**2.0)**0.5 in
        let distance (e0x, e0y) (e1x, e1y) (x, y) =
            let u = (((x -. e0x) *. (e1x -. e0x)) +. ((y -. e0y) *. (e1y -. e0y)))
                    /. ((e1x -. e0x)**2.0 +. (e1y -. e0y)**2.0) in
            if u > 1.0 then point_distance (x, y) (e1x, e1y) else
            if u < 0.0 then point_distance (x, y) (e0x, e0y) else
            let ix = e0x +. u *. (e1x -. e0x) in
            let iy = e0y +. u *. (e1y -. e0y) in
            point_distance (x, y) (ix, iy) in
        let rec g_c_l_aux x0 y0 i accd acci =
            if i = Array.length lines then (accd, acci) else
            let this_line = Array.get lines i in
            let (p0i, p1i) = this_line#endpoints () in
            let (p0x, p0y) = (Array.get points p0i)#vertex () in
            let (p1x, p1y) = (Array.get points p1i)#vertex () in
            let d = distance (float p0x, float p0y) (float p1x, float p1y) (x0, y0) in
            if d < accd then
                g_c_l_aux x0 y0 (i+1) d i
            else
                g_c_l_aux x0 y0 (i+1) accd acci in
        g_c_l_aux x0 y0 0 65536.0 (-1)

    method get_enclosing_poly x0 y0 =
        let pi = asin 1.0 *. 2.0 in
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
        let rec sum_angles points (x0, y0) i acc =
            if i = List.length points then acc else
            let next = (if i = (List.length points) - 1 then 0 else i + 1) in
            let (p0x, p0y) = List.nth points i in
            let (p1x, p1y) = List.nth points next in
            let p0 = (float p0x -. x0, float p0y -. y0) in
            let p1 = (float p1x -. x0, float p1y -. y0) in
            sum_angles points (x0, y0) (i+1) (angle p0 p1 +. acc) in
        let rec g_e_p_aux x0 y0 i =
            if i = Array.length polygons then None else
            let poly = Array.get polygons i in
            let poly_ring = self#get_poly_ring poly in
            let poly_points = List.map (fun x -> (Array.get points x)#vertex ()) poly_ring in
            let sum = sum_angles poly_points (x0, y0) 0 0.0 in
            if abs_float sum < pi then
                g_e_p_aux x0 y0 (i+1)
            else
                Some i in
        g_e_p_aux (x0 +. 1.0) (y0 +. 1.0) 0

    method get_poly_ring (poly : polygon) =
        let endpoints = Array.map
            (fun x -> if x >= Array.length lines then dprint (string_of_int x);
                try (Array.get lines x)#endpoints () with _ -> (0, 0))
            (poly#line_indices ()) in
        let rec loop n acc =
            if n = poly#vertex_count () then acc else
            let (e0, e1) = Array.get endpoints n in
            if n = 0 then begin
                let (e2, e3) = Array.get endpoints 1 in
                if e0 = e2 || e0 = e3 then
                    loop 1 [e0]
                else
                    loop 1 [e1]
            end else if e0 = List.hd acc then
                loop (n+1) (e1 :: acc)
            else
                loop (n+1) (e0 :: acc) in
        loop 0 []

    method recalculate_lengths point =
        let line_length = Array.length lines in
        let length (x0, y0) (x1, y1) =
            int_of_float (((float x0 -. (float x1))**2.0 +.
                           (float y0 -. (float y1))**2.0)**0.5) in
        let rec line_loop n =
            if n = line_length then () else
            let line = Array.get lines n in
            let (p0, p1) = line#endpoints () in
            if p0 = point || p1 = point then
                let p0 = (Array.get points p0)#vertex () in
                let p1 = (Array.get points p1)#vertex () in
                line#set_length (length p0 p1);
            line_loop (n+1) in
        line_loop 0

    method delete_side n =
        let side = Array.get sides n in
        let parent_poly = Array.get polygons (side#polygon_index ()) in
        let parent_line = Array.get lines (side#line_index ()) in
        let poly_side_array = parent_poly#side_indices () in
        (* get rid of this side from the parent array, filling in the empty end
         * slot with a -1 value, signalling emptiness *)
        delete_from_array poly_side_array n (-1);
        (* now clean up the parent line similarly *)
        if parent_line#cw_poly_side_index () = n then
            parent_line#set_cw_poly_side_index (-1)
        else if parent_line#ccw_poly_side_index () = n then
            parent_line#set_ccw_poly_side_index (-1)
        else dprint "How confusing!";
        (* iterate through all the available polys, shifting the indices into
         * the sides array down as needed *)
        Array.iter (fun poly -> destructive_map
                (fun x -> if x > n then x - 1 else x)
                (poly#side_indices ())) polygons

    method delete_poly n =
        let poly = Array.get polygons n in
        (*let lsides = poly#side_indices () in*)
         (*deletes a whole sides array *)
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
         (*delete this poly's sides array *)
        (*trash_sides 0;*)
        (* delete this polygon from the polgons array *)
        polygons <- delete_from_array_and_resize polygons n;
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
            lines;
        (* shift down all the side poly own indices *)
        Array.iter (fun side ->
            let pi = side#polygon_index () in
            if pi > n then side#set_polygon_index (pi - 1)) sides

    method delete_line n =
        let line = Array.get lines n in
        (* if our line is attached to polygons, delete them *)
        let poly0 = line#cw_poly_owner () in
        let poly1 = line#ccw_poly_owner () in
        let (poly0, poly1) = (max poly0 poly1, min poly0 poly1) in
        if poly0 != -1 then self#delete_poly poly0;
        if poly1 != -1 then self#delete_poly poly1;
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
        lines <- delete_from_array_and_resize lines n;
        (* loop through the other polys, fix their line indices *)
        Array.iter (fun x ->
                let lines = x#line_indices () in
                destructive_map (fun x -> if x > n then x - 1 else x) lines)
            polygons;
        (* loop through sides, fix their line owner indices *)
        Array.iter (fun x ->
                let li = x#line_index () in
                x#set_line_index (if li > n then li - 1 else li)) sides

    method delete_point n =
        let lines_length = Array.length lines in
        (* if we have a parent line, find it *)
        let rec aux acc =
            if acc = lines_length then -1 else
            let (p0, p1) = (Array.get lines acc)#endpoints () in
            if p0 = n || p1 = n then acc else aux (acc + 1) in
        let target_line = aux 0 in
        if target_line != -1 then begin
            (* if we do in fact have a parent line, then we need to delete it
             * and then try again *)
            self#delete_line target_line;
            self#delete_point n
        end else begin
            (* we don't have a parent, so let's delete the point itself.  start
             * by removing it from the points array *)
            points <- delete_from_array_and_resize points n;
            (* fix the endpoints of lines *)
            Array.iter (fun x ->
                let (p0, p1) = x#endpoints () in
                let p0 = if p0 > n then p0 - 1 else p0 in
                let p1 = if p1 > n then p1 - 1 else p1 in
                x#set_endpoints (p0, p1)) lines;
            (* fix the point arrays in polygons *)
            Array.iter (fun x ->
                destructive_map (fun x -> if x > n then x - 1 else x)
                    (x#endpoint_indices ())) polygons
        end

    method delete_obj n =
        objs <- delete_from_array_and_resize objs n;
        Array.iter (fun x ->
            let y = x#first_object () in
            if y > n then x#set_first_object (y-1) else
            if y = n then x#set_first_object (-1)) polygons
end
