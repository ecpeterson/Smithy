(*** GeomEdit.ml contains routines that handle the geometry editing tools ***)

open CamlExt

let select_line_loop x y map =
    let lines = map#get_lines_array () in
    let lines_length = Array.length lines in
    let points = map#get_points_array () in
    (* get a list of all lines to the left of our mouse click that pass
     * through the range of y values containing our mouse click. *)
    let build_array () =
        (* tests to see if this line is hit by a ray due east of our mouse click *)
        let test_line n =
            let line = lines.(n) in
            let (p0, p1) = line#endpoints () in
            let (p0x, p0y) = points.(p0)#vertex () in
            let (p1x, p1y) = points.(p1)#vertex () in
            let u = (float (y - p0y)) /. (float (p1y - p0y)) in
            let x_intersect = p0x + (int_of_float (u *. (float p1x -. (float p0x)))) in
            if u >= 0.0 && u <= 1.0 && x_intersect > x then true
            else false in
        (* returns a count of all candidate lines, so we can build the array *)
        let rec line_count n acc =
            if n = lines_length then acc else
            if test_line n then 
                 line_count (n+1) (acc+1)
            else line_count (n+1)  acc    in
        (* fills an array full of candidates *)
        let rec fill_array arr lines_position array_position =
            if lines_position = lines_length then () else
            if test_line lines_position then begin
                arr.(array_position) <- lines_position;
                fill_array arr (lines_position+1) (array_position+1)
            end else
                fill_array arr (lines_position+1) array_position in
        (* the actual array of poly loop candidates *)
        let starters = Array.create (line_count 0 0) 0 in
        fill_array starters 0 0;
        (* returns the x-coordinate on a line that intercepts the aforementioned
         * ray, used to sort the lines outward *)
        let get_intersection_point line =
            let (p0, p1) = line#endpoints () in
            let (p0x, p0y) = points.(p0)#vertex () in
            let (p0x, p0y) = (float p0x, float p0y) in
            let (p1x, p1y) = points.(p1)#vertex () in
            let (p1x, p1y) = (float p1x, float p1y) in
            (float y -. p0y) *. (p1x -. p0x) /. (p1y -. p0y) +. p0x in
        (* the actual sorting function *)
        let sort_helper l1 l2 =
            let l1 = get_intersection_point lines.(l1) in
            let l2 = get_intersection_point lines.(l2) in
            if l1 < l2 then -1 else if l1 = l2 then 0 else 1 in
        Array.sort sort_helper starters;
        starters in
    (* gets the family of lines connected to a particular point.  note that this
     * function returns a list! *)
    let rec get_neighbors index p =
        if index = lines_length then [] else
        let (p0, p1) = lines.(index)#endpoints () in
        if p0 = p || p1 = p then index :: (get_neighbors (index+1) p) else
            get_neighbors (index+1) p in
    let get_neighbors_except line p =
        List.filter (fun x -> x != line) (get_neighbors 0 p) in
    (* this function builds a line loop from a starting line *)
    let rec build_loop target prev working rec_depth starter =
        let prev_vtx = points.(prev)#vertex () in
        let working_vtx = points.(working)#vertex () in
        let diff (x0, y0) (x1, y1) = float x0 -. (float x1), float y0 -. (float y1) in
        let neighbors = get_neighbors_except starter working in
        (* return base cases *)
        if working = target then Some [] else
        if neighbors = [] || rec_depth > 8 then None else begin
        (* pair each line with a dot product of it against the parent line *)
        let neighbors = List.combine neighbors
            (List.map (fun x ->
                let line = lines.(x) in
                let (p0, p1) = line#endpoints () in
                let p0_vtx = points.(p0)#vertex () in
                let p1_vtx = points.(p1)#vertex () in
                if p0 = prev || p1 = prev then neg_infinity else
                if p0 = working then CamlExt.dotf (diff prev_vtx working_vtx)
                                                  (diff p1_vtx working_vtx) /.
                                        (CamlExt.norm (diff p1_vtx working_vtx))
                                else CamlExt.dotf (diff prev_vtx working_vtx)
                                                  (diff p0_vtx working_vtx) /.
                                        (CamlExt.norm (diff p0_vtx working_vtx)))
            neighbors) in
        (* sort by ascending dot product *)
        let neighbors = List.sort (fun (_, x) (_, y) ->
            if x < y then 1 else if x = y then 0 else -1) neighbors in
        (* then pop units off the top until we see one with appropriate cross
         * product, since that means the poly is winding how we want *)
        let rec get_first_neighbor lst =
            match lst with (line_idx, dot_value) :: xs ->
            let line = lines.(line_idx) in
            let (p0, p1) = line#endpoints () in
            let p0_vtx = points.(p0)#vertex () in
            let p1_vtx = points.(p1)#vertex () in
            let cross = if p0 = working then
                     CamlExt.crossf (diff prev_vtx working_vtx)
                                    (diff p1_vtx working_vtx)
                else CamlExt.crossf (diff prev_vtx working_vtx)
                                    (diff p0_vtx working_vtx) in
            if cross >= 0.0 then (line_idx, dot_value) else get_first_neighbor xs in
        let (tightest, dot_value) = get_first_neighbor neighbors in
        (* and pull off the vertex that differs *)
        let (p0, p1) = lines.(tightest)#endpoints () in
        let tightest_pt = if p0 = working then p1 else p0 in
        (* then recurse *)
        match build_loop target working tightest_pt (rec_depth + 1) starter with 
            |None -> None
            |Some a -> Some (tightest :: a) end in
    (* attempt to build a line loop out of each candidate line *)
    let rec array_iter n starters =
        if n = Array.length starters then None else begin
        let starter = starters.(n) in
        let (ep0, ep1) = lines.(starter)#endpoints () in
        let (_, p0y) = points.(ep0)#vertex () in
        let (_, p1y) = points.(ep1)#vertex () in
        let (ep0, ep1) = if p0y < p1y then (ep0, ep1) else (ep1, ep0) in
        try begin match build_loop ep0 ep0 ep1 0 starter with
            |None -> array_iter (n+1) starters
            |Some result -> Some (starter :: result) end 
        with |_ -> array_iter (n+1) starters end in
    (* dominoes! *)
    array_iter 0 (build_array ())

let fill_poly x y map =
    (* first make sure we're not trying to fill an existing poly *)
    match map#get_enclosing_poly (float x) (float y) with Some a -> () | None ->
    (* next try to get the line loop *)
    match select_line_loop x y map with None -> () | Some line_loop ->
    (* now make sure all the lines have a free side *)
    match List.fold_left (fun x y -> x && (
        let line = (map#get_lines_array ()).(y) in
        line#cw_poly_owner () == -1 || line#ccw_poly_owner () == -1)) true
        line_loop with false -> () | true ->
    (* now build a new polygon *)
    let poly = new MapTypes.polygon in
    poly#set_vertex_count (List.length line_loop);
    poly#set_line_indices (Array.append (Array.of_list line_loop)
                                (Array.make (8 - List.length line_loop) 0));
    let rec build_point_loop line_loop point_loop =
        match (line_loop, point_loop) with
            (line :: lines, point :: points) ->
                let (p0, p1) = (map#get_lines_array()).(line)#endpoints () in
                build_point_loop lines ((if p0 = point then p1 else p0) :: point_loop)
            |([], _) -> point_loop in
    let line1 :: line2 :: remaining_lines = line_loop in
    let (p0, p1) = (map#get_lines_array ()).(line1)#endpoints () in
    let (p2, p3) = (map#get_lines_array ()).(line2)#endpoints () in
    let first_point = if p0 = p2 || p0 = p3 then p1 else p0 in
    let point_loop = List.tl (List.rev (build_point_loop line_loop [first_point])) in
    poly#set_endpoint_indices (Array.append (Array.of_list point_loop)
                                   (Array.make (8 - List.length point_loop) 0));
    (* push it onto the big poly array *)
    let poly_idx = map#add_polygon poly in
    (* add this poly as a parent to the child lines *)
    List.iter (fun x ->
        let line = (map#get_lines_array ()).(x) in
        let (cw, ccw) = line#cw_poly_owner(), line#ccw_poly_owner () in
        if cw = -1 then line#set_cw_poly_owner poly_idx else
            line#set_ccw_poly_owner poly_idx;
        if cw != -1 || ccw != -1 then
            line#set_flags [MapTypes.TRANSPARENT]) line_loop

let delete gl map =
    begin match gl#highlighted () with
        |GlFlatDraw.Point n ->
            List.iter (fun n -> map#delete_point n) n
        |GlFlatDraw.Line n ->
            List.iter (fun n -> map#delete_line n) n
        |GlFlatDraw.Poly n ->
            List.iter (fun n -> map#delete_poly n) n
        |GlFlatDraw.No_Highlight
        |_ -> ()
    end;
    gl#set_highlighted GlFlatDraw.No_Highlight;
    gl#draw ()

let start_point = ref 0

let start_line x y map choose_distance =
    let do_new_point () =
        (* spawn a new point, select it *)
        let point = new MapTypes.point in
        point#set_vertex (int_of_float x, int_of_float y);
        let point_id = map#add_point point in
        start_point := point_id in
    (* are we near a point? *)
    let (point_distance, nearest_point) = map#get_closest_point x y in
    let (line_distance, nearest_line) = map#get_closest_line x y in
    if point_distance < choose_distance && nearest_point >= 0 then
        start_point := nearest_point
    else if line_distance < choose_distance && nearest_line >= 0 then begin
        let line = (map#get_lines_array ()).(nearest_line) in
        let (cw_poly, ccw_poly) = (line#cw_poly_owner (), line#ccw_poly_owner ()) in
        if cw_poly = -1 && ccw_poly = -1 then begin
            (* do line splitting *)
            let (p0, p1) = line#endpoints () in
            map#delete_line nearest_line;
            let point = new MapTypes.point in
            point#set_vertex(int_of_float x, int_of_float y);
            let pi = map#add_point point in
            let line1 = new MapTypes.line in
            line1#set_endpoints (p0, pi);
            let line2 = new MapTypes.line in
            line2#set_endpoints (pi, p1);
            map#add_line line1;
            map#add_line line2;
            start_point := pi
        end else do_new_point ()
    end else do_new_point ()

let draw_line x y map gl =
    gl#draw_without_update ();
    let (s, t) = (map#get_points_array ()).(!start_point)#vertex () in
    GlDraw.color GlFlatDraw.line_color;
    GlDraw.begins `lines;
    GlDraw.vertex2 (float s, float t);
    GlDraw.vertex2 (x, y);
    GlDraw.ends ();
    gl#refresh ()

let connect_line x y map gl choose_distance =
    let do_line target_point =
        let (p0x, p0y) = (map#get_points_array ()).(!start_point)#vertex () in
        let (p1x, p1y) = (map#get_points_array ()).(target_point)#vertex () in
        let length = int_of_float (((float p0x -. (float p1x))**2.0 +.
                                    (float p0y -. (float p1y))**2.0)**0.5) in
        let line = new MapTypes.line in
        line#set_endpoints (!start_point, target_point);
        line#set_length length;
        map#add_line line;
        gl#draw () in
    let do_new_point () =
        let point = new MapTypes.point in
        point#set_vertex (int_of_float x, int_of_float y);
        do_line (map#add_point point) in
    let (point_distance, nearest_point) = map#get_closest_point x y in
    let (line_distance, nearest_line) = map#get_closest_line x y in
    if point_distance < choose_distance && point_distance >= 0.0 then
        do_line nearest_point
    else if line_distance < choose_distance && nearest_line >= 0 then begin
        let line = (map#get_lines_array ()).(nearest_line) in
        if (line#cw_poly_owner () = -1 && line#ccw_poly_owner () = -1) then begin
            (* here we split the line *)
            let (p0, p1) = line#endpoints () in
            map#delete_line nearest_line;
            let point = new MapTypes.point in
            point#set_vertex(int_of_float x, int_of_float y);
            let pi = map#add_point point in
            let line1 = new MapTypes.line in
            line1#set_endpoints (p0, pi);
            let line2 = new MapTypes.line in
            line2#set_endpoints (pi, p1);
            map#add_line line1;
            map#add_line line2;
            do_line pi
    end else do_new_point () end else do_new_point ()

(* TODO: this is not done, lol *)
let merge_points gl map () =
    match gl#highlighted () with
    |GlFlatDraw.Point n -> begin
        (* need to be sure no polys are attached to our pointset *)
        if not (Array.fold_left (fun x y ->
            let y = y#endpoint_indices () in
            Array.fold_left (fun x y ->
                x || List.mem y n) false y || x) false (map#get_polygons_array ()))
        then begin
        let n :: ns = n in
        let rec do_merge ns =
            match ns with [] -> () | new_n :: ns ->
            (* is there a line between these two?  delete it *)
            (* update line endpoints *)
            (* update polygon endpoint array *)
            (* change all new_n to n in map *)
            do_merge ns in
        do_merge ns;
        gl#draw () end
    end |_ -> () (* we can't merge things other than points! *)
