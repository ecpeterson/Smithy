(*** GeomEdit.ml contains routines that handle the geometry editing tools ***)

open CamlExt

let select_line_loop x y map =
    (* we use these arrays over and over, might as well give them names *)
    let lines = map#get_lines_array () in
    let lines_length = Array.length lines in
    let points = map#get_points_array () in
    (* get a list of all lines to the left of our mouse click that pass
     * through the range of y values containing our mouse click. *)
    let build_list () =
        (* tests to see if this line is hit by a ray due east of our mouse
         * click.  this works by intersecting that ray with the infinite line
         * corresponding to our line segment, then checking to see if the
         * intersection point lies in the part that parameterizes the segment *)
        let test_line n =
            let line = lines.(n) in
            let (p0, p1) = line#endpoints () in
            let (p0x, p0y) = points.(p0)#vertex () in
            let (p1x, p1y) = points.(p1)#vertex () in
            let u = (float (y - p0y)) /. (float (p1y - p0y)) in
            let x_intersect = p0x + (int_of_float (u *. (float p1x -. (float p0x)))) in
            (* u \in [0, 1] ? *)
            if u >= 0.0 && u <= 1.0 && x_intersect > x then true
            else false in
        (* this is an array filter coupled with an array -> list convertor *)
        let rec fill_list lines_position =
            if lines_position = lines_length then [] else
            if test_line lines_position then begin
                lines_position :: fill_list (lines_position+1)
            end else
                fill_list (lines_position+1) in
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
        (* here's the line that actually does work.  we build the list of all
         * the lines that hit our east-from-click ray, then we sort them by
         * x-coordinate and return the result. *)
        List.sort sort_helper (fill_list 0) in
    (* gets the family of lines connected to a particular point *)
    let rec get_neighbors index p =
        if index = lines_length then [] else
        (* grab a line's endpoints *)
        let (p0, p1) = lines.(index)#endpoints () in
        (* if one matches, include the line, otherwise keep on truckin' *)
        if p0 = p || p1 = p then index :: (get_neighbors (index+1) p) else
            get_neighbors (index+1) p in
    (* same as above, except we can exclude a line (useful in the algorithm) *)
    let get_neighbors_except line p =
        List.filter (fun x -> x != line) (get_neighbors 0 p) in
    (* builds a line loop from a starting line, the core algorithm *)
    let rec build_loop target prev working rec_depth starter =
        (* local utility function *)
        let diff (x0, y0) (x1, y1) = float x0 -. (float x1), float y0 -. (float y1) in
        (* prev_vtx is the vertex we had one step ago, and working_vtx is the
         * vertex we're on now. *)
        let prev_vtx = points.(prev)#vertex () in
        let working_vtx = points.(working)#vertex () in
        (* get the list of neighbors attached to the working vertex *)
        let neighbors = get_neighbors_except starter working in
        (* deal with the base cases of too long a loop or too few neighbors *)
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
        (* thus, tightest will be the next attached vertex which is winding in
         * the correct direction and winds the closest inwards to the line we
         * were just at.  this is how forge, pfhorge, and forgery all do it, so
         * it's good enough for us *)
        let (tightest, dot_value) = get_first_neighbor neighbors in
        (* and pull off the vertex that differs from the working vertex *)
        let (p0, p1) = lines.(tightest)#endpoints () in
        let tightest_pt = if p0 = working then p1 else p0 in
        (* then recurse *)
        match build_loop target working tightest_pt (rec_depth + 1) starter with 
            |None -> None
            |Some a -> Some (tightest :: a) end in
    (* attempt to build a line loop out of each candidate line *)
    let rec array_iter starters =
        (* pull off the first line candidate *)
        match starters with [] -> None | starter :: starters ->
        (* pull and sort its endpoints *)
        let (ep0, ep1) = lines.(starter)#endpoints () in
        let (_, p0y) = points.(ep0)#vertex () in
        let (_, p1y) = points.(ep1)#vertex () in
        let (ep0, ep1) = if p0y < p1y then (ep0, ep1) else (ep1, ep0) in
        (* attempt to build the line loop *)
        try begin match build_loop ep0 ep0 ep1 0 starter with
            |None -> array_iter starters (* try the next candidate *)
            |Some result -> Some (starter :: result) end (* success! *)
        with |_ -> array_iter starters in
    (* dominoes! *)
    array_iter (build_list ())

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
    (* initialize the trivial things *)
    poly#set_vertex_count (List.length line_loop);
    poly#set_line_indices (Array.append (Array.of_list line_loop)
                                (Array.make (8 - List.length line_loop) 0));
    (* takes a line loop and a seed point loop (i.e. which point we want to
     * start from) and returns an ordered point loop *)
    let rec build_point_loop line_loop point_loop =
        match (line_loop, point_loop) with
            (line :: lines, point :: points) ->
                let (p0, p1) = (map#get_lines_array()).(line)#endpoints () in
                build_point_loop lines ((if p0 = point then p1 else p0) :: point_loop)
            |([], _) -> point_loop in
    (* figure out which way we should be winding, since the order of vertices in
     * a particular line is pretty meaningless *)
    let line1 :: line2 :: remaining_lines = line_loop in
    let (p0, p1) = (map#get_lines_array ()).(line1)#endpoints () in
    let (p2, p3) = (map#get_lines_array ()).(line2)#endpoints () in
    let first_point = if p0 = p2 || p0 = p3 then p1 else p0 in
    (* and now build the point loop using the seed we just calculated *)
    let point_loop = List.tl (List.rev (build_point_loop line_loop [first_point])) in
    (* store it to the polygon, /finally/ *)
    poly#set_endpoint_indices (Array.append (Array.of_list point_loop)
                                   (Array.make (8 - List.length point_loop) 0));
    (* push it onto the big poly array *)
    let poly_idx = map#add_polygon poly in
    (* add this poly as a parent to the child lines, note that we don't make a
     * CW/CCW distinction here and i have no idea if that's sound.  i'll put a
     * TODO here just so i see it later *)
    List.iter (fun x ->
        let line = (map#get_lines_array ()).(x) in
        let (cw, ccw) = line#cw_poly_owner(), line#ccw_poly_owner () in
        if cw = -1 then line#set_cw_poly_owner poly_idx else
            line#set_ccw_poly_owner poly_idx;
        if cw != -1 || ccw != -1 then
            line#set_flags [MapTypes.TRANSPARENT]) line_loop

(* dispatch for deleting a highlighted map item *)
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

(* while we're drawing a line, this keeps track of the point we selected at the
 * initial click *)
let start_point = ref 0

(* this gets called on the mouse down event when drawing a line *)
let start_line x y map choose_distance =
    let do_new_point () =
        (* spawn a new point, select it *)
        let point = new MapTypes.point in
        point#set_vertex (int_of_float x, int_of_float y);
        let point_id = map#add_point point in
        start_point := point_id in
    (* are we near a point? *)
    let (point_distance, nearest_point) = map#get_closest_point x y in
    (* how about a line? *)
    let (line_distance, nearest_line) = map#get_closest_line x y in
    (* if we're close enough, just use this point *)
    if point_distance < choose_distance && nearest_point >= 0 then
        start_point := nearest_point
    (* and if we're close enough to the line, just split it and use that
     * resultant point *)
    else if line_distance < choose_distance && nearest_line >= 0 then begin
        let line = (map#get_lines_array ()).(nearest_line) in
        let (cw_poly, ccw_poly) = (line#cw_poly_owner (), line#ccw_poly_owner ()) in
        (* make sure this line isn't owned by a poly! *)
        if cw_poly = -1 && ccw_poly = -1 then begin
            (* do line splitting, pretty straightforward *)
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
        (* if it is owned by a poly, behave like we didn't have a line near *)
        end else do_new_point ()
    (* otherwise spawn a new point in the middle of nowhere, use that one *)
    end else do_new_point ()

(* while we're dragging our line around it would be nice to see it *)
let draw_line x y map gl =
    gl#draw_without_update ();
    let (s, t) = (map#get_points_array ()).(!start_point)#vertex () in
    GlDraw.color GlFlatDraw.line_color;
    GlDraw.begins `lines;
    GlDraw.vertex2 (float s, float t);
    GlDraw.vertex2 (x, y);
    GlDraw.ends ();
    gl#refresh ()

(* called on mouse up when drawing the line *)
let connect_line x y map gl choose_distance =
    (* utility to actually add the line *)
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
    (* utility to add a new point and connect the line up to it *)
    let do_new_point () =
        let point = new MapTypes.point in
        point#set_vertex (int_of_float x, int_of_float y);
        do_line (map#add_point point) in
    (* get the closest point/line to our click *)
    let (point_distance, nearest_point) = map#get_closest_point x y in
    let (line_distance, nearest_line) = map#get_closest_line x y in
    (* if a point is near, connect to it *)
    if point_distance < choose_distance && point_distance >= 0.0 then
        do_line nearest_point
    (* if a line is near, try to split it *)
    else if line_distance < choose_distance && nearest_line >= 0 then begin
        let line = (map#get_lines_array ()).(nearest_line) in
        (* are we attached to a poly? *)
        if (line#cw_poly_owner () = -1 && line#ccw_poly_owner () = -1) then begin
            (* nope, so here we split the line *)
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
        end else (* yup, so we act like it's not there *) do_new_point ()
    end else (* we're near nothing, spawn a new point *) do_new_point ()

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
