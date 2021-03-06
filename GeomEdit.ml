(*** GeomEdit.ml contains lengthy algorithms that correspond to geometric
 * manipulations of map data. ***)
open CamlExt

let new_point = ref None
(* this gets called on the mouse down event when drawing a line *)
let start_line x y choose_distance =
    let do_new_point () =
        (* spawn a new point, select it *)
        let point = new MapTypes.point in
        point#set_vertex (x, y);
        let pi = MapFormat.add_point point in
        new_point := Some pi;
        pi in
    (* are we near a point? *)
    let (point_distance, nearest_point) = MapFormat.get_closest_point x y in
    (* how about a line? *)
    let (line_distance, nearest_line) = MapFormat.get_closest_line x y in
    (* if we're close enough, just use this point *)
    if point_distance < choose_distance && nearest_point >= 0 then
        nearest_point
    (* and if we're close enough to the line, just split it and use that
     * resultant point *)
    else if line_distance < choose_distance && nearest_line >= 0 then begin
        let line = !MapFormat.lines.(nearest_line) in
        let cw_poly, ccw_poly = line#cw_poly_owner, line#ccw_poly_owner in
        (* make sure this line isn't owned by a poly! *)
        if cw_poly = -1 && ccw_poly = -1 then begin
            (* do line splitting, pretty straightforward *)
            let (p0, p1) = line#endpoints in
            MapFormat.delete_line_no_bs nearest_line;
            let point = new MapTypes.point in
            point#set_vertex (x, y);
            let pi = MapFormat.add_point point in
            let line1 = new MapTypes.line in
            line1#set_endpoints (p0, pi);
            let line2 = new MapTypes.line in
            line2#set_endpoints (pi, p1);
            ignore (MapFormat.add_line line1);
            ignore (MapFormat.add_line line2);
            pi
        (* if it is owned by a poly, behave like we didn't have a line near *)
        end else do_new_point ()
    (* otherwise spawn a new point in the middle of nowhere, use that one *)
    end else do_new_point ()

(* called on mouse up when drawing the line *)
let connect_line start_point x y choose_distance =
    (* utility to actually add the line *)
    let do_line target_point =
        if start_point = target_point then () else begin
(*            match !new_point with (* Why was this here?  What nonsense! *)
            |Some pi ->
                MapFormat.delete_point pi;
                new_point := None;
                ()
            |None -> ()
        else begin *)
        let line = new MapTypes.line in
        line#set_endpoints (start_point, target_point);
        line#set_length (distance !MapFormat.points.(start_point)#vertex
                                  !MapFormat.points.(target_point)#vertex);
        MapFormat.add_line line;
        () end in
    (* utility to add a new point and connect the line up to it *)
    let do_new_point () =
        let point = new MapTypes.point in
        point#set_vertex (x, y);
        do_line (MapFormat.add_point point) in
    (* get the closest point/line to our click *)
    let (point_distance, nearest_point) = MapFormat.get_closest_point x y in
    let (line_distance, nearest_line) = MapFormat.get_closest_line x y in
    (* if a point is near, connect to it *)
    if point_distance < choose_distance && point_distance >= 0.0 then
        do_line nearest_point
    (* if a line is near, try to split it *)
    else if line_distance < choose_distance && nearest_line >= 0 then begin
        let line = !MapFormat.lines.(nearest_line) in
        (* are we attached to a poly? *)
        if (line#cw_poly_owner = -1 && line#ccw_poly_owner = -1) then begin
            (* nope, so here we split the line *)
            let (p0, p1) = line#endpoints in
            MapFormat.delete_line_no_bs nearest_line;
            let point = new MapTypes.point in
            point#set_vertex (x, y);
            let pi = MapFormat.add_point point in
            let line1 = new MapTypes.line in
            line1#set_endpoints (p0, pi);
            let line2 = new MapTypes.line in
            line2#set_endpoints (pi, p1);
            ignore (MapFormat.add_line line1);
            ignore (MapFormat.add_line line2);
            do_line pi
        end else (* yup, so we act like it's not there *) do_new_point ()
    end else (* we're near nothing, spawn a new point *) do_new_point ()

let select_line_loop x y =
    (* we use these arrays over and over, might as well give them names *)
    let lines_length = Array.length !MapFormat.lines in
    (* get a list of all lines to the left of our mouse click that pass
     * through the range of y values containing our mouse click. *)
    let build_list () =
        (* tests to see if this line is hit by a ray due east of our mouse
         * click.  this works by intersecting that ray with the infinite line
         * corresponding to our line segment, then checking to see if the
         * intersection point lies in the part that parameterizes the segment *)
        let test_line n =
            let line = !MapFormat.lines.(n) in
            let (p0, p1) = line#endpoints in
            let (p0x, p0y) = !MapFormat.points.(p0)#vertex in
            let (p1x, p1y) = !MapFormat.points.(p1)#vertex in
            let u = (y -. p0y) /. (p1y -. p0y) in
            let x_intersect = p0x +. (u *. (p1x -. p0x)) in
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
            let (p0, p1) = line#endpoints in
            let (p0x, p0y) = !MapFormat.points.(p0)#vertex in
            let (p1x, p1y) = !MapFormat.points.(p1)#vertex in
            (y -. p0y) *. (p1x -. p0x) /. (p1y -. p0y) +. p0x in
        (* the actual sorting function *)
        let sort_helper l1 l2 =
            let l1 = get_intersection_point !MapFormat.lines.(l1) in
            let l2 = get_intersection_point !MapFormat.lines.(l2) in
            if l1 < l2 then -1 else if l1 = l2 then 0 else 1 in
        (* here's the line that actually does work.  we build the list of all
         * the lines that hit our east-from-click ray, then we sort them by
         * x-coordinate and return the result. *)
        List.sort sort_helper (fill_list 0) in
    (* gets the family of lines connected to a particular point *)
    let rec get_neighbors index p =
        if index = lines_length then [] else
        (* grab a line's endpoints *)
        let (p0, p1) = !MapFormat.lines.(index)#endpoints in
        (* if one matches, include the line, otherwise keep on truckin' *)
        if p0 = p || p1 = p then index :: (get_neighbors (index+1) p) else
            get_neighbors (index+1) p in
    (* same as above, except we can exclude a line (useful in the algorithm) *)
    let get_neighbors_except line p =
        List.filter (fun x -> x <> line) (get_neighbors 0 p) in
    (* builds a line loop from a starting line, the core algorithm *)
    let rec build_loop target prev working rec_depth starter =
        (* local utility function *)
        let diff (x0, y0) (x1, y1) = x0 -. x1, y0 -. y1 in
        (* prev_vtx is the vertex we had one step ago, and working_vtx is the
         * vertex we're on now. *)
        let prev_vtx = !MapFormat.points.(prev)#vertex in
        let working_vtx = !MapFormat.points.(working)#vertex in
        (* get the list of neighbors attached to the working vertex *)
        let neighbors = get_neighbors_except starter working in
        (* deal with the base cases of too long a loop or too few neighbors *)
        if working = target then Some [] else
        if neighbors = [] || rec_depth > 8 then None else begin
        (* pair each line with a dot product of it against the parent line *)
        let neighbors = List.combine neighbors
            (List.map (fun x ->
                let line = !MapFormat.lines.(x) in
                let (p0, p1) = line#endpoints in
                let p0_vtx = !MapFormat.points.(p0)#vertex in
                let p1_vtx = !MapFormat.points.(p1)#vertex in
                if p0 = prev || p1 = prev then neg_infinity else
                if p0 = working then dotf (diff prev_vtx working_vtx)
                                          (diff p1_vtx working_vtx) /.
                                        (norm (diff p1_vtx working_vtx))
                                else dotf (diff prev_vtx working_vtx)
                                          (diff p0_vtx working_vtx) /.
                                        (norm (diff p0_vtx working_vtx)))
            neighbors) in
        (* sort by ascending dot product *)
        let neighbors = List.sort (fun (_, x) (_, y) ->
            if x < y then 1 else if x = y then 0 else -1) neighbors in
        (* then pop units off the top until we see one with appropriate cross
         * product, since that means the poly is winding how we want *)
        let rec get_first_neighbor lst =
            match lst with
            |(line_idx, dot_value) :: xs ->
                let line = !MapFormat.lines.(line_idx) in
                let (p0, p1) = line#endpoints in
                let p0_vtx = !MapFormat.points.(p0)#vertex in
                let p1_vtx = !MapFormat.points.(p1)#vertex in
                let cross = if p0 = working then
                        crossf (diff prev_vtx working_vtx)
                               (diff p1_vtx working_vtx)
                    else crossf (diff prev_vtx working_vtx)
                                (diff p0_vtx working_vtx) in
                if cross >= 0.0 then (line_idx, dot_value)
                else get_first_neighbor xs
            |[] -> raise (Failure "empty line list") in
        (* thus, tightest will be the next attached vertex which is winding in
         * the correct direction and winds the closest inwards to the line we
         * were just at.  this is how forge, pfhorge, and forgery all do it, so
         * it's good enough for us *)
        let (tightest, dot_value) = get_first_neighbor neighbors in
        (* and pull off the vertex that differs from the working vertex *)
        let (p0, p1) = !MapFormat.lines.(tightest)#endpoints in
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
        let (ep0, ep1) = !MapFormat.lines.(starter)#endpoints in
        let (_, p0y) = !MapFormat.points.(ep0)#vertex in
        let (_, p1y) = !MapFormat.points.(ep1)#vertex in
        let (ep0, ep1) = if p0y < p1y then (ep0, ep1) else (ep1, ep0) in
        (* attempt to build the line loop *)
        try begin match build_loop ep0 ep0 ep1 0 starter with
            |None -> array_iter starters (* try the next candidate *)
            |Some result ->
                (* XXX: THIS IS A LINE INDEX LOOP *)
                let loop = starter :: result in
                let point_loop = MapFormat.get_point_ring_from_line_ring
                    (Array.of_list loop) (List.length loop) in
                let vertex_loop = List.map (fun x ->
                    !MapFormat.points.(x)#vertex) point_loop in
                if MapFormat.point_loop_encloses_point (x, y) vertex_loop
                    then Some (starter :: result)
                    else array_iter starters end
        with |_ -> array_iter starters in
    (* dominoes! *)
    array_iter (build_list ())

let fill_poly x y =
    (* first make sure we're not trying to fill an existing poly *)
    match MapFormat.get_enclosing_poly x y with Some a -> ()
                                              | None   ->
    (* next try to get the line loop *)
    match select_line_loop x y with None -> () | Some line_loop ->
    (* now make sure all the lines have a free side *)
    match List.fold_left (fun x y -> x && (
        let line = !MapFormat.lines.(y) in
        line#cw_poly_owner == -1 || line#ccw_poly_owner == -1)) true
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
            |(line :: lines, point :: points) ->
                let (p0, p1) = !MapFormat.lines.(line)#endpoints in
                build_point_loop lines ((if p0 = point then p1 else p0) :: point_loop)
            |([], _) -> point_loop
            |(_, []) -> raise (Failure "no point loop given") in
    (* figure out which way we should be winding, since the order of vertices in
     * a particular line is pretty meaningless *)
    let line1, line2, remaining_lines = match line_loop with
    |x :: y :: xs -> x, y, xs
    |_ -> raise (Failure "not enough lines remaining") in
    let (p0, p1) = !MapFormat.lines.(line1)#endpoints in
    let (p2, p3) = !MapFormat.lines.(line2)#endpoints in
    let first_point = if p0 = p2 || p0 = p3 then p1 else p0 in
    (* and now build the point loop using the seed we just calculated *)
    let point_loop = List.tl (List.rev (build_point_loop line_loop [first_point])) in
    (* store it to the polygon, /finally/ *)
    poly#set_endpoint_indices (Array.append (Array.of_list point_loop)
                                   (Array.make (8 - List.length point_loop) 0));
    (* push it onto the big poly array *)
    let poly_idx = MapFormat.add_polygon poly in
    (* now we attach this polygon as a line parent.  if the line endpoints
     * are of the same order as they polygon's vertex loop (which is stored
     * in CW order), then we attach to the CW parent, otherwise to the CCW
     * parent.  we also automatically set the transparent flag if we share
     * this line with some other polygon. *)
    List.iter2 (fun x y ->
        let line = !MapFormat.lines.(x) in
        let (p0, p1) = line#endpoints in
        if p1 = y then line#set_cw_poly_owner poly_idx else
                       line#set_ccw_poly_owner poly_idx;
        if line#cw_poly_owner <> -1 && line#ccw_poly_owner <> -1 then
            line#set_flags [MapTypes.TRANSPARENT]) line_loop point_loop

let decrement_obj obj =
    begin match obj#kind with
        |MapTypes.Monster ->
            (* update plac + (MapFormat.number_of_placements / 2) *)
            let plac = !MapFormat.placements.(obj#index +
                                    MapFormat.number_of_placements / 2) in
            if plac#initial_count > 0 then
                plac#set_initial_count (plac#initial_count - 1)
        |MapTypes.Item ->
            (* update plac *)
            let plac = !MapFormat.placements.(obj#index) in
            if plac#initial_count > 0 then
                plac#set_initial_count (plac#initial_count - 1)
        |_ -> ()
    end

let increment_obj obj =
    begin match obj#kind with
        |MapTypes.Monster ->
            (* update plac + (MapFormat.number_of_placements / 2) *)
            let plac = !MapFormat.placements.(obj#index +
                                    MapFormat.number_of_placements / 2) in
            plac#set_initial_count (plac#initial_count + 1)
        |MapTypes.Item ->
            (* update plac *)
            let plac = !MapFormat.placements.(obj#index) in
            plac#set_initial_count (plac#initial_count + 1)
        |_ -> ()
    end

let clone_idx = ref 0
let make_object x y poly =
    let obj = new MapTypes.obj in
    begin try let clone_obj = !MapFormat.objs.(!clone_idx) in
        obj#set_kind clone_obj#kind;
        obj#set_index clone_obj#index;
        obj#set_facing clone_obj#facing;
        obj#set_flags clone_obj#flags
    with _ -> () end;
    increment_obj obj;
    obj#set_polygon poly;
    obj#set_point (float x, float y, 0.0);
    (* do we need to update the container polygon's information? *)
    clone_idx := MapFormat.add_object obj;
    !clone_idx

let make_annotation x y poly =
    let anno = new MapTypes.annotation in
    anno#set_text "Unknown";
    anno#set_location (x, y);
    anno#set_polygon_index poly;
    MapFormat.add_annotation anno

let merge_points ns () =
    (* need to be sure no polys are attached to our pointset *)
    if not (Array.fold_left (fun x y ->
                    let y = y#endpoint_indices in
                    Array.fold_left (fun x y ->
                        x || List.mem y ns) false y || x)
                false !MapFormat.polygons)
    then begin
    (* sort ascending and pull the first one *)
    let n, ns = match List.sort compare ns with
    |n :: ns -> n, ns
    |[] -> raise (Failure "no points to merge") in
    (* sort the remainder descending *)
    let ns = List.sort (fun x y -> -(compare x y)) ns in
    (* suck the points in one pair at a time *)
    let rec do_merge ns =
        match ns with [] -> () | new_n :: ns ->
        (* is there a line between these two?  delete it *)
        let line = array_find
            (fun x -> x#endpoints = (n, new_n) ||
                      x#endpoints = (new_n, n)) !MapFormat.lines in
        if line <> Array.length !MapFormat.lines then
            MapFormat.delete_line line;
        (** change all new_n to n in map **)
        (* update line endpoints *)
        Array.iter (fun x ->
            let (p0, p1) = x#endpoints in
            let p0 =
                if p0 > new_n then p0 - 1 else
                if p0 = new_n then n else p0 in
            let p1 =
                if p1 > new_n then p1 - 1 else
                if p1 = new_n then n else p1 in
            x#set_endpoints (p0, p1)) !MapFormat.lines;
        (* update polygon endpoint array *)
        Array.iter (fun x ->
            destructive_map (fun y ->
                    if y = new_n then n else if y > new_n then y - 1 else y)
                x#endpoint_indices) !MapFormat.polygons;
        (* actually delete the point *)
        MapFormat.points :=
            delete_from_array_and_resize !MapFormat.points new_n;
        do_merge ns in
    do_merge ns end


let point_center arr =
    let (x, y) =
        Array.fold_left (fun (x, y) p ->
            let xn, yn = !MapFormat.points.(p)#vertex in
            x +. xn, y +. yn) (0., 0.) arr in
    x /. (float (Array.length arr)), y /. (float (Array.length arr))
