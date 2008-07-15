(*** GeomEdit.ml contains lengthy algorithms that correspond to geometric
 * manipulations of map data. ***)

let point_filter (x, y) =
    if !DrawModeSettings.constrain_to_grid then begin
        let granularity= 1024 / (CamlExt.pow 2 !DrawModeSettings.grid_factor) in
        (CamlExt.round (float x /. (float granularity)) * granularity,
         CamlExt.round (float y /. (float granularity)) * granularity)
    end else (x, y)

(* this gets called on the mouse down event when drawing a line *)
let start_line x y choose_distance =
    let do_new_point () =
        (* spawn a new point, select it *)
        let point = new MapTypes.point in
        let (px, py) = point_filter (int_of_float x, int_of_float y) in
        point#set_vertex (px, py);
        MapFormat.add_point point in
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
        let (cw_poly, ccw_poly) = (line#cw_poly_owner (), line#ccw_poly_owner ()) in
        (* make sure this line isn't owned by a poly! *)
        if cw_poly = -1 && ccw_poly = -1 then begin
            (* do line splitting, pretty straightforward *)
            let (p0, p1) = line#endpoints () in
            MapFormat.delete_line nearest_line;
            let point = new MapTypes.point in
            let (px, py) = point_filter (int_of_float x, int_of_float y) in
            point#set_vertex (px, py);
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
        let (p0x, p0y) = !MapFormat.points.(start_point)#vertex () in
        let (p1x, p1y) = !MapFormat.points.(target_point)#vertex () in
        let length = int_of_float (((float p0x -. (float p1x))**2.0 +.
                                    (float p0y -. (float p1y))**2.0)**0.5) in
        let line = new MapTypes.line in
        line#set_endpoints (start_point, target_point);
        line#set_length length;
        MapFormat.add_line line in
    (* utility to add a new point and connect the line up to it *)
    let do_new_point () =
        let point = new MapTypes.point in
        let (px, py) = point_filter (int_of_float x, int_of_float y) in
        point#set_vertex (px, py);
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
        if (line#cw_poly_owner () = -1 && line#ccw_poly_owner () = -1) then begin
            (* nope, so here we split the line *)
            let (p0, p1) = line#endpoints () in
            MapFormat.delete_line nearest_line;
            let point = new MapTypes.point in
            let (px, py) = point_filter (int_of_float x, int_of_float y) in
            point#set_vertex (px, py);
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
            let (p0, p1) = line#endpoints () in
            let (p0x, p0y) = !MapFormat.points.(p0)#vertex () in
            let (p1x, p1y) = !MapFormat.points.(p1)#vertex () in
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
            let (p0x, p0y) = !MapFormat.points.(p0)#vertex () in
            let (p0x, p0y) = (float p0x, float p0y) in
            let (p1x, p1y) = !MapFormat.points.(p1)#vertex () in
            let (p1x, p1y) = (float p1x, float p1y) in
            (float y -. p0y) *. (p1x -. p0x) /. (p1y -. p0y) +. p0x in
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
        let (p0, p1) = !MapFormat.lines.(index)#endpoints () in
        (* if one matches, include the line, otherwise keep on truckin' *)
        if p0 = p || p1 = p then index :: (get_neighbors (index+1) p) else
            get_neighbors (index+1) p in
    (* same as above, except we can exclude a line (useful in the algorithm) *)
    let get_neighbors_except line p =
        List.filter (fun x -> x <> line) (get_neighbors 0 p) in
    (* builds a line loop from a starting line, the core algorithm *)
    let rec build_loop target prev working rec_depth starter =
        (* local utility function *)
        let diff (x0, y0) (x1, y1) = float x0 -. (float x1), float y0 -. (float y1) in
        (* prev_vtx is the vertex we had one step ago, and working_vtx is the
         * vertex we're on now. *)
        let prev_vtx = !MapFormat.points.(prev)#vertex () in
        let working_vtx = !MapFormat.points.(working)#vertex () in
        (* get the list of neighbors attached to the working vertex *)
        let neighbors = get_neighbors_except starter working in
        (* deal with the base cases of too long a loop or too few neighbors *)
        if working = target then Some [] else
        if neighbors = [] || rec_depth > 8 then None else begin
        (* pair each line with a dot product of it against the parent line *)
        let neighbors = List.combine neighbors
            (List.map (fun x ->
                let line = !MapFormat.lines.(x) in
                let (p0, p1) = line#endpoints () in
                let p0_vtx = !MapFormat.points.(p0)#vertex () in
                let p1_vtx = !MapFormat.points.(p1)#vertex () in
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
            let line = !MapFormat.lines.(line_idx) in
            let (p0, p1) = line#endpoints () in
            let p0_vtx = !MapFormat.points.(p0)#vertex () in
            let p1_vtx = !MapFormat.points.(p1)#vertex () in
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
        let (p0, p1) = !MapFormat.lines.(tightest)#endpoints () in
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
        let (ep0, ep1) = !MapFormat.lines.(starter)#endpoints () in
        let (_, p0y) = !MapFormat.points.(ep0)#vertex () in
        let (_, p1y) = !MapFormat.points.(ep1)#vertex () in
        let (ep0, ep1) = if p0y < p1y then (ep0, ep1) else (ep1, ep0) in
        (* attempt to build the line loop *)
        try begin match build_loop ep0 ep0 ep1 0 starter with
            |None -> array_iter starters (* try the next candidate *)
            |Some result -> Some (starter :: result) end (* success! *)
        with |_ -> array_iter starters in
    (* dominoes! *)
    array_iter (build_list ())

let fill_poly x y =
    (* first make sure we're not trying to fill an existing poly *)
    match MapFormat.get_enclosing_poly (float x) (float y) with Some a -> ()
                                                              | None   ->
    (* next try to get the line loop *)
    match select_line_loop x y with None -> () | Some line_loop ->
    (* now make sure all the lines have a free side *)
    match List.fold_left (fun x y -> x && (
        let line = !MapFormat.lines.(y) in
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
                let (p0, p1) = !MapFormat.lines.(line)#endpoints () in
                build_point_loop lines ((if p0 = point then p1 else p0) :: point_loop)
            |([], _) -> point_loop in
    (* figure out which way we should be winding, since the order of vertices in
     * a particular line is pretty meaningless *)
    let line1 :: line2 :: remaining_lines = line_loop in
    let (p0, p1) = !MapFormat.lines.(line1)#endpoints () in
    let (p2, p3) = !MapFormat.lines.(line2)#endpoints () in
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
        let (p0, p1) = line#endpoints () in
        if p1 = y then line#set_cw_poly_owner poly_idx else
                       line#set_ccw_poly_owner poly_idx;
        if line#cw_poly_owner () <> -1 && line#ccw_poly_owner () <> -1 then
            line#set_flags [MapTypes.TRANSPARENT]) line_loop point_loop

let decrement_obj obj =
    begin match obj#kind () with
        |MapTypes.Monster ->
            (* update plac + (MapFormat.number_of_placements / 2) *)
            let plac = !MapFormat.placements.(obj#index () +
                                    MapFormat.number_of_placements / 2) in
            if plac#initial_count () > 0 then
                plac#set_initial_count (plac#initial_count () - 1)
        |MapTypes.Item ->
            (* update plac *)
            let plac = !MapFormat.placements.(obj#index ()) in
            if plac#initial_count () > 0 then
                plac#set_initial_count (plac#initial_count () - 1)
        |_ -> ()
    end

let increment_obj obj =
    begin match obj#kind () with
        |MapTypes.Monster ->
            (* update plac + (MapFormat.number_of_placements / 2) *)
            let plac = !MapFormat.placements.(obj#index () +
                                    MapFormat.number_of_placements / 2) in
            plac#set_initial_count (plac#initial_count () + 1)
        |MapTypes.Item ->
            (* update plac *)
            let plac = !MapFormat.placements.(obj#index ()) in
            plac#set_initial_count (plac#initial_count () + 1)
        |_ -> ()
    end

let clone_idx = ref 0
let make_object x y poly =
    let obj = new MapTypes.obj in
    begin try let clone_obj = !MapFormat.objs.(!clone_idx) in
        obj#set_kind (clone_obj#kind ());
        obj#set_index (clone_obj#index ());
        obj#set_facing (clone_obj#facing ());
        obj#set_flags (clone_obj#flags ())
    with _ -> () end;
    increment_obj obj;
    obj#set_polygon poly;
    obj#set_point (x, y, 0);
    (* do we need to update the container polygon's information? *)
    clone_idx := MapFormat.add_object obj;
    !clone_idx
