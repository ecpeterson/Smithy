(*** MapTypes.ml contains various datatypes natural to the Marathon map format,
 * mostly used for mapping directly over the structs stored in Marathon map
 * files ***)

open CamlExt

let vertices_per_poly = 8

(* a point in Z^2 *)
type p2d = int * int
type p3d = int * int * int

(* a pointer to a location in the Shapes file *)
type shape_descriptor = int * int * int
let sd_of_int n =
    let clut = n land 0xe000 in
    let collection = n land 0x1f00 in
    let shape = n land 0x00ff in
    (clut lsr 13, collection lsr 8, shape)
let int_of_sd (clut, collection, shape) =
    (clut lsl 13) lor (collection lsl 8) lor (shape)
let empty_sd = sd_of_int 65535 (* -1 in 16-bit arithmetic *)

(* a point object in the shapes file (compare with an EPNT entry for an
 * explanation of why this is an object) *)
class point = object (self)
    val mutable vertex = (0, 0)

    method set_vertex x = vertex <- x

    method vertex () = vertex
end
let pnts_reader fh =
    let point = new point in
    let vertex_x = input_signed_word fh in
    let vertex_y = input_signed_word fh in
    point#set_vertex (vertex_x, vertex_y);
    point
let epnt_reader fh =
    let point = new point in
    let flags = input_word fh in
    let highest_adjacent_floor_height = input_signed_word fh in
    let lowest_adjacent_ceiling_height = input_signed_word fh in
    let vertex_x = input_signed_word fh in
    let vertex_y = input_signed_word fh in
    point#set_vertex (vertex_x, vertex_y);
    let transformed_x = input_signed_word fh in
    let transformed_y = input_signed_word fh in
    let supporting_poly_index = input_signed_word fh in
    point
let pnts_writer fh point =
    let (x, y) = point#vertex () in
    output_signed_word fh x;
    output_signed_word fh y

let empty_point = new point

(* a line object *)
type lines_flags = SOLID | TRANSPARENT | LANDSCAPED |
                   ELEVATED | VAR_ELEV | TRANSPARENT_SIDE
let lines_flags_descriptor = [0x4000, SOLID; 0x2000, TRANSPARENT;
    0x1000, LANDSCAPED; 0x0800, ELEVATED; 0x0400, VAR_ELEV;
    0x0200, TRANSPARENT_SIDE]
class line = object
    val mutable endpoints = (0, 0)
    val mutable flags = [SOLID]
    val mutable length = 0
    val mutable highest_adjacent_floor = 0
    val mutable lowest_adjacent_ceiling = 0
    val mutable cw_poly_side_index = -1
    val mutable ccw_poly_side_index = -1
    val mutable cw_poly_owner = -1
    val mutable ccw_poly_owner = -1

    method set_endpoints x = endpoints <- x
    method set_flags x = flags <- x
    method set_length x = length <- x
    method set_highest_adjacent_floor x = highest_adjacent_floor <- x
    method set_lowest_adjacent_ceiling x = lowest_adjacent_ceiling <- x
    method set_cw_poly_side_index x = cw_poly_side_index <- x
    method set_ccw_poly_side_index x = ccw_poly_side_index <- x
    method set_cw_poly_owner x = cw_poly_owner <- x
    method set_ccw_poly_owner x = ccw_poly_owner <- x

    method endpoints () = endpoints
    method flags () = flags
    method length () = length
    method highest_adjacent_floor () = highest_adjacent_floor
    method lowest_adjacent_ceiling () = lowest_adjacent_ceiling
    method cw_poly_side_index () = cw_poly_side_index
    method ccw_poly_side_index () = ccw_poly_side_index
    method cw_poly_owner () = cw_poly_owner
    method ccw_poly_owner () = ccw_poly_owner
end
let empty_line = new line
let lins_reader fh =
    let line = new line in
    let to_lines_flag = CamlExt.of_bitflag lines_flags_descriptor in
    let endpoint1 = input_word fh in
    let endpoint2 = input_word fh in
    line#set_endpoints (endpoint1, endpoint2);
    line#set_flags (to_lines_flag (input_word fh));
    line#set_length (input_signed_word fh);
    line#set_highest_adjacent_floor (input_signed_word fh);
    line#set_lowest_adjacent_ceiling (input_signed_word fh);
    line#set_cw_poly_side_index (input_signed_word fh);
    line#set_ccw_poly_side_index (input_signed_word fh);
    line#set_cw_poly_owner (input_signed_word fh);
    line#set_ccw_poly_owner (input_signed_word fh);
    (* then toss twelve bytes *)
    ignore (input_dword fh);
    ignore (input_dword fh);
    ignore (input_dword fh);
    line
let lins_writer fh line =
    let (endpoint1, endpoint2) = line#endpoints () in
    output_word fh endpoint1;
    output_word fh endpoint2;
    output_word fh (CamlExt.to_bitflag lines_flags_descriptor (line#flags ()));
    (*output_signed_word fh length;*)
    output_signed_word fh 0; (* temporary to see if this fixes a1 *)
    output_signed_word fh (line#highest_adjacent_floor ());
    output_signed_word fh (line#lowest_adjacent_ceiling ());
    output_signed_word fh (line#cw_poly_side_index ());
    output_signed_word fh (line#ccw_poly_side_index ());
    output_signed_word fh (line#cw_poly_owner ());
    output_signed_word fh (line#ccw_poly_owner ());
    ignore (output_padding fh 12)

class platform = object
    val mutable kind = 0
    val mutable speed = 0
    val mutable delay = 0
    val mutable maximum_height = 0
    val mutable minimum_height = 0
    val mutable static_flags = 0
    val mutable polygon_index = 0
    val mutable tag = 0

    method kind () = kind
    method speed () = speed
    method delay () = delay
    method maximum_height () = maximum_height
    method minimum_height () = minimum_height
    method static_flags () = static_flags
    method polygon_index () = polygon_index
    method tag () = tag

    method set_kind x = kind <- x
    method set_speed x = speed <- x
    method set_delay x = delay <- x
    method set_maximum_height x = maximum_height <- x
    method set_minimum_height x = minimum_height <- x
    method set_static_flags x = static_flags <- x
    method set_polygon_index x = polygon_index <- x
    method set_tag x = tag <- x
end
let plat_reader fh =
    let plat = new platform in
    plat#set_kind (input_word fh);
    plat#set_speed (input_word fh);
    plat#set_delay (input_word fh);
    plat#set_maximum_height (input_signed_word fh);
    plat#set_minimum_height (input_signed_word fh);
    plat#set_static_flags (input_dword fh);
    plat#set_polygon_index (input_word fh);
    plat#set_tag (input_word fh);
    ignore (input_dword fh);
    ignore (input_dword fh);
    ignore (input_dword fh);
    ignore (input_word fh);
    plat
let plat_writer fh plat =
    output_word fh (plat#kind ());
    output_word fh (plat#speed ());
    output_word fh (plat#delay ());
    output_signed_word fh (plat#maximum_height ());
    output_signed_word fh (plat#minimum_height ());
    output_dword fh (plat#static_flags ());
    output_word fh (plat#polygon_index ());
    output_word fh (plat#tag ());
    ignore (output_padding fh 14)
let empty_platform = new platform

type poly_kind = Normal | Item_Impassable | Monster_and_Item_Impassable | Hill |
                 Base | Platform | Light_On_Trigger | Platform_On_Trigger |
                 Light_Off_Trigger | Platform_Off_Trigger | Teleporter |
                 Zone_Border | Goal | Visible_Monster_Trigger |
                 Invisible_Monster_Trigger | Dual_Monster_Trigger | Item_Trigger |
                 Must_Be_Explored | Automatic_Exit | Minor_Ouch | Major_Ouch |
                 Glue | Glue_Trigger | Superglue
let poly_kind_descriptor = 0, [Normal; Item_Impassable; Monster_and_Item_Impassable;
                            Hill; Base; Platform; Light_On_Trigger;
                            Platform_On_Trigger; Light_Off_Trigger;
                            Platform_Off_Trigger; Teleporter; Zone_Border; Goal;
                            Visible_Monster_Trigger; Invisible_Monster_Trigger;
                            Dual_Monster_Trigger; Item_Trigger;
                            Must_Be_Explored; Automatic_Exit; Minor_Ouch;
                            Major_Ouch; Glue; Glue_Trigger; Superglue]
(* a polygon object *)
class polygon = object
    val mutable kind = Normal
    val mutable flags = 0
    val mutable permutation = -1
    val mutable vertex_count = 0
    val mutable endpoint_indices = [|0;0;0;0;0;0;0;0|]
    val mutable line_indices = [|0;0;0;0;0;0;0;0|]
    val mutable floor_texture = empty_sd
    val mutable ceiling_texture = empty_sd
    val mutable floor_height = 0.0
    val mutable ceiling_height = 1.0
    val mutable floor_lightsource = 0
    val mutable ceiling_lightsource = 0
    val mutable area = 0
    val mutable first_object = -1
    val mutable first_exclusion_zone_index = 0
    val mutable line_exclusion_zone_count = 0
    val mutable point_exclusion_zone_count = 0
    val mutable floor_transfer_mode = 0
    val mutable ceiling_transfer_mode = 0
    val mutable adjacent_polygon_indices = [|0;0;0;0;0;0;0;0|]
    val mutable first_neighbor_index = 0
    val mutable neighbor_count = 0
    val mutable center = (0, 0)
    val mutable side_indices = [|0;0;0;0;0;0;0;0|]
    val mutable floor_origin = (0, 0)
    val mutable ceiling_origin = (0, 0)
    val mutable media_index = -1
    val mutable media_lightsource = 0
    val mutable sound_source_indices = 0
    val mutable ambient_sound_image_index = -1
    val mutable random_sound_image_index = -1

    method set_kind x = kind <- x
    method set_flags x = flags <- x
    method set_permutation x = permutation <- x
    method set_vertex_count x = vertex_count <- x
    method set_endpoint_indices x = endpoint_indices <- x
    method set_line_indices x = line_indices <- x
    method set_floor_texture x = floor_texture <- x
    method set_ceiling_texture x = ceiling_texture <- x
    method set_floor_height x = floor_height <- x
    method set_ceiling_height x = ceiling_height <- x
    method set_floor_lightsource x = floor_lightsource <- x
    method set_ceiling_lightsource x = ceiling_lightsource <- x
    method set_area x = area <- x
    method set_first_object x = first_object <- x
    method set_first_exclusion_zone_index x = first_exclusion_zone_index <- x
    method set_line_exclusion_zone_count x = line_exclusion_zone_count <- x
    method set_point_exclusion_zone_count x = point_exclusion_zone_count <- x
    method set_floor_transfer_mode x = floor_transfer_mode <- x
    method set_ceiling_transfer_mode x = ceiling_transfer_mode <- x
    method set_adjacent_polygon_indices x = adjacent_polygon_indices <- x
    method set_first_neighbor_index x = first_neighbor_index <- x
    method set_neighbor_count x = neighbor_count <- x
    method set_center x = center <- x
    method set_side_indices x = side_indices <- x
    method set_floor_origin x = floor_origin <- x
    method set_ceiling_origin x = ceiling_origin <- x
    method set_media_index x = media_index <- x
    method set_media_lightsource x = media_lightsource <- x
    method set_sound_source_indices x = sound_source_indices <- x
    method set_ambient_sound_image_index x = ambient_sound_image_index <- x
    method set_random_sound_image_index x = random_sound_image_index <- x
    
    method kind () = kind
    method flags () = flags
    method permutation () = permutation
    method vertex_count () = vertex_count
    method endpoint_indices () = endpoint_indices
    method line_indices () = line_indices
    method floor_texture () = floor_texture
    method ceiling_texture () = ceiling_texture
    method floor_height () = floor_height
    method ceiling_height () = ceiling_height
    method floor_lightsource () = floor_lightsource
    method ceiling_lightsource () = ceiling_lightsource
    method area () = area
    method first_object () = first_object
    method first_exclusion_zone_index () = first_exclusion_zone_index
    method line_exclusion_zone_count () = line_exclusion_zone_count
    method point_exclusion_zone_count () = point_exclusion_zone_count
    method floor_transfer_mode () = floor_transfer_mode
    method ceiling_transfer_mode () = ceiling_transfer_mode
    method adjacent_polygon_indices () = adjacent_polygon_indices
    method first_neighbor_index () = first_neighbor_index
    method neighbor_count () = neighbor_count
    method center () = center
    method side_indices () = side_indices
    method floor_origin () = floor_origin
    method ceiling_origin () = ceiling_origin
    method media_index () = media_index
    method media_lightsource () = media_lightsource
    method sound_source_indices () = sound_source_indices
    method ambient_sound_image_index () = ambient_sound_image_index
    method random_sound_image_index () = random_sound_image_index
end
let poly_reader fh =
    let poly = new polygon in
    poly#set_kind (of_enum poly_kind_descriptor (input_word fh));
    poly#set_flags (input_word fh);
    poly#set_permutation (input_signed_word fh);
    poly#set_vertex_count (input_word fh);
    for j = 0 to vertices_per_poly - 1 do
        (poly#endpoint_indices ()).(j) <- input_word fh
    done;
    for j = 0 to vertices_per_poly - 1 do
        (poly#line_indices ()).(j) <- input_word fh
    done;
    poly#set_floor_texture (sd_of_int (input_word fh));
    poly#set_ceiling_texture (sd_of_int (input_word fh));
    poly#set_floor_height (float (input_signed_word fh) /. 1024.0);
    poly#set_ceiling_height (float (input_signed_word fh) /. 1024.0);
    poly#set_floor_lightsource (input_signed_word fh);
    poly#set_ceiling_lightsource (input_signed_word fh);
    poly#set_area (input_dword fh);
    poly#set_first_object (input_signed_word fh);
    poly#set_first_exclusion_zone_index (input_word fh);
    poly#set_line_exclusion_zone_count (input_word fh);
    poly#set_point_exclusion_zone_count (input_word fh);
    poly#set_floor_transfer_mode (input_word fh);
    poly#set_ceiling_transfer_mode (input_word fh);
    for j = 0 to vertices_per_poly - 1 do
        (poly#adjacent_polygon_indices ()).(j) <- input_word fh
    done;
    poly#set_first_neighbor_index (input_word fh);
    poly#set_neighbor_count (input_word fh);
    let centerx = input_word fh in
    let centery = input_word fh in
    poly#set_center (centerx, centery);
    for j = 0 to vertices_per_poly - 1 do
        (poly#side_indices ()).(j) <- input_word fh
    done;
    let floor_originx = input_word fh in
    let floor_originy = input_word fh in
    poly#set_floor_origin (floor_originx, floor_originy);
    let ceiling_originx = input_word fh in
    let ceiling_originy = input_word fh in
    poly#set_ceiling_origin (ceiling_originx, ceiling_originy);
    poly#set_media_index (input_signed_word fh);
    poly#set_media_lightsource (input_signed_word fh);
    poly#set_sound_source_indices (input_word fh);
    poly#set_ambient_sound_image_index (input_word fh);
    poly#set_random_sound_image_index (input_word fh);
    ignore (input_word fh); (* toss two bytes *)
    poly

let poly_writer fh poly =
    output_word fh (to_enum poly_kind_descriptor (poly#kind ()));
    output_word fh (poly#flags ());
    output_signed_word fh (poly#permutation ());
    output_word fh (poly#vertex_count ());
    Array.iter (fun x -> output_word fh x) (poly#endpoint_indices ());
    Array.iter (fun x -> output_word fh x) (poly#line_indices ());
    output_word fh (int_of_sd (poly#floor_texture ()));
    output_word fh (int_of_sd (poly#ceiling_texture ()));
    output_signed_word fh (int_of_float (poly#floor_height () *. 1024.0));
    output_signed_word fh (int_of_float (poly#ceiling_height () *. 1024.0));
    output_signed_word fh (poly#floor_lightsource ());
    output_signed_word fh (poly#ceiling_lightsource ());
    output_dword fh (poly#area ());
    output_signed_word fh (poly#first_object ());
    output_word fh (poly#first_exclusion_zone_index ());
    output_word fh (poly#line_exclusion_zone_count ());
    output_word fh (poly#point_exclusion_zone_count ());
    output_word fh (poly#floor_transfer_mode ());
    output_word fh (poly#ceiling_transfer_mode ());
    Array.iter (fun x -> output_word fh x) (poly#adjacent_polygon_indices ());
    output_word fh (poly#first_neighbor_index ());
    output_word fh (poly#neighbor_count ());
    let (centerx, centery) = poly#center () in
    output_word fh centerx;
    output_word fh centery;
    Array.iter (fun x -> output_word fh x) (poly#side_indices ());
    let (fox, foy) = poly#floor_origin () in
    output_word fh fox;
    output_word fh foy;
    let (cox, coy) = poly#ceiling_origin () in
    output_word fh cox;
    output_word fh coy;
    output_signed_word fh (poly#media_index ());
    output_signed_word fh (poly#media_lightsource ());
    output_word fh (poly#sound_source_indices ());
    output_word fh (poly#ambient_sound_image_index ());
    output_word fh (poly#random_sound_image_index ());
    ignore (output_padding fh 2)
let empty_polygon = new polygon

type side_texture =
    (p2d * shape_descriptor)
let empty_st = (0, 0), (0, 0, 0)

type side_flags = Control_Panel_Status | Control_Panel | Repair_Switch |
                  Destructive_Switch | Lighted_Switch | Switch_Can_Be_Destroyed |
                  Switch_Can_Only_Be_Hit_By_Projectiles
let side_flags_descriptor = [(1, Control_Panel_Status); (2, Control_Panel);
                             (4, Repair_Switch); (8, Destructive_Switch);
                             (16, Lighted_Switch); (32, Switch_Can_Be_Destroyed);
                             (64, Switch_Can_Only_Be_Hit_By_Projectiles)]
type side_kind = Full_Side | High_Side | Low_Side | Composite_Side | Split_Side
let side_kind_descriptor = 0, [Full_Side; High_Side; Low_Side; Composite_Side;
                               Split_Side]
(*type side_kind = Oxygen_Refuel | Shield_Refuel | Double_Shield_Refuel |*)
                 (*Triple_Shield_Refuel | Light_Switch | Platform_Switch |*)
                 (*Tag_Switch | Pattern_Buffer | Computer_Terminal*)
(*let side_kind_descriptor = 0, [Oxygen_Refuel; Shield_Refuel; Double_Shield_Refuel;*)
                            (*Triple_Shield_Refuel; Light_Switch; Platform_Switch;*)
                            (*Tag_Switch; Pattern_Buffer; Computer_Terminal]*)
class side = object
    val mutable kind = Full_Side
    val mutable flags = ([] : side_flags list)
    val mutable primary_texture = empty_st
    val mutable secondary_texture = empty_st
    val mutable transparent_texture = empty_st
    val mutable exclusion_zone = ((0, 0), (0, 0), (0, 0), (0, 0))
    val mutable control_panel_type = 0
    val mutable control_panel_permutation = 0
    val mutable primary_transfer_mode = 0
    val mutable secondary_transfer_mode = 0
    val mutable transparent_transfer_mode = 0
    val mutable polygon_index = 0
    val mutable line_index = 0
    val mutable primary_lightsource = 0
    val mutable secondary_lightsource = 0
    val mutable transparent_lightsource = 0
    val mutable ambient_delta = 0

    method set_kind x = kind <- x
    method set_flags x = flags <- x
    method set_primary_texture x = primary_texture <- x
    method set_secondary_texture x = secondary_texture <- x
    method set_transparent_texture x = transparent_texture <- x
    method set_exclusion_zone x = exclusion_zone <- x
    method set_control_panel_type x = control_panel_type <- x
    method set_control_panel_permutation x = control_panel_permutation <- x
    method set_primary_transfer_mode x = primary_transfer_mode <- x
    method set_secondary_transfer_mode x = secondary_transfer_mode <- x
    method set_transparent_transfer_mode x = transparent_transfer_mode <- x
    method set_polygon_index x = polygon_index <- x
    method set_line_index x = line_index <- x
    method set_primary_lightsource x = primary_lightsource <- x
    method set_secondary_lightsource x = secondary_lightsource <- x
    method set_transparent_lightsource x = transparent_lightsource <- x
    method set_ambient_delta x = ambient_delta <- x

    method kind () = kind
    method flags () = flags
    method primary_texture () = primary_texture
    method secondary_texture () = secondary_texture
    method transparent_texture () = transparent_texture
    method exclusion_zone () = exclusion_zone
    method control_panel_type () = control_panel_type
    method control_panel_permutation () = control_panel_permutation
    method primary_transfer_mode () = primary_transfer_mode
    method secondary_transfer_mode () = secondary_transfer_mode
    method transparent_transfer_mode () = transparent_transfer_mode
    method polygon_index () = polygon_index
    method line_index () = line_index
    method primary_lightsource () = primary_lightsource
    method secondary_lightsource () = secondary_lightsource
    method transparent_lightsource () = transparent_lightsource
    method ambient_delta () = ambient_delta
end
let sids_reader fh =
    let side = new side in
    side#set_kind (of_enum side_kind_descriptor (input_word fh));
    side#set_flags (of_bitflag side_flags_descriptor (input_word fh));
    let pri_tex_x = input_word fh in
    let pri_tex_y = input_word fh in
    let pri_tex_sd = sd_of_int (input_word fh) in
    side#set_primary_texture ((pri_tex_x, pri_tex_y), pri_tex_sd);
    let sec_tex_x = input_word fh in
    let sec_tex_y = input_word fh in
    let sec_tex_sd = sd_of_int (input_word fh) in
    side#set_secondary_texture ((sec_tex_x, sec_tex_y), sec_tex_sd);
    let tpt_tex_x = input_word fh in
    let tpt_tex_y = input_word fh in
    let tpt_tex_sd = sd_of_int (input_word fh) in
    side#set_transparent_texture ((tpt_tex_x, tpt_tex_y), tpt_tex_sd);
    let s0x = input_word fh in
    let s0y = input_word fh in
    let s1x = input_word fh in
    let s1y = input_word fh in
    let s2x = input_word fh in
    let s2y = input_word fh in
    let s3x = input_word fh in
    let s3y = input_word fh in
    side#set_exclusion_zone ((s0x, s0y), (s1x, s1y), (s2x, s2y), (s3x, s3y));
    side#set_control_panel_type (input_word fh);
    side#set_control_panel_permutation (input_word fh);
    side#set_primary_transfer_mode (input_word fh);
    side#set_secondary_transfer_mode (input_word fh);
    side#set_transparent_transfer_mode (input_word fh);
    side#set_polygon_index (input_word fh);
    side#set_line_index (input_word fh);
    side#set_primary_lightsource (input_word fh);
    side#set_secondary_lightsource (input_word fh);
    side#set_transparent_lightsource (input_word fh);
    side#set_ambient_delta (input_dword fh);
    ignore (input_word fh); (* drop 2 bytes *)
    side

let sids_writer fh side =
    output_word fh (to_enum side_kind_descriptor (side#kind ()));
    output_word fh (to_bitflag side_flags_descriptor (side#flags ()));
    let ((x, y), sd) = side#primary_texture () in
    output_word fh x;
    output_word fh y;
    output_word fh (int_of_sd sd);
    let ((x, y), sd) = side#secondary_texture () in
    output_word fh x;
    output_word fh y;
    output_word fh (int_of_sd sd);
    let ((x, y), sd) = side#transparent_texture () in
    output_word fh x;
    output_word fh y;
    output_word fh (int_of_sd sd);
    let ((s0x, s0y), (s1x, s1y), (s2x, s2y), (s3x, s3y)) = side#exclusion_zone () in
    output_word fh s0x;
    output_word fh s0y;
    output_word fh s1x;
    output_word fh s1y;
    output_word fh s2x;
    output_word fh s2y;
    output_word fh s3x;
    output_word fh s3y;
    output_word fh (side#control_panel_type ());
    output_word fh (side#control_panel_permutation ());
    output_word fh (side#primary_transfer_mode ());
    output_word fh (side#secondary_transfer_mode ());
    output_word fh (side#transparent_transfer_mode ());
    output_word fh (side#polygon_index ());
    output_word fh (side#line_index ());
    output_word fh (side#primary_lightsource ());
    output_word fh (side#secondary_lightsource ());
    output_word fh (side#transparent_lightsource ());
    output_dword fh (side#ambient_delta ());
    ignore (output_padding fh 2)
let empty_side = new side

type light_spec = int * int * int * float * float
let empty_ls = 0, 30, 0, 0.0, 0.0
type light_kind = Normal_Light | Strobe_Light | Media_Light
let light_kind_descriptor = 0, [Normal_Light; Strobe_Light; Media_Light]
type light_flag = Active_Light | Slaved_Intensities | Stateless_Light
let light_flag_descriptor = [1, Active_Light; 2, Slaved_Intensities;
                             4, Stateless_Light]

class light = object
    val mutable kind = Normal_Light
    val mutable flags = ([] : light_flag list)
    val mutable phase = 0
    val mutable primary_active = empty_ls
    val mutable secondary_active = empty_ls
    val mutable becoming_active = empty_ls
    val mutable primary_inactive = empty_ls
    val mutable secondary_inactive = empty_ls
    val mutable becoming_inactive = empty_ls
    val mutable tag = 0

    method set_kind x = kind <- x
    method set_flags x = flags <- x
    method set_phase x = phase <- x
    method set_primary_active x = primary_active <- x
    method set_secondary_active x = secondary_active <- x
    method set_becoming_active x = becoming_active <- x
    method set_primary_inactive x = primary_inactive <- x
    method set_secondary_inactive x = secondary_inactive <- x
    method set_becoming_inactive x = becoming_inactive <- x
    method set_tag x = tag <- x

    method kind () = kind
    method flags () = flags
    method phase () = phase
    method primary_active () = primary_active
    method secondary_active () = secondary_active
    method becoming_active () = becoming_active
    method primary_inactive () = primary_inactive
    method secondary_inactive () = secondary_inactive
    method becoming_inactive () = becoming_inactive
    method tag () = tag
end
let lite_reader fh =
    let light = new light in
    let input_ls fh =
        let kind = input_word fh in
        let period = input_word fh in
        let delta_period = input_word fh in
        let intensity = input_fixed fh in
        let delta_intensity = input_fixed fh in
        (kind, period, delta_period, intensity, delta_intensity) in
    light#set_kind (CamlExt.of_enum light_kind_descriptor (input_word fh));
    light#set_flags (CamlExt.of_bitflag light_flag_descriptor (input_word fh));
    light#set_phase (input_word fh);
    light#set_primary_active (input_ls fh);
    light#set_secondary_active (input_ls fh);
    light#set_becoming_active (input_ls fh);
    light#set_primary_inactive (input_ls fh);
    light#set_secondary_inactive (input_ls fh);
    light#set_becoming_inactive (input_ls fh);
    light#set_tag (input_word fh);
    ignore (input_dword fh); (* skip two bytes *)
    ignore (input_dword fh);
    light

let lite_writer fh light =
    let output_ls fh (x, y, z, s, t) =
        output_word fh x;
        output_word fh y;
        output_word fh z;
        output_fixed fh s;
        output_fixed fh t in
    output_word fh (CamlExt.to_enum light_kind_descriptor (light#kind ()));
    output_word fh (CamlExt.to_bitflag light_flag_descriptor (light#flags ()));
    output_word fh (light#phase ());
    output_ls fh (light#primary_active ());
    output_ls fh (light#secondary_active ());
    output_ls fh (light#becoming_active ());
    output_ls fh (light#primary_inactive ());
    output_ls fh (light#secondary_inactive ());
    output_ls fh (light#becoming_inactive ());
    output_word fh (light#tag ());
    ignore (output_padding fh 8)
let empty_light = new light

type object_kind = Monster | Scenery | Item | Player | Goal | Sound_Source
let object_kind_descriptor = 0, [Monster; Scenery; Item; Player; Goal;
                                 Sound_Source]
type object_flags = Invisible_Or_Platform | Platform_Sound | Hangs_From_Ceiling|
                    Blind | Deaf | Floats | Network_Only
let object_flags_descriptor = [1, Invisible_Or_Platform; 2, Hangs_From_Ceiling;
                               4, Blind; 8, Deaf; 16, Floats; 32, Network_Only]
class obj = object
    val mutable kind = Player
    val mutable index = 0
    val mutable facing = 0
    val mutable polygon = 0
    val mutable point = (0, 0, 0)
    val mutable flags = ([] : object_flags list)

    method set_kind x = kind <- x
    method set_index x = index <- x
    method set_facing x = facing <- x
    method set_polygon x = polygon <- x
    method set_point x = point <- x
    method set_flags x = flags <- x

    method kind () = kind
    method index () = index
    method facing () = facing
    method polygon () = polygon
    method point () = point
    method flags () = flags
end
let objs_reader fh =
    let obj = new obj in
    obj#set_kind (CamlExt.of_enum object_kind_descriptor (input_word fh));
    obj#set_index (input_word fh);
    obj#set_facing (input_signed_word fh);
    obj#set_polygon (input_word fh);
    let px = input_signed_word fh in
    let py = input_signed_word fh in
    let pz = input_signed_word fh in
    obj#set_point (px, py, pz);
    obj#set_flags (CamlExt.of_bitflag object_flags_descriptor (input_word fh));
    obj
let objs_writer fh obj =
    output_word fh (CamlExt.to_enum object_kind_descriptor (obj#kind ()));
    output_word fh (obj#index ());
    output_signed_word fh (obj#facing ());
    output_word fh (obj#polygon ());
    let (x, y, z) = obj#point () in
    output_signed_word fh x;
    output_signed_word fh y;
    output_signed_word fh z;
    output_word fh (CamlExt.to_bitflag object_flags_descriptor (obj#flags ()))
let empty_obj = new obj

type media_flags = Liquid_Obstructs_Sounds
let media_flags_descriptor = [1, Liquid_Obstructs_Sounds]
class media = object
    val mutable kind = 0
    val mutable flags = ([] : media_flags list)
    val mutable light_index = 0
    val mutable direction = 0
    val mutable magnitude = 0
    val mutable low = 0
    val mutable high = 0
    val mutable origin = (0, 0)
    val mutable height = 0
    val mutable minimum_light_intensity = 0.0
    val mutable texture = empty_sd
    val mutable transfer_mode = 0

    method kind () = kind
    method flags () = flags
    method light_index () = light_index
    method direction () = direction
    method magnitude () = magnitude
    method low () = low
    method high () = high
    method origin () = origin
    method height () = height
    method minimum_light_intensity () = minimum_light_intensity
    method texture () = texture
    method transfer_mode () = transfer_mode

    method set_kind x = kind <- x
    method set_flags x = flags <- x
    method set_light_index x = light_index <- x
    method set_direction x = direction <- x
    method set_magnitude x = magnitude <- x
    method set_low x = low <- x
    method set_high x = high <- x
    method set_origin x = origin <- x
    method set_height x = height <- x
    method set_minimum_light_intensity x = minimum_light_intensity <- x
    method set_texture x = texture <- x
    method set_transfer_mode x = transfer_mode <- x
end
let medi_reader fh =
    let media = new media in
    media#set_kind (input_word fh);
    media#set_flags (CamlExt.of_bitflag media_flags_descriptor (input_word fh));
    media#set_light_index (input_word fh);
    media#set_direction (input_word fh); (* not sure if this is right *)
    media#set_magnitude (input_signed_word fh);
    media#set_low (input_signed_word fh);
    media#set_high (input_signed_word fh);
    let ox = input_signed_word fh in
    let oy = input_signed_word fh in
    media#set_origin (ox, oy);
    media#set_height (input_signed_word fh);
    media#set_minimum_light_intensity (input_fixed fh);
    media#set_texture (sd_of_int (input_word fh));
    media#set_transfer_mode (input_word fh);
    ignore (input_dword fh); (* skip four bytes *)
    media
let medi_writer fh media =
    output_word fh (media#kind ());
    output_word fh (CamlExt.to_bitflag media_flags_descriptor (media#flags ()));
    output_word fh (media#light_index ());
    output_word fh (media#direction ());
    output_signed_word fh (media#magnitude ());
    output_signed_word fh (media#low ());
    output_signed_word fh (media#high ());
    let (ox, oy) = media#origin () in
    output_signed_word fh ox;
    output_signed_word fh oy;
    output_signed_word fh (media#height ());
    output_fixed fh (media#minimum_light_intensity ());
    output_word fh (int_of_sd (media#texture ()));
    output_word fh (media#transfer_mode ());
    ignore (output_padding fh 4)
let empty_media = new media

class placement = object
    val mutable flags = 0
    val mutable initial_count = 0
    val mutable minimum_count = 0
    val mutable maximum_count = 0
    val mutable random_count = 0
    val mutable random_chance = 0

    method flags () = flags
    method initial_count () = initial_count
    method minimum_count () = minimum_count
    method maximum_count () = maximum_count
    method random_count () = random_count
    method random_chance () = random_chance

    method set_flags x = flags <- x
    method set_initial_count x = initial_count <- x
    method set_minimum_count x = minimum_count <- x
    method set_maximum_count x = maximum_count <- x
    method set_random_count x = random_count <- x
    method set_random_chance x = random_chance <- x
end
let plac_reader fh =
    let plac = new placement in
    plac#set_flags (input_word fh);
    plac#set_initial_count (input_word fh);
    plac#set_minimum_count (input_word fh);
    plac#set_maximum_count (input_word fh);
    plac#set_random_count (input_word fh);
    plac#set_random_chance (input_word fh);
    plac

let plac_writer fh plac =
    output_word fh (plac#flags ());
    output_word fh (plac#initial_count ());
    output_word fh (plac#minimum_count ());
    output_word fh (plac#maximum_count ());
    output_word fh (plac#random_count ());
    output_word fh (plac#random_chance ())
let empty_placement = new placement
