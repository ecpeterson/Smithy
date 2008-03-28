(** colors!  TODO: make configurable **)
(* these define what color things get painted while in draw mode *)
let background_color       = (0.3, 0.3, 0.3)
let grid_color             = (0.5, 0.5, 0.5)
let anchor_point_color     = (0.3, 0.8, 0.8)
let polygon_color          = (1.0, 1.0, 1.0)
let point_color            = (1.0, 0.0, 0.0)
let line_color             = (0.0, 0.0, 0.0)
let transparent_line_color = (0.0, 1.0, 1.0)
let highlight_color        = (1.0, 0.5, 0.0)
let invalid_polygon        = (1.0, 0.5, 0.5)

(* these define S and V in the HSV calculation while in poly type edit mode *)
let poly_type_saturation = 0.5
let poly_type_value = 0.5

(* these define colors while in VISUAL MODE *)
let ceiling_color = (1.0, 0.0, 0.0)
let ceiling_fill_color = (0.0, 1.0, 1.0)
let floor_color = (0.0, 1.0, 0.0)
let floor_fill_color = (1.0, 0.0, 1.0)
let wall_color = (0.0, 0.0, 1.0)
let wall_fill_color = (1.0, 1.0, 0.0)
(** end colors! **)
