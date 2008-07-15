(*** Colors.ml contains color settings.  TODO: these could probably be moved to
 * DrawModeSettings.ml. ***)
(* these define what color things get painted while in draw mode *)
let background_color       = ref (0.3, 0.3, 0.3)
let grid_color             = ref (0.5, 0.5, 0.5)
let anchor_point_color     = ref (0.3, 0.8, 0.8)
let polygon_color          = ref (1.0, 1.0, 1.0)
let point_color            = ref (1.0, 0.0, 0.0)
let solid_line_color       = ref (0.0, 0.0, 0.0)
let transparent_line_color = ref (0.0, 1.0, 1.0)
let passable_line_color    = ref (0.5, 1.0, 0.5)
let highlight_color        = ref (1.0, 0.5, 0.0)
let invalid_polygon        = ref (1.0, 0.5, 0.5)

(* these define S and V in the HSV calculation while in poly type edit mode *)
let poly_type_saturation = ref 0.5
let poly_type_value = ref 0.5
