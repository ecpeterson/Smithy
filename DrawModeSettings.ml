(*** DrawModeSettings.ml contains near-global mutables that correspond to state
 * of the application while in draw mode that multiple modules may need to
 * modify. ***)

type tool = ArrowTool | LineTool |
            PolyTool  | FillTool |
            PanTool   | ZoomTool |
            TextTool  | ObjTool

let active_tool = ref ArrowTool
let grid_factor = ref 3
let display_grid = ref true
let constrain_to_grid = ref false
let show_monsters = ref true
let show_objects = ref true
let show_scenery = ref true
let show_players = ref true
let show_goals = ref true
let show_sounds = ref true
let show_annotations = ref true
let vm_crosshair = ref true
let highlighted_point_thickness = ref 5
let pixel_epsilon = ref 8.0
let floor_cutoff = ref (-9.0)
let ceiling_cutoff = ref (9.0)

(* we keep track of what item is highlighted as part of the drawing / interface
 * object, and we have an enumerative type to match across *)
type highlighted_component =  No_Highlight       |
                              Point of int list  |
                              Line of int list   |
                              Poly of int list   |
                              Object of int list
(* some stateful data *)
let highlight = ref No_Highlight
