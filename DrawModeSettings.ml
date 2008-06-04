type tool = ArrowTool | LineTool |
            PolyTool  | FillTool |
            PanTool   | ZoomTool |
            TextTool  | ObjTool

let active_tool = ref ArrowTool
let grid_size = ref 3
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
