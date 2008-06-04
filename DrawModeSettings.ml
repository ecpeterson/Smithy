type tool = ArrowTool | LineTool |
            PolyTool  | FillTool |
            PanTool   | ZoomTool |
            TextTool  | ObjTool

let active_tool = ref ArrowTool
