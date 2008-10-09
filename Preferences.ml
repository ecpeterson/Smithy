(*** Preferences.ml contains routines that load and save the cross-execution
 * state, like colors or file dialog paths. ***)

let load_prefs _ =
    let scale = try
        let fh = open_in_bin Resources.preferences_file in
        DrawModeSettings.grid_factor := Marshal.from_channel fh;
        DrawModeSettings.display_grid := Marshal.from_channel fh;
        DrawModeSettings.constrain_to_grid := Marshal.from_channel fh;
        DrawModeSettings.highlighted_point_thickness := Marshal.from_channel fh;
        DrawModeSettings.pixel_epsilon := Marshal.from_channel fh;
        Colors.background_color := Marshal.from_channel fh;
        Colors.grid_color := Marshal.from_channel fh;
        Colors.anchor_point_color := Marshal.from_channel fh;
        Colors.polygon_color := Marshal.from_channel fh;
        Colors.point_color := Marshal.from_channel fh;
        Colors.solid_line_color := Marshal.from_channel fh;
        Colors.transparent_line_color := Marshal.from_channel fh;
        Colors.passable_line_color := Marshal.from_channel fh;
        Colors.highlight_color := Marshal.from_channel fh;
        Colors.invalid_polygon := Marshal.from_channel fh;
        Colors.poly_type_saturation := Marshal.from_channel fh;
        Colors.poly_type_value := Marshal.from_channel fh;
        let scale = (Marshal.from_channel fh) in
        FileDialogs.path := Marshal.from_channel fh;
        close_in fh;
        scale
    with _ -> print_endline "Failed to load preferences!"; 32. in
    scale

let save_prefs scale =
    try
        let fh = open_out_bin Resources.preferences_file in
        Marshal.to_channel fh !DrawModeSettings.grid_factor [];
        Marshal.to_channel fh !DrawModeSettings.display_grid [];
        Marshal.to_channel fh !DrawModeSettings.constrain_to_grid [];
        Marshal.to_channel fh !DrawModeSettings.highlighted_point_thickness [];
        Marshal.to_channel fh !DrawModeSettings.pixel_epsilon [];
        Marshal.to_channel fh !Colors.background_color [];
        Marshal.to_channel fh !Colors.grid_color [];
        Marshal.to_channel fh !Colors.anchor_point_color [];
        Marshal.to_channel fh !Colors.polygon_color [];
        Marshal.to_channel fh !Colors.point_color [];
        Marshal.to_channel fh !Colors.solid_line_color [];
        Marshal.to_channel fh !Colors.transparent_line_color [];
        Marshal.to_channel fh !Colors.passable_line_color [];
        Marshal.to_channel fh !Colors.highlight_color [];
        Marshal.to_channel fh !Colors.invalid_polygon [];
        Marshal.to_channel fh !Colors.poly_type_saturation [];
        Marshal.to_channel fh !Colors.poly_type_value [];
        Marshal.to_channel fh (scale) [];
        Marshal.to_channel fh !FileDialogs.path [];
        close_out fh
    with _ -> print_endline "Failed to write preferences!"
