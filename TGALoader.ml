open CamlExt

let read_tga_from_handle fh =
    let id_size = input_byte fh in
    let cmap_exists = input_byte fh in
    let image_type = input_byte fh in
    let cmap_start = input_word fh in
    let cmap_length = input_word fh in
    let cmap_entry_width = input_word fh in
    let xstart = input_word fh in
    let ystart = input_word fh in
    let width = input_word fh in
    let height = input_word fh in
    let pixel_width_bits = input_byte fh in
    let pixel_width_bytes =
        match pixel_width_bits with
            |16 -> 2
            |24 -> 3
            |32 -> 4 in
    let flip = input_byte fh in
    seek_in fh (pos_in fh + id_size);
    let buffer_size = width * height * pixel_width_bits in
    let raw = Raw.create `ubyte buffer_size in
    let rec dump_to_raw acc =
        if acc = buffer_size then () else begin
            Raw.set raw acc (input_byte fh);
            dump_to_raw (acc+1)
        end in
    dump_to_raw 0;
    raw

let read_tga name =
    let fh = open_in_bin name in
    let raw = read_tga_from_handle fh in
    close_in fh;
    raw
