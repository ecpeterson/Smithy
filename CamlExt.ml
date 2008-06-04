(*** CamlExt.ml provides a handful of utility functions that extend the
 * functionality of OCaml itself, need no larger context to justify their
 * existence, and may be used frequently throughout the program ***)

(* initialize GTK itself *)
let _ =
    ignore (GtkMain.Main.init ());
    Gdk.Rgb.init ();
    GtkBase.Widget.set_default_visual (Gdk.Rgb.get_visual ());
    GtkBase.Widget.set_default_colormap (Gdk.Rgb.get_cmap ())

(* some functionals *)
let id x = x
let const a = fun x -> a
(* how is this not a standard ocaml function *)
let rec pow n a = if a = 0 then 1 else n * (pow n (a - 1))

(* some haskell wishes *)
let ($) a b = a b
let (@) a b = fun x -> a (b x)
let (|>) x y = y x (* left-associative *)
let (@>) x y = y x (* right-assocative *)

(** reading wider integers out of binary files **)
let byte_cap = 256
let word_cap = 65536

let input_word fh =
    let first_byte = input_byte fh in
    let second_byte = input_byte fh in
    (first_byte * byte_cap) + second_byte

let output_word fh word =
    let second_byte = word land 0xff in
    let first_byte = (word lsr 8) land 0xff in
    output_byte fh first_byte;
    output_byte fh second_byte

(* utilities so we can read signed shorts *)
let unsigned_to_signed_16 x =
    let sign = x land 0x8000 in
    let contents = x land 0x7fff in
    if sign <> 0 then contents - 0x8000 else contents

let signed_to_unsigned_16 x =
    let x = x land 0xffff in
    if x < 0 then 0x10000 - x else x

let input_signed_word fh =
    input_word fh
        |> unsigned_to_signed_16

let output_signed_word fh x =
    signed_to_unsigned_16 x
        |> output_word fh

let input_dword fh =
    let first_word = input_word fh in
    let second_word = input_word fh in
    (first_word * word_cap) + second_word

let output_dword fh x =
    let second_word = x land 0xffff in
    let first_word = (x lsr 16) land 0xffff in
    output_word fh first_word;
    output_word fh second_word

let input_fixed fh =
    let word1 = input_word fh in
    let word2 = input_word fh in
    (float word1) +. (float word2 /. (2. ** 16.))

let output_fixed fh x =
    let word1 = (int_of_float x) land 0xffff in
    let word2 = (int_of_float (x *. 65536.0)) land 0xffff in
    output_word fh word1;
    output_word fh word2

(* and some utilities to read enumerative types *)
let rec of_bitflag descriptor x =
    match descriptor with
    |[] -> []
    |(x1, x2) :: xs ->
        if x land x1 <> 0
            then x2 :: of_bitflag xs x
            else of_bitflag xs x

let to_bitflag descriptor x =
    let rec search lst target =
        match lst with
            |(x, y) :: xs ->
                if y = target then x else search xs target
            |_ -> 0 in
    List.fold_left (fun x y -> x + search descriptor y) 0 x

let to_enum (start, descriptor) x =
    let rec search lst acc =
        match lst with
            |[] -> raise (Failure "to_enum can't find match!")
            |y :: ys ->
                if x = y then acc
                         else search ys (acc+1) in
    search descriptor start

let of_enum (start, descriptor) x =
    List.nth descriptor (x + start)

(* not all structs are byte-aligned, more utility *)
let rec output_padding fh n =
    if n = 0 then () else begin
    output_byte fh 0;
    output_padding fh (n - 1)
    end

let output_string_n fh str len =
    output_string fh str;
    ignore (output_padding fh (len - (String.length str)))

(* two debugging print routines *)
let dprint str =
    print_endline str;
    flush stdout

let rec print_array arr =
    match arr with
        |[] -> print_endline ""; flush stdout
        |x :: xs -> print_int x; print_string ";"; print_array xs

let rec print_array2 arr =
    Array.iter (fun x -> print_int x; print_string ";") arr;
    print_endline ""

(** array mangling routines, since all our map structures are arrays **)
(* finds obj in arr using whatever = is defined as for obj *)
let find_in_array arr obj =
    let length = Array.length arr in
    let rec fia_aux arr obj acc =
        if acc = length then acc else
        if arr.(acc) = obj
            then acc
            else fia_aux arr obj (acc+1) in
    fia_aux arr obj 0

(* deletes obj from an arr, shifts the remainder over, and fills the tail of the
 * array with replace *)
let delete_from_array arr obj replace =
    let pos = find_in_array arr obj in
    let len = Array.length arr in
    let rec dfa_aux arr replace acc =
        if acc >= pos && acc < len - 1 then begin
            arr.(acc) <- arr.(acc + 1);
            dfa_aux arr replace (acc + 1)
        end else
            arr.(acc) <- replace in
    if pos < len then dfa_aux arr replace pos else ()

(* an in-place map *)
let destructive_map f arr =
    let len = Array.length arr in
    let rec dm_aux f arr n =
        arr.(n) <- f arr.(n);
        if n = len - 1 then () else dm_aux f arr (n+1) in
    dm_aux f arr 0

(* deletes index n from arr and resizes it inefficiently *)
let delete_from_array_and_resize arr n =
    let len = Array.length arr in
    Array.append (Array.sub arr 0 n) (Array.sub arr (n+1) (len - n - 1))

(** some geometric functions that get used a lot *)
(* dot products *)
let dot (x0, y0) (x1, y1) =
    x0 * x1 + y0 * y1

let dotf (x0, y0) (x1, y1) =
    x0 *. x1 +. y0 *. y1

(* cross products *)
let crossf (x0, y0) (x1, y1) =
    y0 *. x1 -. x0 *. y1

let cross (x0, y0) (x1, y1) =
    y0 * x1 - x0 * y1

(* euclidean norm on R^2 *)
let norm (x, y) =
    (x**2.0 +. y**2.0)**0.5

(* euclidean metric on R^2 *)
let distance (x0, y0) (x1, y1) =
    norm ((x1 -. x0), (y1 -. y0))

(* checks a point ring for concavity *)
let vertex_array_is_concave vertices =
    let length = List.length vertices in
    (* this runs a generic comparison function on the cross products of adjacent
     * lines, returns true if all the comparisons pass and false otherwise *)
    let rec loop_compare vertices comp n =
        if n = length then true else begin
        let next1 = (if n = length - 1 then 0 else n + 1) in
        let next2 = (if next1 = length - 1 then 0 else next1 + 1) in
        let (p0x, p0y) = List.nth vertices n in
        let (p1x, p1y) = List.nth vertices next1 in
        let (p2x, p2y) = List.nth vertices next2 in
        let p0 = (p1x - p0x, p1y - p0y) in
        let p1 = (p2x - p1x, p2y - p1y) in
        if comp (cross p0 p1) then
            loop_compare vertices comp (n+1)
        else
            false end in
    (* if all the crosses are nonpositive, we have a clockwise point loop *)
    let vertex_array_is_cw vertices =
        loop_compare vertices (fun x -> x <= 0) 0 in
    (* if all the crosses are nonnegative, we have a ccw point loop *)
    let vertex_array_is_ccw vertices =
        loop_compare vertices (fun x -> x >= 0) 0 in
    (* and we want to test for loops that are neither completely cw nor ccw *)
    not (vertex_array_is_cw vertices) && not (vertex_array_is_ccw vertices)

(* replicating a nice idea from Mathematica *)
let map_indexed f lst =
    let rec m_i_aux f lst acc =
        match lst with
            |[] -> []
            |x :: xs -> f x acc :: m_i_aux f xs (acc+1) in
    m_i_aux f lst 0

let iter_indexed f lst =
    let rec i_i_aux f lst acc =
        match lst with
            |[] -> ()
            |x :: xs -> f x acc; i_i_aux f xs (acc+1) in
    i_i_aux f lst 0

let array_fold_left_indexed f init arr =
    let (x, y) = Array.fold_left (fun (x, i) y ->
        (f x y i, i+1)) (init, 0) arr in x

(* my favorite piece of code ever, takes a function 'a -> 'b and returns a
 * function 'a -> 'b that incorporates a memoization layer *)
let memoize f =
    let h = Hashtbl.create 0 in
    fun x ->
        try Hashtbl.find h x
        with Not_found ->
            let v = f x in
            Hashtbl.add h x v;
            v

(* conversion between color indexing formats *)
let hsv_to_rgb (h, s, v) = 
    if s = 0.0 then (v, v, v) else
    let h = h *. 6. in
    let i = floor h in
    let f = floor (h -. i) /. 6.0 in
    let p = v *. (1. -. s) in
    let q = v *. (1. -. s *. f) in
    let t = v *. (1. -. s *. (1. -. f)) in
    match int_of_float i with
        |0 -> (v, t, p)
        |1 -> (q, v, p)
        |2 -> (p, v, t)
        |3 -> (p, q, v)
        |4 -> (t, p, v)
        |_ -> (v, p, q) (* case 5 *)
let hsv_to_rgb = memoize hsv_to_rgb (*memoize this bitch*)

let nub list =
    List.fold_left (fun x y ->
            if List.mem y x then x else y :: x) [] list
        |> List.rev

let rec list_pos list target =
    match list with
        |x :: xs -> if x = target then 1 else 1 + list_pos xs target
        |[] -> raise (Failure target)
