(*** CamlExt.ml provides a handful of utility functions that extend the
 * functionality of OCaml itself, need no larger context to justify their
 * existence, and may be used frequently throughout the program ***)

(* some functionals *)
let id x = x
let const a = fun x -> a
(* how is this not a standard ocaml function *)
let rec pow n a = if a = 0 then 1 else n * (pow n (a - 1))

(* some haskell wishes *)
let ($) a b =
    let x = a in
    let y = b in
    x y
let (@) a b =
    fun x -> a (b x)

(* reading wider integers out of binary files *)
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

let unsigned_to_signed_16 x =
    let sign = x land 0x8000 in
    let contents = x land 0x7fff in
    if sign != 0 then contents - 0x8000 else contents

let signed_to_unsigned_16 x =
    let x = x land 0xffff in
    if x < 0 then 0x10000 - x else x

let input_signed_word fh =
    unsigned_to_signed_16 (input_word fh)

let output_signed_word fh x =
    output_word fh (signed_to_unsigned_16 x)

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

let rec of_bitflag descriptor x =
    match descriptor with
    |[] -> []
    |(x1, x2) :: xs ->
        if x land x1 != 0
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
            |[] -> raise (Failure "of_enum can't find match!")
            |y :: ys ->
                if x = y then acc
                         else search ys (acc+1) in
    search descriptor start

let of_enum (start, descriptor) x =
    List.nth descriptor (x + start)

let rec output_padding fh n =
    if n = 0 then () else begin
    output_byte fh 0;
    output_padding fh (n - 1)
    end

let output_string_n fh str len =
    output_string fh str;
    ignore (output_padding fh (len - (String.length str)))

let dprint str =
    print_endline str;
    flush stdout

let find_in_array arr obj =
    let length = Array.length arr in
    let rec fia_aux arr obj acc =
        if acc = length then acc else
        if Array.get arr acc = obj
            then acc
            else fia_aux arr obj (acc+1) in
    fia_aux arr obj 0

let delete_from_array arr obj replace =
    let pos = find_in_array arr obj in
    let len = Array.length arr in
    let rec dfa_aux arr replace acc =
        if acc >= pos && acc < len - 1 then begin
            Array.set arr acc (Array.get arr (acc + 1));
            dfa_aux arr replace (acc + 1)
        end else
            Array.set arr acc replace in
    if pos < len then dfa_aux arr replace pos else ()

let destructive_map f arr =
    let len = Array.length arr in
    let rec dm_aux f arr n =
        Array.set arr n (f (Array.get arr n));
        if n = len - 1 then () else dm_aux f arr (n+1) in
    dm_aux f arr 0

let delete_from_array_and_resize arr n =
    let len = Array.length arr in
    Array.append (Array.sub arr 0 n) (Array.sub arr (n+1) (len - n - 1))

let dot (x0, y0) (x1, y1) =
    x0 * x1 + y0 * y1

let dotf (x0, y0) (x1, y1) =
    x0 *. x1 +. y0 *. y1

let crossf (x0, y0) (x1, y1) =
    y0 *. x1 -. x0 *. y1

let cross (x0, y0) (x1, y1) =
    y0 * x1 - x0 * y1

let norm (x, y) =
    (x**2.0 +. y**2.0)**0.5

let distance (x0, y0) (x1, y1) =
    norm ((x1 -. x0), (y1 -. y0))

let rec print_array arr =
    match arr with
        |[] -> print_endline ""; flush stdout
        |x :: xs -> print_int x; print_string ";"; print_array xs

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
    let (x, y) = Array.fold_left (fun (x, i) y -> (f x y i, i+1)) (init, 0) arr in x
