type nyan_op =
    | VInc
    | VDec
    | PInc
    | PDec
    | VWrite
    | VRead
    | LoopStart
    | LoopEnd
    | Nop
    | PIncMulti of int
    | PDecMulti of int
    | VIncMulti of int
    | VDecMulti of int

(* NekoVM program prologue prepended to all programs. *)
let prologue = "\
file_read_char = $loader.loadprim(\"std@file_read_char\",1);\n\
file_stdin = $loader.loadprim(\"std@file_stdin\",0);\n\
tape = $amake(9999);\n\
var i = 0;\n\
while(i<=9999) {\n\
    tape[i] = 0;\n\
    i ++=1;\n\
}\n\
ptr = 0;\n\
value = 0;\n\
output = $smake(1);
"

(* Nesting level checking stuff. We don't track positions here,
   just check if the number of left brackets equals the number
   or right ones. *)
let update_nesting_level token level =
    match token with
    | LoopStart -> level + 1
    | LoopEnd -> level - 1
    | _         -> level

let check_nesting_level ast =
    let rec loop ast level =
        match ast with
        | [] -> level
        | hd :: tl -> loop tl (update_nesting_level hd level)
    in loop ast 0

let neko_of_op op =
    match op with
    | PInc    -> "ptr += 1;"
    | PDec    -> "ptr -= 1;"
    | VInc   -> "tape[ptr] += 1;"
    | VDec   -> "tape[ptr] -= 1;"
    | VWrite  -> "$sset(output,0,tape[ptr]); $print(output);"
    | VRead  -> "try tape[ptr] = file_read_char(file_stdin()) catch e 0; ;"
    | LoopStart -> "while (tape[ptr] > 0) {"
    | LoopEnd -> "}"
    | PIncMulti x -> "ptr += " ^ (string_of_int x) ^ ";"
    | PDecMulti x -> "ptr -= " ^ (string_of_int x) ^ ";"
    | VIncMulti x -> "tape[ptr] += " ^ (string_of_int x) ^ ";"
    | VDecMulti x -> "tape[ptr] -= " ^ (string_of_int x) ^ ";"
    | _         -> ""

let op_of_token token =
    match token with
    | "nyan"    -> PInc
    | "Nyan"    -> PDec
    | "nyaan"   -> VInc
    | "Nyaan"   -> VDec
    | "nyaaan"  -> VWrite
    | "Nyaaan"  -> VRead
    | "nyaaaan" -> LoopStart
    | "Nyaaaan" -> LoopEnd
    | _         -> Nop

let ops_of_tokens words =
    let rec loop words acc =
        match words with
        | [] -> acc
        | x :: xs ->
            let op = op_of_token x in
            if op = Nop then loop xs acc
            else loop xs (op :: acc)
    in List.rev (loop words [])

let neko_of_ops ops =
    List.map neko_of_op ops

(* Replace sequences of some operation with
   a single compound operation *)
let replace_subsequence op make_op_fun ops =
    let rec loop ops count acc =
        match ops with
        | [] -> acc
        | [x] -> x :: acc
        | x :: x' :: xs ->
            if (x = x') && (x = op) then
                (* We are inside a sequence *)
                if xs = [] then
                    (* ...but the end of the sequence is also the end
                       of the list. Account for the two last items *)
                    loop xs 0 ((make_op_fun (count + 2)) :: acc)
                else
                    (* There are more items *)
                    loop (x' :: xs) (count + 1) acc
            else
                if count > 0 then
                    (* We have just exited a sequence *)
                    loop (x' :: xs) 0 ((make_op_fun (count + 1)) :: acc)
                else
                    (* We weren't even in a sequence *)
                    loop (x' :: xs) 0 (x :: acc)
    in List.rev (loop ops 0 [])

let apply_optimizations ops =
    replace_subsequence PInc (fun x -> PIncMulti x) ops |>
    replace_subsequence PDec (fun x -> PDecMulti x)     |>
    replace_subsequence VInc (fun x -> VIncMulti x)     |>
    replace_subsequence VDec (fun x -> VDecMulti x)     |>
    replace_subsequence VRead (fun x -> VRead)

let error msg =
    print_endline msg;
    exit 1

(* Read source and break it into tokens *)
let read_source filename =
    let ops = ref [] in
    let chan = open_in filename in
    try
        while true do
            let line = input_line chan in
            let line_tokens = Str.split (Str.regexp " ") line in
            let line_ops = ops_of_tokens line_tokens in
            ops := !ops @ line_ops
        done; []
    with End_of_file ->
      close_in chan;
    !ops

let () =
    if (Array.length Sys.argv) <= 1 then error "Specify the file, nyan!"
    else
        begin
            let pseudo_ast = read_source Sys.argv.(1) in
            if check_nesting_level pseudo_ast > 0 then
            error "Unmatched bracket, nyan!";

            let optimized = apply_optimizations pseudo_ast in
            let translated_source = String.concat "\n" (neko_of_ops optimized) in
            let output = prologue ^ translated_source in
            print_string output
        end
