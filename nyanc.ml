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

let rec check_nesting_level ?(level=0) source =
    match source with
    | [] -> level
    | hd :: tl -> check_nesting_level tl ~level:(update_nesting_level hd level)

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

            let translated_source = String.concat "\n" (neko_of_ops pseudo_ast) in
            let output = prologue ^ translated_source in
            print_string output
        end
