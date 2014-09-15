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

(* Read source and break it into tokens *)
let read_source filename = 
    let tokens = ref [] in
    let chan = open_in filename in
    try
        while true do
            let line = input_line chan in
            let line_tokens = Str.split (Str.regexp " ") line in 
            tokens := !tokens @ line_tokens
        done; []
    with End_of_file ->
      close_in chan;
    !tokens

(* Nesting level checking stuff. We don't track positions here,
   just check if the number of left brackets equals the number
   or right ones. *)
let update_nesting_level token level =
    match token with
    | "nyaaaan" -> level + 1
    | "Nyaaaan" -> level - 1
    | _         -> level

let rec check_nesting_level ?(level=0) source =
    match source with
    | [] -> level
    | hd :: tl -> check_nesting_level tl ~level:(update_nesting_level hd level)

let translate_token token =
    match token with
    | "nyan"    -> "ptr += 1;"
    | "Nyan"    -> "ptr -= 1;"
    | "nyaan"   -> "tape[ptr] += 1;"
    | "Nyaan"   -> "tape[ptr] -= 1;"
    | "nyaaan"  -> "$sset(output,0,tape[ptr]); $print(output);"
    | "Nyaaan"  -> "try tape[ptr] = file_read_char(file_stdin()) catch e 0; ;"
    | "nyaaaan" -> "while (tape[ptr] > 0) {"
    | "Nyaaaan" -> "}"
    | _         -> ""

let rec translate source =
    match source with
    | [] -> []
    | hd :: tl -> translate_token hd :: translate tl

let () =
    let source = read_source Sys.argv.(1) in
    if check_nesting_level source > 0 then
        failwith "Unmatched bracket, nyan!";

    let translated_source = String.concat "\n" (translate source) in
    let output = prologue ^ translated_source in
    print_string output
