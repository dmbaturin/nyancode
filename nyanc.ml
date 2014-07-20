(* NekoVM arrays are immutable. Think about it,
   immutable arrays. String are mutable though,
   so we'll use a string instead and initialize it
   with zeroes. *)
let prologue = "\
var c = $loader.loadmodule(\"chr\",$loader);
var a = $smake(9999);\n\
var i = 0;\n\
while(i<=9999) {\n\
    $sset(a,i,0);\n\
    i ++=1;\n\
}\n\
ptr = 0;\n\
value = 0;\n\
"
;;

(* Joins two lists *)
let rec append_list l1 l2 = 
    match l1 with
    | [] -> l2
    | head :: tail -> head :: (append_list tail l2)
;;

(* Read source and break it into tokens *)
let read_source filename = 
    let tokens = ref [] in
    let chan = open_in filename in
    try
        while true do
            let line = input_line chan in
            let line_tokens = Str.split (Str.regexp " ") line in 
            tokens := append_list !tokens line_tokens
        done; []
    with End_of_file ->
      close_in chan;
    !tokens
;;

(* Nesting level checking stuff. We don't track positions here,
   just check if the number of left brackets equals the number
   or right ones. *)
let update_nesting_level token level =
    match token with
    | "nyaaaan" -> level + 1
    | "Nyaaaan" -> level - 1
    | _         -> level
;;

let rec check_nesting_level ?(level=0) source =
    match source with
    | [] -> level
    | hd :: tl -> check_nesting_level tl ~level:(update_nesting_level hd level)
;;

(* NekoVM docs mention sys_getch() function, but I couldn't get it
   to work. If anyone knows the right incantation, please tell,
   right now "," is nop here.*)
let translate_token token =
    match token with
    | "nyan"    -> "ptr ++= 1;"
    | "Nyan"    -> "ptr --= 1;"
    | "nyaan"   -> "value=$sget(a,ptr); value ++=1; $sset(a,ptr,value); value=0;"
    | "Nyaan"   -> "value=$sget(a,ptr); value --=1; $sset(a,ptr,value); value=0;"
    | "nyaaan"  -> "$print(c.chr($sget(a,ptr)));"
    | "Nyaaan"  -> ""
    | "nyaaaan" -> "while ($sget(a,ptr) > 0) {"
    | "Nyaaaan" -> "}"
    | _         -> ""
;;

let rec translate source =
    match source with
    | [] -> []
    | head :: tail -> translate_token head :: translate tail
;;


let source = read_source Sys.argv.(1) in
    if check_nesting_level source > 0 then
        failwith "Unmatched bracket, nyan!";
    let translated_source = String.concat "\n" (translate source) in
    let output = String.concat "" [prologue; translated_source] in
    print_string output
;;
