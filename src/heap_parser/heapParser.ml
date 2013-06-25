open Javalib_pack
open JBasics
open JProgram
open Javalib
open ParserType 
exception InvalidFile  


(*Parse heap file and return an heap object.*)
let get_init_heap heap_file =
  let dump_input = 
    try open_in heap_file 
    with Sys_error _ -> raise Not_found
  in
    try 
      let lexbuf = Lexing.from_channel dump_input in
        Parse_heap.input Lex_heap.token lexbuf
    with _ -> raise InvalidFile



