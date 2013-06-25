open Javalib_pack
open JBasics
open JProgram
open ParserType
exception InvalidFile  

(*See type in ParserType. *)


(** Read a file containing an heap dump and return the parsed heap.
  * Raise Not_found exception if the file cannot be opened and InvalidFile
  * exception if the file does not appear to have the correct format (parsing
  * failed).
* *)
val get_init_heap : string -> parsed_heap


