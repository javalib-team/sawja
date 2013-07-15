open Javalib_pack 
open JProgram
open JBasics

val parse_program: string -> string -> class_method_signature -> JCode.jcode program * JCode.jcode class_node ClassMap.t
