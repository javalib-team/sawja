(*
 * This file is part of SAWJA
 * Copyright (c)2013 Pierre Vittet (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

{
(*Start of lexer.*)
  open Parse_heap


  (*from parsed array index, return the array index as integer*)
let get_array_idx str_ar = 
  let str_len = String.length str_ar in
  match (String.get str_ar 0, String.get str_ar (str_len -1)) with
    | ('[',']') -> int_of_string (String.sub str_ar 1 (str_len-2))
    | _ -> failwith "invalid array index"
}

let letter = ['a'-'z' 'A'-'Z' '_' ] 

let digit  = ['0'-'9']

let digit16  = ['a'-'f' 'A'-'F' '0'-'9']

let ident = (letter|digit | "$" | ".")+

let classId = (ident* '.')* letter*
  
let space = [ ' ' '\n'  '\t' '\r']+

let number = '-'? digit+

let hexNum = "0X" digit16+

rule token = parse
  (*file sections*)
  | "~~CLASSES~~" {ClassPart}
  | "~~ARRAYS~~" {ArrayPart}
  (*general*)
  | space {token lexbuf}
  | "{" {Open}
  | "}" {Close}
  | "class"  {Class}
(*   | classId {ClassId (Lexing.lexeme lexbuf)} *)
  | "=" {Eq}
  | ";" {SemiCol}
  | number as n {Num (Int64.of_string n)}
  | hexNum as n {HexNum (Int64.of_string n)}
  (*class oriented*)
  (*field*)
  | "static" {Static}
  | "long" {TLong}
  | "short" {TShort}
  | "double" {TDouble}
  | "float" {TFloat}
  | "int" {TInt}
  | "bool" {TBool}
  | "byte" {TByte}
  | "char" {TChar}
  | "[]" {ArrayType}
  | '[' number ']' {ArrayIdx (get_array_idx (Lexing.lexeme lexbuf))} (*can also be a size*)
  | ident {Ident (Lexing.lexeme lexbuf)}
  (**)
  | eof {Eof}
  | _ as c {Printf.printf "Error %c\n" c; exit 1}



{
(*end of lexer*)
}
