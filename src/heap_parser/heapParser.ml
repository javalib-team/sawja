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



