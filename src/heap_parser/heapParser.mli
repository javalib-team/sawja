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


