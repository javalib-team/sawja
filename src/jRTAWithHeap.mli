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
open JProgram
open JBasics

(*[parse_program dump classpath main]: Like RTA but instead of starting from
* some java internal entrypoints, use an initial concrete heap as starting
* point for initialized class. [dump] contains this initial heap (obtained
* using HeapInspector). [classpath] is the path of java class, and main is the
* main entrypoint method. *)
val parse_program: ParserType.parsed_heap -> string -> class_method_signature -> 
  JCode.jcode program * JCode.jcode class_node ClassMap.t
