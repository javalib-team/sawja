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
open HeapParser
open ParserType

let parse_program instances_dump classpath cms = 
  let heap = get_init_heap instances_dump in
  let (hp_class,hp_array) = (heap.hp_class,heap.hp_array)
  in
  let instanciated = 
    ClassMap.fold 
      (fun cn cl lst -> 
         match Ptmap.is_empty cl.cl_instances with
           | true -> lst
           | false -> cn::lst) 
      hp_class []
  in
  JRTA.parse_program ~instantiated:instanciated ~other_entrypoints:[] classpath cms



