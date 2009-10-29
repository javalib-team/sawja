(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

open JBasics
open Javalib

type t = {
  super_classes : class_name list ClassMap.t;
  classpath : class_path
}

(* let make cp cns = *)

let rec get_super_classes classpath map cn =
  try
    (ClassMap.find cn map,map)
  with
    | Not_found -> begin
	let ioc = get_class classpath cn in
	  match ioc with
	    | JClass c ->
		(match c.c_super_class with
		   | None -> ([],ClassMap.add cn [] map)
		   | Some sc ->
		       let (ss_classes,map) =
			 get_super_classes classpath map sc in
		       let super_classes = cn :: ss_classes in
			 (super_classes,ClassMap.add cn super_classes map))
	    | JInterface _ -> get_super_classes classpath map java_lang_object
      end

let make classpath cn_list =
  {
    super_classes = List.fold_left
		      (fun map cn -> snd (get_super_classes classpath map cn))
		      ClassMap.empty
		      cn_list;
    classpath = classpath
  }

let add ph cn =
  let (_,map) = get_super_classes ph.classpath ph.super_classes cn in
    { super_classes = map; classpath = ph.classpath }

exception UnknownClass of class_name
let is_sub_class ph cn1 cn2 =
  cn_equal cn1 cn2 ||
    try List.exists (cn_equal cn1) (ClassMap.find cn1 ph.super_classes)
    with Not_found -> raise (UnknownClass cn1)
