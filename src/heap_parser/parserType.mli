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
open JType

(** {2 Final type } *)
type reference = 
  | Null 
  | Ref of int

type parser_value =
  | VInt of int32
  | VChar of int
  | VShort of int
  | VBool of int
  | VByte of int
  | VLong of int64
  | VFloat of float
  | VDouble of float
  | VObject of (class_name * reference)
  | VArray of value_type * reference 


type class_el ={
  cl_name: class_name;
  cl_static_fields: parser_value FieldMap.t;
  cl_instances: parser_value FieldMap.t Ptmap.t; (**The int key is the instance id.*)
}

type parsed_heap={
  hp_class: class_el ClassMap.t;
  hp_array: parser_value array Ptmap.t ObjectMap.t;
  (**For each array class, a map of instance (key is the instance identifier)
    representing the array.*)
}

val get_dyn_type: int -> object_type

(** {2 Printer. } *)
val parser_value2string: parser_value -> string 
val heap_to_string : parsed_heap -> string

(** {2 Temporary types (only used during parsing } *)
type field = 
{
  fname: string;
  fvalue: parser_value;
}


type instance
(**for array instance.*)
type instance_ar 



(** {2 Temporary function (only used internally during parsing). } *)

val raw_2_ref : Int64.t -> reference
val gen_instance: int64 -> field list -> instance 
val gen_class: string -> field list -> instance list -> class_el 
val add_class: class_el ClassMap.t -> class_el -> class_el ClassMap.t

(**array stuff*)

val gen_instance_ar: int64 -> int -> (int * int64) list -> instance_ar
val add_instance_ar: instance_ar -> instance_ar Ptmap.t -> instance_ar Ptmap.t
val gen_class_ar: string -> int -> instance_ar Ptmap.t -> 
  object_type * parser_value array Ptmap.t

val finalize_class_ar: object_type * parser_value array Ptmap.t -> 
  parser_value array Ptmap.t ObjectMap.t -> parser_value array Ptmap.t ObjectMap.t




