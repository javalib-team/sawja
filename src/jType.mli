(*
 * This file is part of SAWJA
 * Copyright (c)2013 Pierre Vittet (INRIA)
 * Copyright (c)2016 David Pichardie (ENS Rennes)
 * Copyright (c)2016 Laurent Guillo (CNRS)
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

(** It provides some type utilities (mainly subtyping). It uses the JBasics
  * object type, allowing type comparaison and subtyping. Current
  * implementation does not handle subtyping of parameterized types and
  * generics.
*)
open Javalib_pack
open JBasics
open JProgram



val obj_compare : object_type -> object_type -> int 


(** This module allows to build maps of elements indexed by [object_type] values. *)
module ObjectMap : GenericMap.GenericMapSig with type key = object_type


(** This module allows to build sets of [object_type] values. *)
module ObjectSet : GenericSet.GenericSetSig with type elt = object_type


(*[supertype t1 t2] Return true if t1 is a supertype of t2*)
val supertype : 'a program -> object_type -> object_type -> bool
(*[subtype t1 t2] Return true if t1 is a subtype of t2*)
val subtype: 'a program -> object_type -> object_type -> bool

(** Return true of t1 is a direct supertype of t2*)
val direct_supertype : 'a program -> object_type -> object_type -> bool

(** Return true of t1 is a direct subtype of t2*)
val direct_subtype : 'a program -> object_type -> object_type -> bool

(** Return true of t1 is a supertype of t2 using recursively direct_supertype.
* It is used to debugging purpose and supertype function should be preferably
* used for performance reasons.*)
val supertype_from_direct: 'a program -> object_type -> object_type -> bool

(** Return true of t1 is a subtype of t2 using recursively direct_subtype.
* It is used to debugging purpose and subtype function should be preferably
* used for performance reasons.*)
val subtype_from_direct: 'a program -> object_type -> object_type -> bool


