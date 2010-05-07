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

open Javalib_pack

type const =
      [ `ANull
      | `Byte of int
      | `Class of JBasics.object_type
      | `Double of float
      | `Float of float
      | `Int of int32
      | `Long of int64
      | `Short of int
      | `String of string ]

val print_const : const -> string

val varname : string

type unindexed_var =
  | OriginalVar of int * string option (* register number, name (debug if available) *)
  | TempVar of int
  | CatchVar of int
  | BranchVar of int * int
  | BranchVar2 of int * int

type var = int * unindexed_var

(** [var_equal v1 v2] is equivalent to [v1 = v2], but is faster.  *)
val var_equal : var -> var -> bool

(** [var_orig v] is [true] if and only if the variable [v] comes from the
    initial bytecode program. Does not depend on debug information. *)
val var_orig : var -> bool

(** [var_name v] returns the string identifying the variable [v].
    Does not use debug information. *) 
val var_name : var -> string

(** [var_name_debug v] returns, if possible the original variable names of [v],
    if the initial class was compiled using debug information. *)
val var_name_debug : var -> string option

(** [var_name_g v] returns the string identifying the variable [v], according to
    the local variable table provided in the class file from which it has been
    created *)
val var_name_g : var -> string

(** [bc_num v] returns the local var number if the variable comes from the
    initial bytecode program. *)
val bc_num : var -> int option

(** [index v] returns the hash value of the given variable. *)
val index : var -> int

(** mutable dictionary used to give unique index to variables. *)
type dictionary

(** build an empty dictionary. *)
val make_dictionary : unit -> dictionary

(** [make_var d v] return the indexified versino of [v]. Add it to [d] if necessary. *)
val make_var : dictionary -> unindexed_var -> var

(** [make_array_var d] build the array of all variables that appear in [d].
    [vars.(i)] is the variable of index [i]. *)
val make_array_var : dictionary -> var array


type conv = | I2L  | I2F  | I2D  | L2I  | L2F  | L2D  | F2I  | F2L  | F2D | D2I  | D2L  | D2F | I2B  | I2C  | I2S

type unop =
    Neg of JBasics.jvm_basic_type
  | Conv of conv
  | ArrayLength
  | InstanceOf of JBasics.object_type
  | Cast of JBasics.object_type

val print_unop : unop -> string

type comp =  DG | DL | FG | FL | L

val print_typ : JBasics.value_type -> string

(* type statistics = { *)
(*     mutable nb_jump_with_non_empty_stacks : int; *)
(*     mutable nb_back_jump_with_non_empty_stacks : int; *)
(*     mutable nb_store_is_var_in_stack : int; *)
(*     mutable nb_incr_is_var_in_stack : int; *)
(*     mutable nb_arraystore_is_array_access_in_stack : int; *)
(*     mutable nb_putfield_is_field_in_stack : int; *)
(*     mutable nb_putstatic_is_static_in_stack : int; *)
(*     mutable nb_method_call_with_modifiable_in_stack : int; *)
(*     mutable nb_store : int; *)
(*     mutable nb_incr : int; *)
(*     mutable nb_putfield : int; *)
(*     mutable nb_arraystore : int; *)
(*     mutable nb_putstatic : int; *)
(*     mutable nb_method_call : int; *)
(*     mutable nb_tempvar : int; *)
(*     mutable nb_tempvar_branch : int; *)
(*     mutable nb_tempvar_removed : int; *)
(*     mutable nb_tempvar_method_effect : int; *)
(*     mutable nb_tempvar_putfield : int; *)
(*     mutable nb_tempvar_arraystore : int; *)
(*     mutable nb_tempvar_side_effect : int; *)
(*     mutable nb_tempvar_flat : int; *)
(*     mutable nb_tempvar_3a : int; *)
(*   } *)

type mode = Normal | Flat | Addr3

type exception_handler = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : JBasics.class_name option;
	e_catch_var : var
}
