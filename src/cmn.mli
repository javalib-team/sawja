(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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

(** Only for internal purpose: Common code for intermediate
    representations JBir and A3Bir.*)

open Javalib_pack
open JBasics

(** Common "variable" type and functions signature for all IR representations that could be used for functors depending on variable type.*)
module type VarSig = 
sig
  type var
  val var_equal: var -> var -> bool
  val var_name_debug: var -> string option
  val var_name: var -> string
  val var_name_g: var -> string
  val bc_num: var -> int option

  module VarSet : Javalib_pack.JBasics.GenericSetSig with type elt = var
  module VarMap : Javalib_pack.JBasics.GenericMapSig with type key = var
end

(** Common "variable" type and functions for JBir and A3Bir *)
module Var : sig

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

  val var_ssa : var -> bool

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

  (** [make_var d v] return the indexified version of [v]. Add it to [d] if necessary. *)
  val make_var : dictionary -> unindexed_var -> var

  (** [make_array_var d] build the array of all variables that appear in [d].
      [vars.(i)] is the variable of index [i]. *)
  val make_array_var : dictionary -> var array

  (** This module allows to build efficient sets of [var] values. *)
  module VarSet : Javalib_pack.JBasics.GenericSetSig with type elt = var

  (** This module allows to build maps of elements indexed by [var] values. *)
  module VarMap : Javalib_pack.JBasics.GenericMapSig with type key = var

end

(** Common exception signature depending on "variable" type*)
module type ExceptionSig = 
sig
  type var_e
  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : class_name option;
    e_catch_var : var_e
  }

  (** [print_handler exc] returns a string representation for
      exception handler [exc]. *)
  val print_handler : exception_handler -> string
end

(** Common exception type and functions depending on "variable" type *)
module Exception : functor (Vars:VarSig) ->  
sig
  type var_e = Vars.var
  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : class_name option;
    e_catch_var : Vars.var
  }
  val exception_edges': 'a array -> exception_handler list -> (int * exception_handler) list
  val print_handler : exception_handler -> string
end

(**Common representation of instructions*)
module type InstrSig = 
  sig
    type instr
    val print_instr: ?show_type:bool -> instr -> string
    val instr_jump_to: instr -> int option
  end

(** Common types and functions that does not depend on "variable" type
    for all IRS *)
module Common : sig 

  type mode = Normal | Flat | Addr3

  type const = JCode.jconst

  type conv = | I2L  | I2F  | I2D  | L2I  | L2F  | L2D  | F2I  | F2L  | F2D | D2I  | D2L  | D2F | I2B  | I2C  | I2S

  type unop =
      Neg of JBasics.jvm_basic_type
    | Conv of conv
    | ArrayLength
    | InstanceOf of JBasics.object_type
    | Cast of JBasics.object_type

  type comp =  DG | DL | FG | FL | L

  type binop =
    | ArrayLoad of JBasics.value_type
    | Add of JBasics.jvm_basic_type
    | Sub of JBasics.jvm_basic_type
    | Mult of JBasics.jvm_basic_type
    | Div of JBasics.jvm_basic_type
    | Rem of JBasics.jvm_basic_type
    | IShl | IShr  | IAnd | IOr  | IXor | IUshr
    | LShl | LShr | LAnd | LOr | LXor | LUshr
    | CMP of comp

  (* Type transformation *)

val basic_to_num : JBasics.jvm_basic_type ->
  [> `Double | `Float | `Int | `Long ]

(* Printing functions *)

val print_const : JCode.jconst -> string

val print_unop : unop -> string

val print_binop : binop -> string

val print_typ : JBasics.value_type -> string

val print_list_sep: string -> ('a -> string) -> 'a list -> string

val print_list_sep_list: string -> ('a -> string) -> 'a list -> string list

val print_field: ?long_fields:bool ->
  Javalib_pack.JBasics.class_name ->
  Javalib_pack.JBasics.field_signature -> string

val bracket: bool -> string -> string

end

(** The lowest common interface for the code of all IRs: obtained by
    adding accessors to the representation code fields*)
module type CodeSig  =
sig

  include VarSig

  type instr

  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : JBasics.class_name option;
    e_catch_var : var
  }

  type t

  val print_handler : exception_handler -> string

  val jump_target : t -> bool array
    
  val get_source_line_number : int -> t -> int option

  val exception_edges : t -> (int * exception_handler) list

  module Internal :
  sig
    val vars : t -> var array
    val params : t -> (JBasics.value_type * var) list
    val code : t -> instr array
    val exc_tbl : t -> exception_handler list
    val line_number_table : t -> (int * int) list option
    val pc_bc2ir : t -> int Ptmap.t
    val pc_ir2bc : t -> int array
    val print_simple : t -> string list
  end

end
