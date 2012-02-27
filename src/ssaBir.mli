(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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
open Javalib

(** Common code for SSA representations*)

(** Signature of IR to transform in SSA form*)
module type IRSig = sig
  (** Abstract data type for variables *)
  type var

  (** [var_equal v1 v2] is equivalent to [v1 = v2], but is faster.  *)
  val var_equal : var -> var -> bool

  (** [var_orig v] is [true] if and only if the variable [v] was already used at
      bytecode level. *)
  val var_orig : var -> bool

  (** Used only for internal transformations. *)
  val var_ssa : var -> bool

  (** [var_name v] returns a string representation of the variable [v]. *)
  val var_name : var -> string

  (** [var_name_debug v] returns, if possible, the original variable name of [v], 
      if the initial class was compiled using debug information. *)
  val var_name_debug : var -> string option

  (** [var_name_g v] returns a string representation of the variable [v]. 
      If the initial class was compiled using debug information, original 
      variable names are build on this information. It is equivalent to
      [var_name_g x = match var_name_debug with Some s -> s | _ -> var_name x] *)
  val var_name_g : var -> string

  (** [bc_num v] returns the local var number if the variable comes from the initial bytecode program. *)
  val bc_num : var -> int option

  (** [index v] returns the hash value of the given variable. *)
  val index : var -> int

  type instr

  val print_instr : ?show_type:bool -> instr -> string

  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : JBasics.class_name option;
    e_catch_var : var
  }

  (** [t] is the parameter type for JBir methods. *)
  type t = {
    vars : var array;  
    (** All variables that appear in the method. [vars.(i)] is the variable of
	index [i]. *)
    params : (JBasics.value_type * var) list;
    (** [params] contains the method parameters (including the receiver this for
	virtual methods). *)
    code : instr array;
    (** Array of instructions the immediate successor of [pc] is [pc+1].  Jumps
	are absolute. *)
    exc_tbl : exception_handler list;
    (** [exc_tbl] is the exception table of the method code. Jumps are
	absolute. *)
    line_number_table : (int * int) list option;
    (** [line_number_table] contains debug information. It is a list of pairs
	[(i,j)] where [i] indicates the index into the bytecode array at which the
	code for a new line [j] in the original source file begins.  *)
    pc_bc2ir : int Ptmap.t;
    (** map from bytecode code line to ir code line (very sparse). *)
    pc_ir2bc : int array; 
    (** map from ir code line to bytecode code line *)
  }

  (** [jump_target m] indicates whether program points are join points or not in [m]. *)
  val jump_target : t -> bool array

  (** [exception_edges m] returns a list of edges [(i,e);...] where
      [i] is an instruction index in [m] and [e] is a handler whose
      range contains [i]. *)
  val exception_edges :  t -> (int * exception_handler) list

end
(*
(** Common "variable" type and functions signature for SSA form *)
module type VarSig =
sig
  type ir_var
  type var = int * (ir_var * int)
  val var_equal : var -> var -> bool
  val var_orig : var -> bool
  val var_name_debug: var -> string option
  val var_name: var -> string
  val var_name_g: var -> string
  val bc_num: var -> int option
  val var_origin : var -> ir_var
  val var_ssa_index : var -> int
  val index : var -> int
  type dictionary
  val make_dictionary : unit -> dictionary
  val make_var : dictionary -> ir_var -> int -> var
  val make_array_var : dictionary -> ir_var -> var array  
  module VarSet : Javalib_pack.JBasics.GenericSetSig with type elt = int * (ir_var * int)
  module VarMap : Javalib_pack.JBasics.GenericMapSig with type key = int * (ir_var * int)  
end

(** Functor to create "variable" type and functions for SSA form from
    IR*)
module Var (IR:IRSig) : VarSig with type ir_var = IR.var

(** Common code represenation types for SSA forms *)
module type TSsaSig = 
sig
  type var_t
  type var_set
  type instr_t
 
  type phi_node = {
    def : var_t;
    (** The variable defined in the phi node*)
    use : var_t array;
    (** Array of used variable in the phi node, the index of a used
	variable in the array corresponds to the index of the program
	point predecessor in [preds.(phi_node_pc)].*)
    use_set : var_set;
    (** Set of used variable in the phi node (no information on
	predecessor program point for a used variable)*)
  }  

  type t = {
    vars : var_t array;  
  (** All variables that appear in the method. [vars.(i)] is the variable of
      index [i]. *)
    params : (JBasics.value_type * var_t) list;
    (** [params] contains the method parameters (including the receiver this for
	virtual methods). *)
    code : instr_t array;
    (** Array of instructions the immediate successor of [pc] is [pc+1].  Jumps
	are absolute. *)
    preds : (int array) array;
    (** Array of instructions program point that are predecessors of
      instruction [pc]. *)
    phi_nodes : (phi_node list) array;
    (** Array of phi nodes assignments. Each phi nodes assignments at point [pc] must
	be executed before the corresponding [code.(pc)] instruction. *)
    exc_tbl : exception_handler list;
    (** [exc_tbl] is the exception table of the method code. Jumps are
	absolute. *)
    line_number_table : (int * int) list option;
    (** [line_number_table] contains debug information. It is a list of pairs
	[(i,j)] where [i] indicates the index into the bytecode array at which the
	code for a new line [j] in the original source file begins.  *)
    pc_bc2ir : int Ptmap.t;
    (** map from bytecode code line to ir code line (very sparse). *)
    pc_ir2bc : int array; 
    (** map from ir code line to bytecode code line *)
  } 

    val jump_target : t -> bool array

    (** [print_phi_node phi] returns a string representation for phi node [phi]. *)
    val print_phi_node : ?phi_simpl:bool -> phi_node -> string

    (** [print_phi_nodes phi_list] returns a string representation for phi nodes 
	[phi_list]. *)
    val print_phi_nodes : ?phi_simpl:bool -> phi_node list -> string

    (** [print c] returns a list of string representations for instruction of [c]
	(one string for each program point of the code [c]). *)
    val print : ?phi_simpl:bool -> t -> string list
      
    (** [exception_edges m] returns a list of edges [(i,e);...] where
	[i] is an instruction index in [m] and [e] is a handler whose
	range contains [i]. *)
    val exception_edges :  t -> (int * exception_handler) list

    (** [get_source_line_number pc m] returns the source line number corresponding
	the program point [pp] of the method code [m].  The line number give a rough
	idea and may be wrong.  It uses the field [t.pc_ir2bc] of the code
	representation and the attribute LineNumberTable (cf. JVMS §4.7.8).*)
    val get_source_line_number : int -> t -> int option

end 

(** Functor to create code representation from SSA "variable" and "instruction" *)
module T (Var : VarSig) 
  (Instr : Cmn.InstrSig) 
  : sig
    type var_t = Var.var
    type instr_t = Instr.instr
    type var_set = Var.VarSet.t
    include Cmn.ExceptionSig with type var_e = var_t
    type phi_node = {
      def : Var.var;
      (** The variable defined in the phi node*)
      use : Var.var array;
      (** Array of used variables in the phi node, the index of a used
	  variable in the array corresponds to the index of the program
	  point predecessor in [preds.(phi_node_pc)].*)
      use_set : Var.VarSet.t;
      (** Set of used variables in the phi node (no information on
	  predecessor program point for a used variable)*)
    }
    type t = {
      vars : Var.var array;
      params : (JBasics.value_type * Var.var) list;
      code : Instr.instr array;
      preds : (int array) array;
      (** Array of instructions program point that are predecessors of
	  instruction [pc]. *)
      phi_nodes : (phi_node list) array;
      (** Array of phi nodes assignments. Each phi nodes assignments at point [pc] must
	  be executed before the corresponding [code.(pc)] instruction. *)
      exc_tbl : exception_handler list;
      line_number_table : (int * int) list option;
      pc_bc2ir : int Ptmap.t;
      pc_ir2bc : int array; 
    }

    val jump_target : t -> bool array

    (** [print_phi_node phi] returns a string representation for phi node [phi]. *)
    val print_phi_node : ?phi_simpl:bool -> phi_node -> string

    (** [print_phi_nodes phi_list] returns a string representation for phi nodes 
	[phi_list]. *)
    val print_phi_nodes : ?phi_simpl:bool -> phi_node list -> string

    (** [print c] returns a list of string representations for instruction of [c]
	(one string for each program point of the code [c]). *)
    val print : ?phi_simpl:bool -> t -> string list

    (** [print_simple c] same fun as print with phi_simpl = true (for
	compatibility with non-SSA representations). *)
    val print_simple : t -> string list
      
    (** [exception_edges m] returns a list of edges [(i,e);...] where
	[i] is an instruction index in [m] and [e] is a handler whose
	range contains [i]. *)
    val exception_edges :  t -> (int * exception_handler) list

    (** [get_source_line_number pc m] returns the source line number corresponding
	the program point [pp] of the method code [m].  The line number give a rough
	idea and may be wrong.  It uses the field [t.pc_ir2bc] of the code
	representation and the attribute LineNumberTable (cf. JVMS §4.7.8).*)
    val get_source_line_number : int -> t -> int option

    val vars : t -> Var.var array
    val params : t -> (JBasics.value_type * Var.var) list
    val code : t -> Instr.instr array
    val exc_tbl : t -> exception_handler list
    val line_number_table : t -> (int * int) list option
    val pc_bc2ir : t -> int Ptmap.t
    val pc_ir2bc : t -> int array
  end


(** Signature of type and function to provide in order to transform IR
in SSA form*)
module type IR2SsaSig = sig
  type ir_t
  type ir_var
  type ir_instr
  type ir_exc_h
  type ssa_var
  type ssa_instr
  type ssa_exc_h
  val use_bcvars : ir_instr -> Ptset.t
  val def_bcvar : ir_instr -> Ptset.t
  val var_defs : ir_t -> Ptset.t Ptmap.t
  val map_instr : (ir_var -> ssa_var) -> (ir_var -> ssa_var) -> ir_instr -> ssa_instr
  val map_exception_handler : 
(ir_var -> int -> ssa_var) -> ir_exc_h -> ssa_exc_h
  val preds : ir_t -> int -> int list
  val succs : ir_t -> int -> int list
  val live_analysis : ir_t -> int -> ir_var -> bool
end

(** Functor that provides the transformation function *)
module SSA 
  (IR:IRSig) 
  (Var:VarSig 
   with type ir_var = IR.var
   and type var = int * (IR.var * int))
  (TSSA:TSsaSig 
   with type var_t = Var.var
   and type var_set = Var.VarSet.t)
  (IR2SSA:IR2SsaSig 
   with type ir_t = IR.t
   and type ir_var = IR.var
   and type ir_instr = IR.instr
   and type ir_exc_h = IR.exception_handler
   and type ssa_var = Var.var
   and type ssa_instr = TSSA.instr_t
   and type ssa_exc_h = TSSA.exception_handler
  )
  : 
sig
  val transform_from_ir : IR.t -> TSSA.t
end
*)
