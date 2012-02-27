(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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

(** Utility module for printing XML documents.*)

val replace_forb_xml_ch : ?repl_amp:bool -> string -> string

val mkdirp : string -> Unix.file_perm -> unit

type xml_tree =
    CustomTag of string * xml_tree list * string
  | SimpleTag of string
  | PCData of string
  | CData of string

val gen_custom_tag :
  string -> (string * string) list -> xml_tree list -> xml_tree

val gen_simple_tag : string -> (string * string) list -> xml_tree

val create_package_dir : string -> string list -> unit

val print_xml_tree_ext :
  ?br:bool -> ?spc:int -> xml_tree -> 'a IO.output -> unit

val print_xml_tree : ?spc:int -> xml_tree -> out_channel -> unit

(**/**)
module JCodeUtil :
  sig
    val iter_code :
      (int -> Javalib_pack.JCode.jopcode list -> unit) ->
      Javalib_pack.JCode.jcode -> unit
    val method_param_names :
      Javalib_pack.JCode.jcode Javalib_pack.Javalib.interface_or_class ->
      Javalib_pack.JBasics.method_signature -> string list option
  end

module type CodeSig  =
sig

  type var

  val var_equal: var -> var -> bool
  val var_name_debug: var -> string option
  val var_name: var -> string
  val var_name_g: var -> string
  val bc_num: var -> int option

  module VarSet : Javalib_pack.JBasics.GenericSetSig with type elt = var
  module VarMap : Javalib_pack.JBasics.GenericMapSig with type key = var

  type instr

  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : Javalib_pack.JBasics.class_name option;
    e_catch_var : var
  }

  type t

  val print_handler : exception_handler -> string

  val jump_target : t -> bool array
    
  val get_source_line_number : int -> t -> int option

  val exception_edges : t -> (int * exception_handler) list

  val vars : t -> var array
  val params : t -> (Javalib_pack.JBasics.value_type * var) list
  val code : t -> instr array
  val exc_tbl : t -> exception_handler list
  val line_number_table : t -> (int * int) list option
  val pc_bc2ir : t -> int Ptmap.t
  val pc_ir2bc : t -> int array
  val print : t -> string list

end

module IRUtil (Code:CodeSig) : 
sig
  val iter_code : (int -> Code.instr list -> unit) -> Code.t Lazy.t -> unit
  val method_param_names :
    Code.t Javalib_pack.Javalib.interface_or_class ->
    Javalib_pack.JBasics.method_signature -> string list option
end
(**/**)

