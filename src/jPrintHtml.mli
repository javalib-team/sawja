(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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

(** Pretty-Html-printer for high level programs. *)

open JBasics
open Javalib
open JProgram

(** This module allows to generate a web site from high level programs.
    This web site can be used for any kind of visualisation or
    debbuging purposes. Annotations can be attached to the program and
    will be displayed properly (according to a given css). *)
  
(** {2 Program information.} *)
  
(** This type represents the information that will be printed. *)
type info = {
  p_class : class_name -> string list;
  (** Prints class information that is printed inside the class, along with
      other attributes of the class. *)
  p_field : class_name -> field_signature -> string list;
  (** Prints field information that is printed along with the corresponding
      field. *)
  p_method : class_name -> method_signature -> string list;
  (** Prints method information that is printed inside the method,
      along with other attributes of the method. *)
  p_pp : class_name -> method_signature -> int -> string list;
  (** Prints information associated to program points. The information is
      printed after the instruction. *)
}
    
(** [void_info] is an instance of [info] that does not print anything
    nor filter anything. *)
val void_info : info
  

(** {2 Printing a JCode.jcode program.} *)

val print_program : ?css:string -> ?js:string -> ?info:info
  -> JCode.jcode program -> string -> unit

(** {2 Printing a JBir.t program.} *)

val print_jbir_program : ?css:string -> ?js:string -> ?info:info
  -> JBir.t program -> string -> unit

(** {2 Building a Printer for any program representation.} *)

(* type html_tree *)
  
type param

val simple_param : string -> param

val value_param : ?dim:int -> 'a program -> value_type -> class_name -> param

val get_param : ?called_cname:string -> 'a program -> class_name -> class_name
  -> field_signature -> param

val invoke_param : ?called_cname:string -> ?called_mname:string -> 'a program ->
  class_name -> method_signature -> invoke -> param

module type HTMLPrinter =
sig
  type code
    
  val css:string
  val js:string

  (** [print_program ~css ~js ~info program
      outputdir] generates html files representing the program [p] in
      the output directory [outputdir], given the annotation
      information [info] ([void_info] by default), an optional
      Cascading Style Sheet (CSS) [css] and an optional JavaScript
      file [js]. If [css] or [js] is not provided, {!css} and {!js}
      are used when [css] or [js] is not provided. @raise Sys_error if
      the output directory [outputdir] does not exist. @raise
      Invalid_argument if the name corresponding to [outputdir] is
      a file.
  *)
  val print_program :
    ?css:string -> ?js:string -> ?info:info -> code program -> string -> unit
end

module type PrintInterface =
sig
  type instr
  type code
  val print_instr : instr -> string
  val iter_code : (int -> instr -> unit) -> code Lazy.t -> unit
  val method_param_names : code program -> class_name -> method_signature
    -> string list option
  val inst_html : code program -> class_name -> method_signature -> int
    -> instr -> param list option
  val get_callgraph : code program -> callgraph
end

module Make (S : PrintInterface) : HTMLPrinter
