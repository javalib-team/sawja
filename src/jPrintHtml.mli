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

(** [print_program ~css ~js ~info program outputdir] generates html
    files representing the [JCode.jcode] program [p] in the output
    directory [outputdir], given the annotation information [info]
    ([void_info] by default), an optional Cascading Style Sheet (CSS)
    file [css] and an optional JavaScript file [js]. If [css] or [js]
    is not provided, a default CSS or JavaScript file is generated.
    @raise Sys_error if the output directory [outputdir] does not
    exist. @raise Invalid_argument if the name corresponding to
    [outputdir] is a file. *)
val print_program : ?css:string -> ?js:string -> ?info:info
  -> JCode.jcode program -> string -> unit

(** {2 Printing a JBir.t program.} *)

(** [print_program ~css ~js ~info program outputdir] generates html
    files representing the {!JBir.t} program [p] in the output
    directory [outputdir], given the annotation information [info]
    ([void_info] by default), an optional Cascading Style Sheet (CSS)
    file [css] and an optional JavaScript file [js]. If [css] or [js]
    is not provided, a default CSS or JavaScript file is generated.
    @raise Sys_error if the output directory [outputdir] does not
    exist. @raise Invalid_argument if the name corresponding to
    [outputdir] is a file. *)
val print_jbir_program : ?css:string -> ?js:string -> ?info:info
  -> JBir.t program -> string -> unit

(** {2 Building a Printer for any program representation.} *)

(** Abstract type representing basic elements to build html instructions. *)
type elem

(** [simple_elem s] Builds an [elem] from a string [s]. No html effect
    is provided. *)
val simple_elem : string -> elem

(** [value_elem ~dim p cn v] builds an [elem] from a value_type [v].
    An optional parameter [~dim] can be provided if the value_type is
    a multidimensional array whose size have to be displayed. If the
    value_type represents a class or a class array, an hyperlink will
    be displayed, referencing the corresponding class in the generated
    html files. The program [p] and the current class_name [cn] are
    essential to build a relative hyperlink. *)
val value_elem : ?dim:int -> 'a program -> class_name -> value_type -> elem

val field_elem : ?called_cname:string -> 'a program -> class_name -> class_name
  -> field_signature -> elem

val invoke_elem : ?called_cname:string -> ?called_mname:string -> 'a program ->
  class_name -> method_signature -> invoke -> elem

val method_args_elem : 'a program -> class_name -> method_signature -> elem

module type HTMLPrinter =
sig
  type code
    
  (** [print_program ~css ~js ~info program outputdir] generates html
      files representing the program [p] in the output directory
      [outputdir], given the annotation information [info]
      ([void_info] by default), an optional Cascading Style Sheet
      (CSS) [css] and an optional JavaScript file [js]. If [css] or
      [js] is not provided, a default CSS or JavaScript file is
      generated. @raise Sys_error if the output directory [outputdir]
      does not exist. @raise Invalid_argument if the name
      corresponding to [outputdir] is a file. *)
  val print_program :
    ?css:string -> ?js:string -> ?info:info -> code program -> string -> unit
end

module type PrintInterface =
sig
  type instr
  type code
  val iter_code : (int -> instr -> unit) -> code Lazy.t -> unit
  val method_param_names : code program -> class_name -> method_signature
    -> string list option
  val inst_html : code program -> class_name -> method_signature -> int
    -> instr -> elem list
  val get_callgraph : code program -> callgraph
end

module Make (S : PrintInterface) : HTMLPrinter
