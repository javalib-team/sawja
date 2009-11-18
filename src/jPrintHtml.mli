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

(** {2 Printing an A3Bir.t program.} *)

(** [print_program ~css ~js ~info program outputdir] generates html
    files representing the {!A3Bir.t} program [p] in the output
    directory [outputdir], given the annotation information [info]
    ([void_info] by default), an optional Cascading Style Sheet (CSS)
    file [css] and an optional JavaScript file [js]. If [css] or [js]
    is not provided, a default CSS or JavaScript file is generated.
    @raise Sys_error if the output directory [outputdir] does not
    exist. @raise Invalid_argument if the name corresponding to
    [outputdir] is a file. *)
val print_a3bir_program : ?css:string -> ?js:string -> ?info:info
  -> A3Bir.t program -> string -> unit

(** {2 Building a Printer for any program representation.} *)

(** Abstract type representing basic elements to build html instructions. *)
type elem

(** [simple_elem s] Builds an [elem] from a string [s]. No html effect
    is provided. *)
val simple_elem : string -> elem

(** [value_elem ~dim p cn v] builds an [elem] from a program [p],
    a current class [cn] and a value type [v]. An optional parameter
    [~dim] can be provided [v] is a multidimensional array whose size
    have to be displayed. If the [v] represents a class or an array of
    classes, an hyperlink will be generated, referencing the
    corresponding class in the generated html files. Otherwise, raw
    text corresponding to [v] is returned. If [v] contains a class,
    the program [p] is necessary to know if this class is in the
    program. If it is not the case, no hyperlink will be generated.
    The current class_name [cn] is necessary to build a relative
    hyperlink. *)
val value_elem : ?dim:int -> 'a program -> class_name -> value_type -> elem

(** [field_elem ~called_cname p cn fcn fs] builds an [elem] from
    a program [p], a current class name [cn], a field class name [fcn]
    and a field signature [fs]. The field will be represented by an
    hyperlink with text [A.f] where [f] is the field name. By default
    [A] corresponds to [cn] class name. If you want to specify the [A]
    string, you can provide the optional parameter [~called_cname].
*)
val field_elem : ?called_cname:string -> 'a program -> class_name -> class_name
  -> field_signature -> elem

(** [invoke_elem ~called_cname p cn ms pp ccn cms] builds an [elem]
    from a program [p], a current class name [cn], a current method
    signature [ms], a program point [pp] where the invoke occurs,
    a called class name [ccn] and a called method signature [cms]. The
    invoke call will be represented by a dynamic link with text [A.m]
    where [m] is the called method name (contained in [k]). By
    default, [A] corresponds to [cn] class name. If you want to
    specify the [A] string, you can provide the optional parameter
    [~called_cname].
*)
val invoke_elem : ?called_cname:string -> 'a program -> class_name ->
  method_signature -> int -> class_name -> method_signature -> elem

(** [method_args_elem p cn ms] builds an [elem] from a program [p],
    a current class name [cn] and a method signature [ms]. The
    generated [elem] is a list of method parameters [(A,B,int)] where
    class references are hyperlinks (if they are present in [p]).
*)
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
  (** Type of the instructions. *)
  type instr

  (** Type of the code. *)
  type code

  (** Function to provide in order to iter on the code structure. The
      function passed to [iter_code] expects as paramters a program point
      (in the code representation) and the corresponding list of
      instructions. In {JCode.jcode} representation, this list only
      contains one element. *)
  val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit

  (** Function to provide in order to display the source variable
      names in the method signatures. *)
  val method_param_names : code program -> class_name -> method_signature
    -> string list option

  (** Function to provide in order to display instructions. Given
  a context constituted by the program, the current class name, the
  current method signature, the instruction you want to display and
  its program point, you must provide a list of [elem] by using the
  functions defined in {!JPrintHtml} module. *)
  val inst_html : code program -> class_name -> method_signature -> int
    -> instr -> elem list

  (** Function to provide in order to associate an intermediate
  representation program point to a JCode.jcode program point. *)
  val jcode_pp : ('a program -> class_name -> method_signature -> int -> int) option
end

module Make (S : PrintInterface) : HTMLPrinter
