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

val css:string
val js:string

(** {2 HTML printing functions.} *)

(** [pp_print_program_to_html_files ~css ~js ~info program outputdir]
    generates html files representing the program [p] in the output
    directory [outputdir], given the annotation information [info]
    ([void_info] by default), an optional Cascading Style Sheet (CSS)
    [css] and an optional JavaScript file [js]. If [css] or [js] is
    not provided, {!css} and {!js} are used when [css] or [js] is not
    provided. @raise Sys_error if the output directory [outputdir]
    does not exist. @raise Invalid_argument if the name corresponding
    to [outputdir] is a file.
*)
val pp_print_program_to_html_files :
  ?css:string -> ?js:string -> ?info:info -> JCode.jcode program -> string -> unit
