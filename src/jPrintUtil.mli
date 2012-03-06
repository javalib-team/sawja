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


