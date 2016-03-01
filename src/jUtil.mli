(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
 * Copyright (c)2012 Pierre Vittet (INRIA)
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

(** This modules provides some utility types and functions useful in Sawja,
    they are internal and must to be used outside of Sawja.*)


open Javalib_pack.JBasics


(** {2 Various utility functions over arrays and lists}*)

(**
   Fold a function f on an accumulator x0 and an array t 
   f a [| b0 ; b1 ; ... ; bn |] --->  f 0 b0 (f 1 b1 (f ... (f n bn x0) ...) )
 *)
val foldi: (int -> 'a -> 'b -> 'b) -> 'b -> 'a array -> 'b

(**
  [for_all f ar]: Run the function [f] iteratively on every element of the
  array [ar]. Stop on the first element for which [f] return false and return
  false (or return true if each element is true).
  [f] take as first argument the index of the current element and the current
  element as second argument.
*)
val for_all: (int -> 'a -> bool) -> 'a array -> bool

(** [find_index x l] return the position of element [x] in list [l]. 
    @raise Not_found if [x] does not appear in [l]. *)
val find_index: 'a -> 'a list -> int

(** {2 Print utilities }*)

(** [print_list_sep sep fprint elLst] : Return the elements contained in the
  list [elLst] using the print function [fprint] in a single string. a
  separator string [sep] can be inserted between each printed element.*)
val print_list_sep : string -> ('a -> string) -> 'a list -> string

(** [print_list_sep_id sep strLst]: Same as print_list_sep but applied a list
  of string [strLst] which does not need to string functions.*)
val print_list_sep_id : string -> string list -> string

val print_field : ?long_fields:bool -> Javalib_pack.JBasics.class_name 
                  -> Javalib_pack.JBasics.field_signature -> string

(**[bracket b s]: If [b] is true, return [s] else return [s] surrounded with
   bracket.*)
val bracket : bool -> string -> string

(** {3 Timing utilities } *)

val timer : ('a -> 'b) -> 'a -> 'b * float
val ref_timer : float ref-> ('a -> 'b) -> 'a -> 'b
