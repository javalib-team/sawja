(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
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

(** {{:http://en.wikipedia.org/wiki/Available_expression}Available
    expressions analysis} for {!JBir} representation  (use {!Iter} solver).*)

(** {2 Domain of the available expressions analysis} *)

(** A set of couple: a variable and its available expression
    associated*)
module Lat :
sig
  type elt = JBir.var * JBir.expr
  type t
  val mem : elt -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool

  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val split : elt -> t -> t * bool * t
  val collect_affect_var : JBir.t -> t
  val print_key : JBir.var * JBir.expr -> string
  val to_string : t -> string
  val bottom : t
  val kill : JBir.var -> t -> t
  val gen : JBir.var -> JBir.expr -> t -> t
end

(** {2 Launch the analysis}*)

(** [run code] returns a function [f pc] that returns the domain of
the available expressions analysis before execution of the program
point [pc]*)
val run : JBir.t -> int -> Lat.t

