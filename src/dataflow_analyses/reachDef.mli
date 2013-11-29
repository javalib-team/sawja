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

(** {{:http://en.wikipedia.org/wiki/Use-define_chain}Reachable
definitions analysis} for {!JBir} representation (use {!Iter} solver).*)

(** {2 Domain of the reachable definitions analysis} *)

(** Map each variable to the set of instructions program points that
could have defined its value*)
module Lat :
  sig
    type t
    val get : t -> JBir.var -> Ptset.t
    val to_string : JBir.t -> t -> string
  end

(** {2 Launch the analysis}*)

(** [run code] returns a function [f pc] that returns the domain of
the reachable definitions analysis before execution of the program
point [pc]*)
val run : JBir.t -> int -> Lat.t
val run2 : JBir.t -> int -> Lat.t

