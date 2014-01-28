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

(** Defines a simple fixpoint solver for constraints between integer
    values (suitable for constraints between program
    points). The {!Live_bir}, {!ReachDef} and {!AvailableExpr} analyzes are
  examples of use of this solver.*)


type workset_strategy =
  | Decr (** Program points are chosen in decreasing order *)
  | Incr (** Program points are chosen in increasing order *)

(** manager of the workset iterator.  ['a] is the type of abstract
    elements.  ['b] is the type of transfer functions. *)
type ('a,'b) manager = {
  bot : 'a; (** bottom element *)
  join : 'a -> 'a -> 'a; (** binary least upper bound *)
  leq :  'a -> 'a -> bool; (** partial order test *)
  normalize :  'a -> 'a; (** normalize after each join (identity is correct) *)
  eval : 'b -> 'a -> 'a; (** evaluates a transfer function *)
  size : int; (** analysis will be computed for points in [0 .. size-1] *)
  workset_strategy : workset_strategy;
  cstrs : (int * 'b * int) list; (** constraints on which iterate*)
  init_points : int list; (** entry points in the equation system *)
  init_value : int -> 'a; (** constant contraints on entry points *)
  verbose : bool;
  dom_to_string : 'a -> string;
  transfer_to_string : 'b -> string
}

(** [run manager] computes the least solution of the constraints given in [manager].
    The result is a function of type [int -> 'a] that attaches an abstract element to
    each point in [0 .. manager.size -1]. *)
val run : ('a,'b) manager -> int -> 'a
