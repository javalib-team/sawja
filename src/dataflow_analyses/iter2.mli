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


(** manager of the workset iterator.
    ['var] is the type of equation variables.
    ['dom] is the type of abstract elements.
    ['tf] is the type of transfer functions. *)
type ('var,'dom,'tf) manager = {
  string_of_var : 'var -> string;   (** gives to each variable a string representation (for debug) *)
  hash_var : 'var -> int;           (** gives to each variable an unique hash value *)
  bot : 'dom;                       (** bottom element *)
  join : 'dom -> 'dom -> 'dom;      (** binary least upper bound *)
  leq :  'dom -> 'dom -> bool;      (** partial order test *)
  normalize :  'dom -> 'dom;        (** normalize after each join (identity is correct) *)
  eval : 'tf -> 'dom list -> 'dom;  (** evaluates a transfer function *) 
	cstrs : ('var list * 'tf * 'var) list; (** constraints on which iterate*)
  verbose : bool;
  dom_to_string : 'dom -> string;
  transfer_to_string : 'tf -> string
}

(** [run manager] computes the least solution of the constraints given in [manager].
    The result is a function of type [int -> 'dom] that attaches an abstract element to
    each point in [0 .. manager.size -1]. *)
val run : ('var,'dom,'tf) manager -> 'var -> 'dom
