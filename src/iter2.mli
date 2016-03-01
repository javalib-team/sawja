(*
 * This file is part of SAWJA
 * Copyright (c)2013 David Pichardie (ENS Rennes)
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
  bot : 'dom;                       (** bottom element *)
  join : 'dom -> 'dom -> 'dom;      (** binary least upper bound *)
  leq :  'dom -> 'dom -> bool;      (** partial order test *)
  normalize :  'dom -> 'dom;        (** normalize after each join (identity is correct) *)
  eval : 'tf -> 'dom list -> 'dom;  (** evaluates a transfer function *) 
  is_id : 'tf -> bool;
  is_strict : 'tf -> bool;          (** for each transfer function, tells if it 
					is strict (i.e. f(bot,...,bot)=bot). [false] is always a safe answer *)
  cstrs : 'tf list;                 (** constraints on which iterate*)
  target : 'tf -> 'var;             (** the variable that is targeted by a constraint *)
  args : 'tf -> 'var list;          (** the variables that are used by a constraint *)
  verbose : bool;
  dom_to_string : 'dom -> string;
  transfer_to_string : 'tf -> string;
  transfer_to_dot_string : 'tf -> string;
  update_transfer : 'tf -> 'var -> 'var -> 'tf
	
}

exception UndefinedVar of string

(** [run manager] computes the least solution of the constraints given in [manager].
    The result is a function of type ['var -> 'dom] that attaches an abstract element to
    each variable used in the manager constraints. It raises an exception [UndefinedVar]
    when used on a variable that does not appear in the manager constraints. *)
val run : ('var,'dom,'tf) manager -> 'var -> 'dom


(** manager of the workset iterator - inplace version.
    ['var] is the type of equation variables.
    ['dom] is the type of abstract elements.
    ['tf] is the type of transfer functions. *)
type ('var,'dom,'tf) mutable_manager = {
  m_string_of_var : 'var -> string;   (** gives to each variable a string representation (for debug) *)
  m_bot : unit -> 'dom;               (** bottom element *)
  m_is_top : 'dom -> bool;            (** if [m_is_top val = true] then [val=TOP] *)
  m_eval_and_join :
   'tf -> 'dom list -> 'dom -> bool;  (** evaluates a transfer function and join the result
  				          with the last argument (in-place modification).
				          returns [false] is the update does not change the previous value *)  
  m_eval :
   'tf -> 'dom list -> 'dom -> bool;  (** evaluates a transfer function and put the result
  				          in the last argument (in-place modification).
				          returns [false] is the update does not change the previous value *)  
  m_is_id : 'tf -> bool;
  m_is_strict : 'tf -> bool;          (** for each transfer function, tells if it 
					is strict (i.e. f(bot,...,bot)=bot). [false] is always a safe answer *)
  m_cstrs : 'tf list;                 (** constraints on which iterate*)
  m_target : 'tf -> 'var;             (** the variable that is targeted by a constraint *)
  m_args : 'tf -> 'var list;          (** the variables that are used by a constraint *)
  m_verbose : bool;
  m_dom_to_string : 'dom -> string;
  m_transfer_to_string : 'tf -> string;
  m_transfer_to_dot_string : 'tf -> string;
  m_update_transfer : 'tf -> 'var -> 'var -> 'tf
}

(** same as [run], but in place version *)
val m_run : ('var,'dom,'tf) mutable_manager -> 'var -> 'dom

val mutable_manager_of_manager :
  ('var,'dom,'tf) manager -> ('var,'dom ref,'tf) mutable_manager
					 
