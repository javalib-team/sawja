(*
 * This file is part of SAWJA
 * Copyright (c)2013 Pierre Vittet (INRIA)
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


open Javalib_pack
open JBasics
open JProgram

  (*Abstraction of a variable*)
module AbVSet:
sig
  type t
  type analysisID = unit
  type analysisDomain = t

  val bot : t
  (*==Null*)
  val empty : t 
  val isBot : t -> bool
  val isTop: t -> bool

  val primitive : t 
  val isPrimitive : t -> bool

  (*If the set is empty, it means that the only possible concrete value of the
   * variable is null.*)
  val is_empty: t -> bool
  (** [singleton pps cn]: Create a set from a singleton element. [pps] is a list
    * of program point characterizing the location of the affectation. cn is the
    * class of the allocated object. *)
  val singleton : JBirPP.t list -> object_type-> t

  val equal : t -> t -> bool

  val inter : t -> t -> t
  val join : ?modifies:bool ref -> t -> t -> t 
  val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> t -> t
  
  (** [filter_with_compatible prog abs cn] :Restrain the abstraction [abs] to
    * the type compatible with [cn].*)
  val filter_with_compatible : 'a JProgram.program -> t -> object_type -> t
(** [filter_with_uncompatible prog abs cn] :Restrain the abstraction [abs] to
    * the type not compatible with [cn].*)
  val filter_with_uncompatible : 'a JProgram.program -> t -> object_type -> t
  val concretize : t -> JType.ObjectSet.t


  val to_string : t -> string
  val pprint : Format.formatter -> t -> unit
  val get_analysis : analysisID -> t -> analysisDomain
 
end

  (*Abstraction of a field*)
module AbFSet :
sig
  type t
  type analysisID = unit
  type analysisDomain = t

  val bot : t
  val empty : t 
  val isBot : t -> bool

  val is_empty: t -> bool
  val equal : t -> t -> bool

  val inter : t -> t -> t
  val join : ?modifies:bool ref -> t -> t -> t 
  val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> t -> t

  (** [var2fSet obj var]: for a field such as [obj].field = [var], return its AbFSet according to the AbVSet of [obj] and [var]. *)
  val var2fSet : AbVSet.t -> AbVSet.t -> t
  (** [fSet2var fset objvSet]: From a field abstraction [fset] and [objvSet],
    * the abstraction of the object variable used to access the field,  return
    * a variable abstraction corresponding to the possible abstract values when
    * then variable is affected the field value.*)
  val fSet2var: t -> AbVSet.t -> AbVSet.t
  (*A special 'virtual' set which can contains static variables. It used as obj*)
  val static_field_dom : AbVSet.t

  val to_string : t -> string

  val pprint : Format.formatter -> t -> unit
  val get_analysis : analysisID -> t -> analysisDomain
 
end



(*primitive variables are ignored from this map*)
module AbLocals : sig
  type t
  type analysisID = AbVSet.analysisID
  type analysisDomain = t
  val bot : t 
  val init : t
  val isBot : analysisDomain -> bool
  val join : ?modifies:bool ref -> t -> t -> t
  val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
  val equal : t -> t -> bool
  val get_analysis : analysisID -> t -> analysisDomain
  val to_string : t -> string 
  val pprint : Format.formatter -> t -> unit
  val get_var : int -> analysisDomain -> AbVSet.t
  val set_var : int -> AbVSet.t -> analysisDomain -> analysisDomain
end


module AbMethod : sig
  type t 
  type analysisID = unit
  type analysisDomain = t
(*   val is_static : A3Bir.t node -> method_signature -> bool *)
  val equal : t -> t -> bool
  val bot : t
  val isBot : t -> bool
  val init : t

  (**For a virtual method, the argument at index 0, is 'this'.*)
  val get_args : t -> AbLocals.t
  val init_locals : JBir.t node -> method_signature -> t -> AbLocals.t
  val get_return : t -> AbVSet.t
  val get_exc_return : t -> AbVSet.t
  val join_args : t -> AbLocals.t -> t
  val set_args: t -> AbLocals.t -> t
  val join_return : t -> AbVSet.t -> t
  val join_exc_return : t -> AbVSet.t -> t
  val join : ?modifies:bool ref -> t -> t -> t
  val join_ad : ?do_join:bool -> ?modifies:bool Pervasives.ref ->
       t -> analysisDomain -> t
  val pprint : Format.formatter -> t -> unit
  val to_string : t -> string 
  val get_analysis : analysisID -> t -> analysisDomain
end


module Var : Safe.Var.S with module Context = Safe.Var.EmptyContext

module AbField : (Safe.Domain.S
                   with type t = AbFSet.t
                   and type analysisDomain = AbFSet.t
                   and type analysisID = AbFSet.analysisID)


module AbPP : (Safe.Domain.S
                   with type t = AbLocals.t
                   and type analysisDomain = AbLocals.t
                   and type analysisID = AbLocals.analysisID)

module AbMeth : (Safe.Domain.S
                   with type t = AbMethod.t
                   and type analysisDomain = AbMethod.t
                   and type analysisID = AbMethod.analysisID)

module CFAState : Safe.State.S 
  with module Var = Safe.Var.Make(Safe.Var.EmptyContext) 
  and module Global = Safe.Domain.Empty
  and module IOC = Safe.Domain.Empty
  and module Field = AbField
  and module Method = AbMeth
  and module PP = AbPP

module CFAConstraints : Safe.Constraints.S with module State = CFAState 
