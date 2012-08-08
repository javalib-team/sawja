(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
 * Copyright (c)2012 Pierre Vittet (INRIA)
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


(** {2 Containers}*)

module GenericSet ( S : sig type t end ) :
sig
  type elt = int * S.t
  type t = elt Ptmap.t

  val empty : t
  val is_empty : t -> bool
  val mem :  elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val equal : t -> t -> bool
  val elements : t -> elt list
  val cardinal : t -> int
  val iter :(elt -> unit) -> t -> unit
  val fold :(elt -> 'a -> 'a) -> t -> 'a -> 'a
  val exists :(elt -> bool) -> t -> bool
  val filter :(elt -> bool) -> t -> t
  val inter :t -> t -> t
  val of_list :(elt) list -> t
  val of_array :(elt) array -> t
end

module GenericMap ( S : sig type t end ) :
sig
  type key = int * S.t
  type 'a t = (key * 'a) Ptmap.t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val modify : key -> ('a option -> 'a) -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t 
  val mem : key -> 'a t -> bool 
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) ->  'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val merge :  ('a -> 'a -> 'a ) -> 'a t -> 'a t -> 'a t
  val choose_and_remove : 'a t -> key * 'a * 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
  val key_elements : 'a t -> key list
  val value_elements : 'a t -> 'a list
  val elements : 'a t -> (key * 'a) list
end

module MaptoSet ( S : sig type t end )
  ( GMap : GenericMapSig with type key = S.t )
  ( GSet : GenericSetSig with type elt = S.t ) :
sig
  val to_set : 'a GMap.t -> GSet.t
end


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



