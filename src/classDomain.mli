(*
 * This file is part of SAWJA
 * Copyright (c)2010 Laurent Hubert (CNRS)
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

module Make :
  functor (S : sig val nb_bits:int end) ->
    sig
      type t
      type analysisID = unit
      type analysisDomain = t
      val bot : t
      val empty : t
      val isBot : t -> bool
      val singleton : JBasics.class_name -> t
      val is_empty : t -> bool
      val mem : JBasics.class_name -> t -> bool
      val set_size : t -> int
      val meet : t -> t -> t
      val of_set : JBasics.ClassSet.t -> t
      val to_set : t -> JBasics.ClassSet.t
      val join : ?modifies:bool ref -> t -> t -> t
      val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> t -> t
      val equal : t -> t -> bool
      val get_analysis : unit -> 'a -> 'a
      val pprint : Format.formatter -> t -> unit
    end
