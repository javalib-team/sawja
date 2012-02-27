(*
 * This file is part of SAWJA
 * Copyright (c)2009 Nicolas Barre (INRIA)
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

(** A growable when itering list. *)


(* TODO : add documentation *)

type 'a wlist

type 'a tail

(** {2 Basic operations.} *)

val create : unit -> 'a wlist

val tail : 'a wlist -> 'a tail

val add : 'a -> 'a wlist -> unit

val iter_to_head : ('a -> unit) -> 'a tail -> unit

val size : 'a wlist -> int
