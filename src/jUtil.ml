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

(* 
 * Fold a function f on an accumulator x0 and an array t 
 * f a [| b0 ; b1 ; ... ; bn |] --->  f 0 b0 (f 1 b1 (f ... (f n bn x0) ...) )
 *)
let foldi f x0 t =
  let n = Array.length t in
  let rec aux i =
    if i>=n then x0
    else f i t.(i) (aux (i+1)) in
    aux 0

let for_all f t =
  let n = Array.length t in
  let rec aux i =
    if i>=n then true
    else f i t.(i) && (aux (i+1)) in
    aux 0

(* [find_index x l] return the position of element [x] in list [l]. 
   @raise Not_found if [x] does not appear in [l]. *)
let find_index x l = 
  let rec aux i = function
      [] -> raise Not_found
    | y::q ->
	if x=y then i 
	else aux (i+1) q in
    aux 0 l

(* Containers. *)

module GenericSet = GenericSet.Make
module GenericMap = GenericMap.Make

open Javalib_pack.JBasics

module MaptoSet ( S : sig type t end )
  ( GMap : GenericMapSig with type key = S.t )
  ( GSet : GenericSetSig with type elt = S.t ) =
struct
  let to_set m =
    GMap.fold (fun k _ s -> GSet.add k s) m GSet.empty
end

(* Print utilities ... *)

let rec print_list_sep_rec sep pp = function
  | [] -> ""
  | x::q -> sep^(pp x)^(print_list_sep_rec sep pp q)

let rec print_list_sep_list_rec sep pp = function
  | [] -> []
  | x::q -> (sep^(pp x))::(print_list_sep_list_rec sep pp q)

let print_list_sep sep pp = function
  | [] -> ""
  | x::q -> (pp x)^(print_list_sep_rec sep pp q)

let print_list_sep_id sep = print_list_sep sep (fun x -> x)

let print_field ?(long_fields=false) c f =
  if long_fields then
    Printf.sprintf "<%s:%s>" (Javalib_pack.JPrint.class_name c) (fs_name f)
  else (fs_name f)

let bracket b s =
  if b then s else Printf.sprintf "(%s)" s



