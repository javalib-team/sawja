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

exception HeadNode
exception TailNode
exception NoHeadNode
exception CellNotFound

type 'a link = 'a cellule
and 'a content = Content of 'a | Head | Tail
and 'a cellule = { mutable prev : 'a link;
		   content : 'a content;
		   mutable next : 'a link }
and 'a wlist = 'a cellule

type 'a tail = 'a wlist

let create () =
  let rec head = { prev = tail; content = Head; next = tail }
  and tail = { prev = head; content = Tail; next = head }
  in head
let _get (l:'a wlist) =
  match l.content with
    | Content c -> c
    | Head -> raise HeadNode
    | Tail -> raise TailNode
let _next (l:'a wlist) : 'a wlist = l.next
let _prev (l:'a wlist) : 'a wlist = l.prev
let tail (l:'a wlist) : 'a wlist =
  match l.content with
    | Head -> l.prev
    | _ -> raise NoHeadNode

let add (e:'a) (l:'a wlist) =
  match l.content with
    | Head ->
	let new_elm = { prev = l;
			content = Content e;
			next = l.next } in
	let cell = new_elm.next in
	  cell.prev <- new_elm;
	  l.next <- new_elm
    | _ -> raise NoHeadNode

let _del (l:'a wlist) =
  match l.content with
    | Head -> raise HeadNode
    | Tail -> raise TailNode
    | _ ->
	l.next.prev <- l.prev;
	l.prev.next <- l.next

let rec _mem (e:'a) (l:'a wlist) =
  match l.content with
    | Head ->
	let cell = l.next in
	  _mem e cell
    | Tail -> false
    | Content c ->
	if (c = e) then
	  true
	else let cell = l.next in
	  _mem e cell

let rec size' ?(s=0) (l:'a wlist) =
  match l.content with
    | Head ->
	let cell = l.next in
	  size' ~s:0 cell
    | Tail -> s
    | Content _ ->
	let cell = l.next in
	  size' ~s:(s+1) cell

let size l = size' l

let rec _iter (f:'a -> unit) (l:'a wlist) =
  match l.content with
    | Head ->
	let cell = l.next in
	  _iter f cell
    | Tail -> ()
    | Content c ->
	f c;
	let cell = l.next in
	  _iter f cell

let rec _iter_until_cell (f:'a -> unit) (bound:'a wlist) (l:'a wlist) =
  match l.content with
    | Head ->
	let cell = l.next in
	  _iter_until_cell f bound cell
    | Tail -> raise CellNotFound
    | Content c ->
	if not( bound == l) then
	  (f c;
	   let cell = l.next in
	     _iter_until_cell f bound cell)

let rec iter_to_head_i (f:'a wlist -> 'a -> unit) (l:'a wlist) =
  match l.content with
    | Head -> ()
    | Tail ->
	let cell = l.prev in
	  iter_to_head_i f cell
    | Content c ->
	f l c;
	let cell = l.prev in
	  iter_to_head_i f cell

let iter_to_head (f:'a -> unit) (l:'a wlist) =
  iter_to_head_i (fun _ x -> f x) l

let _map (f:'a -> 'b) (l:'a wlist) =
  let tail = tail l
  and lm = ref [] in
    iter_to_head (fun x -> lm := (f x) :: !lm) tail;
    !lm
