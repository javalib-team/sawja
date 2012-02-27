(*
 * This file is part of SAWJA
 * Copyright (c)2009 Frederic Besson (INRIA)
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

open Bigarray

type t

(* avoid the OS the check /etc/localtime every now and then *)
let _ = Unix.putenv "TZ" "GMT0"

(* Information on BDDs *)

(** [var t] is undefined if the BDD is [tt] or [ff] -- the BDD library exits *)
external var : t -> int = 
    "ocaml_bdd_var"

external low : t -> t =
    "ocaml_bdd_low"

external high : t -> t = 
    "ocaml_bdd_high"

external support : t -> t = 
    "ocaml_bdd_support"

external satcount : t -> float = 
    "ocaml_bdd_satcount"

external satcountset : t -> t -> float = 
    "ocaml_bdd_satcountset"

external satcountsetln : t -> float = 
    "ocaml_bdd_satcountln"


external satcountlnset : t -> t -> float = 
    "ocaml_bdd_satcountlnset"


external nodecount : t -> int = 
    "ocaml_bdd_nodecount"

external anodecount : t -> int = 
    "ocaml_bdd_anodecount"

external pathcount : t -> float = 
    "ocaml_bdd_pathcount"

(* Kernle BDD operations and data structures *)

(** [init i j] calls bdd_init i j  and sets a error handler
    throwing (on error) the Caml exceptionon Invalid_argument.
*)
external init : int -> int -> int = 
    "ocaml_bdd_init"

(** [reset () ] is probably unsafe -- 
    it migth free the BDD table even if BDD nodes are still references in the Caml heap *)
external reset : unit -> unit = 
    "ocaml_bdd_done"

external setvarnum : int -> int = 
    "ocaml_bdd_setvarnum"

external extvarnum : int -> int = 
    "ocaml_bdd_extvarnum"

external isrunning : unit -> bool = 
    "ocaml_bdd_isrunning"

external setmaxnodenum : int -> int = 
    "ocaml_bdd_setmaxnodenum"

external setcacheratio : int -> int =
    "ocaml_bdd_setcacheratio"

external setmaxnodenum : int -> int = 
    "ocaml_bdd_setmaxnodenum"

external setmaxincrease : int -> int = 
    "ocaml_bdd_setmaxincrease"

external setminfreenodes : int -> int = 
    "ocaml_bdd_setminfreenodes"

external getnodenum : unit -> int = 
    "ocaml_bdd_getnodenum"

external getallocnum : unit -> int = 
    "ocaml_bdd_getallocnum"

external versionstr : unit -> string = 
    "ocaml_bdd_versionstr"

external versionnum : unit -> int = 
  "ocaml_bdd_versionnum"

external tt : unit -> t = 
  "ocaml_bdd_true"

external ff  : unit -> t = 
  "ocaml_bdd_false"

external varnum : unit -> int = 
    "ocaml_bdd_varnum"

external ithvar : int -> t = 
    "ocaml_bdd_ithvar"

external nithvar : int -> t = 
    "ocaml_bdd_nithvar"

external gbc : unit -> unit = 
    "ocaml_bdd_gbc"

(** BDD operators *)

external not : t -> t = 
    "ocaml_bdd_not"


type bddop = AND| XOR | OR | NAND | NOR | IMP | BIIMP | DIFF | LESS | INVIMP

external apply : t -> t -> bddop -> t = 
    "ocaml_bdd_apply"

external ite : t -> t -> t -> t = 
    "ocaml_bdd_ite"

external restrict : t -> t -> t =
    "ocaml_bdd_restrict"

external constrain : t -> t -> t = 
    "ocaml_bdd_constrain"


type vars = (int32 , int32_elt , c_layout) Array1.t


external replace_all : t -> vars -> vars -> t =
    "ocaml_bdd_replace_all"

external replace : t -> int -> int -> t =
    "ocaml_bdd_replace"


external compose : t -> t -> t -> t = 
    "ocaml_bdd_compose"

external exist : t -> t -> t =
    "ocaml_bdd_exist"

external forall : t -> t -> t =
    "ocaml_bdd_forall"

external unique : t -> t -> t = 
    "ocaml_bdd_unique"

external appex : t -> t -> bddop -> t -> t =
    "ocaml_bdd_appex"

external appall : t -> t -> bddop -> t -> t = 
    "ocaml_bdd_appall"


(** Variable reordering *)

external swapvar : int -> int -> int =
    "ocaml_bdd_swapvar"

type reorder = WIN2 | WIN2ITE | WIN3 | WIN3ITE | SIFT | SIFTITE | RANDOM | NONE

external reorder : reorder -> unit =
    "ocaml_bdd_reorder"

external reorder_gain : unit -> int =
    "ocaml_bdd_reorder_gain"

external clrvarblocks : unit -> unit =
   "ocaml_bdd_clrvarblocks"


(** [intaddvarblock t fixed] false -> BDD_REORDER_FREE  true -> BDD_REORDER_FIXED  *)
external intaddvarblock : t -> bool -> int =
    "ocaml_bdd_intaddvarblock"

external varblockall : unit -> unit =
    "ocaml_bdd_varblockall"

external autoreorder : reorder -> reorder =
    "ocaml_bdd_autoreorder"

external autoreorder_times : reorder -> int -> reorder =
    "ocaml_bdd_autoreorder_times"

external var2level : int -> int = 
    "ocaml_bdd_var2level"

(** [enable_reorder b] if b then enable reordering else disable reordering *)
external enable_reorder : bool -> unit = 
    "ocaml_bdd_enable_disable_reorder"
    

(* Finite domain variable blocks *)
external fdd_overlapdomain : int -> int -> int=
    "ocaml_fdd_overlapdomain"

external fdd_clearall : unit -> unit =
    "ocaml_fdd_clearall"

external fdd_domainnum : unit -> int =
    "ocaml_fdd_domainnum"

external fdd_equals : int -> int -> int =
    "ocaml_fdd_equals"

external fdd_extdomain : vars -> int = 
    "ocaml_fdd_extdomain"

external fdd_intaddvarblock : int -> int -> int = 
    "ocaml_fdd_intaddvarblock"

external fdd_ithset : int -> t =
    "ocaml_fdd_ithset"

external fdd_ithvar : int -> int -> t =
    "ocaml_fdd_ithvar"

external fdd_vars : int -> int array = 
    "ocaml_fdd_vars"
