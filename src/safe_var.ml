(*
 * This file is part of SAWJA
 * Copyright (c)2009, 2010 Laurent Hubert (CNRS)
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

module type CONTEXT = sig
  (* The Context could be
     - Context sensibility (duplicate program point)
     - Analysis identification (several pp because several analyses)
     - Flow information (Intermediate state, return, parameters,
     exception returned, etc. ) *)
  type context

  val compare : context -> context -> int
  val equal : context -> context -> bool
  val hash : context -> int
  val to_string : context -> string
  val pprint : Format.formatter -> context -> unit
end



(* maybe we should also make a functor to combine contexts *)
module EmptyContext = struct
  type context = unit
  let compare () () = 0
  let equal () () = true
  let hash () = 1
  let to_string () = "()"
  let pprint fmt () = Format.pp_print_string fmt "()"
end

(* module PP = struct *)
(*   type t = JBasics.class_method_signature * int *)
(*   let equal = (=) *)
(*   let compare = compare *)
(*   let hash = Hashtbl.hash *)
(* end *)


module type S = sig

  (* The Context could be
     - Context sensibility (duplicate program point)
     - Analysis identification (several pp because several analyses)
     - Flow information (Intermediate state, return, parameters,
     exception returned, etc. ) *)

  module Context:CONTEXT
  type ioc = JBasics.class_name (* shortcut *)

  (* The context part could be added automatically *)
  type var_global = [`Global of Context.context]
  type var_ioc = [`IOC of Context.context * ioc]
  type var_field =[`Field of Context.context * ioc * JBasics.field_signature]
  type var_method = [`Method of Context.context * ioc * JBasics.method_signature]
  type var_pp = [`PP of Context.context *  ioc * JBasics.method_signature * int]
  type t = [ var_global | var_ioc | var_field | var_method | var_pp ]
      (* automatically generated*)

  val compare : t -> t -> int
    (* automatically generated from compare_* functions *)
  val equal : t -> t -> bool
    (* automatically generated from equal_* functions *)
  val hash : t -> int (* or [<t] -> int *)
    (* automatically generated from hash_* functions *)
  val pprint : Format.formatter -> t -> unit

  val compare_global : var_global -> var_global -> int
  val compare_ioc : var_ioc -> var_ioc -> int
  val compare_field : var_field -> var_field -> int
  val compare_method : var_method -> var_method -> int
  val compare_pp : var_pp -> var_pp -> int

  val equal_global : var_global -> var_global -> bool
  val equal_ioc : var_ioc -> var_ioc -> bool
  val equal_field : var_field -> var_field -> bool
  val equal_method : var_method -> var_method -> bool
  val equal_pp : var_pp -> var_pp -> bool

  val hash_global : var_global -> int
  val hash_ioc : var_ioc -> int
  val hash_field : var_field -> int
  val hash_method : var_method -> int
  val hash_pp : var_pp -> int

  val pprint_global : Format.formatter -> var_global -> unit
  val pprint_ioc : Format.formatter -> var_ioc -> unit
  val pprint_field : Format.formatter -> var_field -> unit
  val pprint_method : Format.formatter -> var_method -> unit
  val pprint_pp : Format.formatter -> var_pp -> unit
end


module Make = functor (Context:CONTEXT) -> struct
  open Javalib_pack
open JBasics
  open Javalib

  (* The Context could be
     - Context sensibility (duplicate program point)
     - Analysis identification (several pp because several analyses)
     - Flow information (Intermediate state, return, parameters,
     exception returned, etc. ) *)

  module Context = Context
  type ioc = JBasics.class_name
  type ctx = Context.context

  type var_global =
      [`Global of ctx]
  type var_ioc =
      [`IOC of ctx * ioc]
  type var_field =
      [`Field of ctx * ioc * field_signature]
  type var_method =
      [`Method of ctx * ioc * method_signature]
  type var_pp =
      [`PP of ctx * ioc * method_signature * int]
  type t = [ var_global | var_ioc | var_field | var_method | var_pp ]

  type t' = G| C | F | M | PP
  let t2t' : [<t] -> t' = function
    | `Global _ -> G
    | `IOC _ -> C
    | `Field _ -> F
    | `Method _ -> M
    | `PP _ -> PP

  let pprint_global fmt = function `Global ctx ->
    Format.fprintf fmt "(@[VG:@,%a@])"
      Context.pprint ctx

  let pprint_ioc fmt = function `IOC (ctx,ioc) ->
    Format.fprintf fmt "(@[VIOC:@,%a,%s@])"
      Context.pprint ctx
      (Javalib.JPrint.class_name ioc)

  let pprint_field fmt = function `Field (ctx,ioc,fs) ->
    Format.fprintf fmt "(@[VF:@,%a,(%s,%s)@])"
      Context.pprint ctx
      (JPrint.class_name ioc)
      (Javalib.JPrint.field_signature ~jvm:true fs)

  let pprint_method fmt = function `Method (ctx,ioc,ms) ->
    Format.fprintf fmt "(@[VM:@,%a,(%s)@])"
      Context.pprint ctx
      (Javalib.JPrint.method_signature ~callee:(TClass ioc) ~jvm:true ms)

  let pprint_pp fmt = function `PP (ctx,ioc,ms,i) ->
    Format.fprintf fmt "(@[VPP:@,%a,(%s:%d)@])"
      Context.pprint ctx
      (Javalib.JPrint.method_signature ~callee:(TClass ioc) ~jvm:true ms)
      i

  let pprint fmt : t -> unit= function
    | `Global _ as var -> pprint_global fmt var
    | `IOC _ as var -> pprint_ioc fmt var
    | `Field _ as var -> pprint_field fmt var
    | `Method _ as var -> pprint_method fmt var
    | `PP _ as var -> pprint_pp fmt var

  let compare_global =
    function `Global c1 -> function `Global c2 ->
      Context.compare c1 c2
  let compare_ioc =
    let (+) = fun a b -> if Lazy.force a <> 0 then a else b in
      function `IOC (c1,ioc1) -> function `IOC (c2,ioc2) ->
	Lazy.force
	  (lazy (cn_compare ioc1 ioc2)
	   + lazy (Context.compare c1 c2))
  let compare_field =
    let (+) = fun a b -> if Lazy.force a <> 0 then a else b in
      function `Field (c1,ioc1,fs1) -> function `Field (c2,ioc2,fs2) ->
	Lazy.force
          (lazy (cn_compare ioc1 ioc2)
           + lazy (fs_compare fs1 fs2)
           + lazy (Context.compare c1 c2))
  let compare_method =
    let (+) = fun a b -> if Lazy.force a <> 0 then a else b in
      function `Method (c1,ioc1,ms1) -> function `Method (c2,ioc2,ms2) ->
	Lazy.force
          (lazy (cn_compare ioc1 ioc2)
           + lazy (ms_compare ms1 ms2)
           + lazy (Context.compare c1 c2))
  let compare_pp =
    let (+) = fun a b -> if Lazy.force a <> 0 then a else b in
      function `PP (c1,ioc1,ms1,pc1) -> function `PP  (c2,ioc2,ms2,pc2) ->
        Lazy.force
          (lazy (compare pc1 pc2)
           + lazy (cn_compare ioc1 ioc2)
           + lazy (ms_compare ms1 ms2)
           + lazy (Context.compare c1 c2))

  (* should be automatically generated *)
  let compare (v1:t) (v2:t) : int =
    match v1,v2 with
      | `PP _ as v1, (`PP _ as v2) ->
	  compare_pp v1 v2
      | `Method _ as v1, (`Method _ as v2) ->
	  compare_method v1 v2
      | `Field _ as v1, (`Field _ as v2) ->
	  compare_field v1 v2
      | `IOC _ as v1, (`IOC _ as v2) ->
	  compare_ioc v1 v2
      | `Global _ as v1, (`Global _ as v2) ->
	  compare_global v1 v2
      | v1,v2 -> compare (t2t' v1) (t2t' v2)

  let equal_global = function `Global c1 -> function `Global c2 ->
    Context.equal c1 c2
  let equal_ioc = function `IOC (c1,ioc1) -> function `IOC (c2,ioc2) ->
    cn_equal ioc1 ioc2 && Context.equal c1 c2
  let equal_field = function `Field (c1,ioc1,fs1) -> function `Field (c2,ioc2,fs2) ->
    cn_equal ioc1 ioc2 && fs_equal fs1 fs2 && Context.equal c1 c2
  let equal_method = function `Method (c1,ioc1,ms1) -> function `Method (c2,ioc2,ms2) ->
    cn_equal ioc1 ioc2 && ms_equal ms1 ms2 && Context.equal c1 c2
  let equal_pp = function `PP (c1,ioc1,ms1,pc1) -> function `PP (c2,ioc2,ms2,pc2) ->
    pc1 = pc2 && cn_equal ioc1 ioc2 && ms_equal ms1 ms2 && Context.equal c1 c2

  let equal (v1:t) (v2:t) : bool =
    if v1==v2 then true
    else match v1,v2 with
      | `PP _ as v1, (`PP _ as v2) ->
	  equal_pp v1 v2
      | `Method _ as v1, (`Method _ as v2) ->
	  equal_method v1 v2
      | `Field _ as v1, (`Field _ as v2) ->
	  equal_field v1 v2
      | `IOC _ as v1, (`IOC _ as v2) ->
	  equal_ioc v1 v2
      | `Global _ as v1, (`Global _ as v2) ->
	  equal_global v1 v2
      | _, _ -> false

  let hash_global = function `Global c ->
    Context.hash c
  let hash_ioc = function `IOC (c,ioc) ->
    Hashtbl.hash (Context.hash c, cn_hash ioc)
  let hash_field = function `Field (c,ioc,fs) ->
    Hashtbl.hash (Context.hash c, cn_hash ioc, fs_hash fs)
  let hash_method = function `Method (c,ioc,ms) ->
    Hashtbl.hash (Context.hash c, cn_hash ioc, ms_hash ms)
  let hash_pp = function `PP (c,ioc,ms,pc) ->
    Hashtbl.hash (Context.hash c, cn_hash ioc, ms_hash ms, pc)

  let hash = function
    | `Global _ as v -> hash_global v
    | `IOC _ as v -> hash_ioc v
    | `Field _ as v -> hash_field v
    | `Method _ as v -> hash_method v
    | `PP _ as v -> hash_pp v

end
