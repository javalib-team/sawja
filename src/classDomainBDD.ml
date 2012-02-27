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

module BddBuddy = struct

  type t = Buddy.t

  let init nb_bits =
    if not (Buddy.isrunning ())
    then
      let check int f =
        if int < 0
        then failwith ("buddy."^f^" failed")
      in
      let nbnodes = 1000000 in
        check (Buddy.init nbnodes nbnodes) "init";
        check (Buddy.setcacheratio 2) "setcacheratio";
        check (Buddy.setmaxincrease nbnodes) "setmaxincrease";
        check (Buddy.setvarnum nb_bits) "setvarnum";
        check (Buddy.setmaxnodenum 0) "setmaxnodenum";
        check (Buddy.setminfreenodes 50) "setminfreenodes";
        (* ignore (Buddy.autoreorder Buddy.WIN2ITE); *)
        ()

  let ff _ = Buddy.ff ()
  let tt _ = Buddy.tt ()

  let bdd_and v1 v2 = Buddy.apply v1 v2 Buddy.AND
  let bdd_or v1 v2 = Buddy.apply v1 v2 Buddy.OR
  let bdd_implies v1 v2 = Buddy.apply v1 v2 Buddy.IMP

  let equal = (=)
  let compare = Pervasives.compare

  let ithvar _man i = Buddy.ithvar i
  let nithvar _man i = Buddy.nithvar i

  let is_true = (=) (tt ())
  let is_false = (=) (ff ())

  let high = Buddy.high
  let low = Buddy.low

  let var = Buddy.var

end

(* module BddCudd = struct *)
(*   type t = Bdd.t *)

(*   let nb_bits = 32 *)

(*   let init nb_bits = Manager.make nb_bits 0 0 0 0 *)

(*   let ff man = Bdd.dfalse man *)
(*   let tt man = Bdd.dtrue man *)

(*   let bdd_and = Bdd.dand *)
(*   let bdd_or = Bdd.dor *)
(*   let bdd_implies = Bdd.is_leq *)

(*   let equal = (=) *)
(*   let compare = Pervasives.compare *)


(*   let ithvar man v = Bdd.ithvar man v *)
(*   let nithvar man v = Bdd.dnot (Bdd.ithvar man v) *)

(*   let is_true = Bdd.is_true *)
(*   let is_false = Bdd.is_false *)

(*   let high = Bdd.dthen *)
(*   let low = Bdd.delse *)

(*   let var t = *)
(*     let var = Bdd.topvar t in *)
(*       if var = 65536 *)
(*       then invalid_arg "BddCudd.var" *)
(*       else var *)

(* end *)

module MakeBDD (S:sig val nb_bits : int end) = struct

  module Bdd = BddBuddy

  type t = Bdd.t

  type bdd_ite =
    | Ite of int * Bdd.t * Bdd.t
    | Bool of bool


  open Javalib_pack
open S

  let singletons = ref Ptmap.empty

  let man = Bdd.init nb_bits
  let ff = Bdd.ff man
  let tt = Bdd.tt man

  let empty = ff

  let is_empty s = s = empty

  let singleton elem : Bdd.t =
    try Ptmap.find elem !singletons
    with Not_found ->
      let rec bdd_of_elem' current_bit current_val =
        if current_bit >= nb_bits
        then
          if current_val = 0
          then tt
          else invalid_arg "singleton: not enough bits"
        else
          let half = current_val lsr 1
          and var =
            if (current_val land 1) == 1 (* is_odd *)
            then Bdd.ithvar man current_bit
            else Bdd.nithvar man current_bit
          in
            Bdd.bdd_and var (bdd_of_elem' (succ current_bit) half)
      in
      let bdd = bdd_of_elem' 0 elem
      in
        singletons := Ptmap.add elem bdd !singletons;
        bdd


  let union set1 set2 = Bdd.bdd_or set1 set2

  let inter set1 set2 = Bdd.bdd_and set1 set2

  let add elem set = union set (singleton elem) 

  let mem v s =
    let s1 = Bdd.is_true (Bdd.bdd_implies (singleton v) s)
    and s2 = s = (add v s)
    in
      if s1 <> s2 then assert false;
      s1

  (* let compare = Pervasives.compare in *)
  let equal = (=)

  let elements : t -> int list =
    let cache_elements = ref PMap.empty
    in function set ->
      let rec inspect set =
        if Bdd.is_true set
        then Bool true
        else if Bdd.is_false set
        then Bool false
        else Ite (Bdd.var set,
                  (Bdd.low set),
                  (Bdd.high set))
      in
      let pow2 x = 1 lsl x in
        (* [pow2 x] returns 2 power x. *)
      let rec to_list cur_bit set =
        try PMap.find (cur_bit,set) !cache_elements
        with Not_found ->
          let res =
            match inspect set with
              | Bool true -> [0]
              | Bool false -> []
              | Ite (var,ff,tt) ->
                  let (ff,tt) =
                    if cur_bit < var
                    then set,set
                    else (assert (cur_bit = var); ff,tt)
                  in
                  let (ffl,ttl) =
                    to_list (succ cur_bit) ff,
                    to_list (succ cur_bit) tt
                  in
                    ffl @ (List.map ((+) (pow2 cur_bit)) ttl)
          in
            cache_elements := PMap.add (cur_bit,set) res !cache_elements;
            res
      in
        to_list 0 set

end

module Make (S:sig val nb_bits:int end) = struct

  module IntSet = MakeBDD(S)

  let class_names : JBasics.class_name Ptmap.t ref = ref Ptmap.empty

  type t = Set of IntSet.t | Bot | Top
  type analysisID = unit
  type analysisDomain = t

  let bot = Bot
  let empty = Set IntSet.empty
  let isBot = (=) Bot

  let singleton cn =
    let ci = JBasics.cn_hash cn in
      class_names := Ptmap.add ci cn !class_names;
      Set (IntSet.singleton ci)

  let is_empty = function
    | Bot -> true
    | Set s -> IntSet.is_empty s
    | Top -> false

  let mem cn = function
    | Bot -> false
    | Top -> true
    | Set s -> IntSet.mem (JBasics.cn_hash cn) s

  let set_size = function
    | Bot -> 0
    | Set s -> List.length (IntSet.elements s)
    | Top -> raise (Invalid_argument "set_size")

  let meet v1 v2 = match v1,v2 with
    | Set s1, Set s2 -> Set (IntSet.inter s1 s2)
    | Bot, _
    | _ , Bot -> Bot
    | Set _ as v, Top
    | Top, (Set _ as v) -> v
    | Top, Top -> Top

  let of_set s =
    let set =
      JBasics.ClassSet.fold
        (fun cn iset ->
           let ci = JBasics.cn_hash cn in
             class_names := Ptmap.add ci cn !class_names;
             IntSet.add ci iset)
        s
        IntSet.empty
    in Set set

  let to_set = function
    | Set s ->
        List.fold_left
          (fun cs ci ->
             let cn = Ptmap.find ci !class_names in
               JBasics.ClassSet.add cn cs)
          JBasics.ClassSet.empty
          (IntSet.elements s)
    | Bot | Top -> invalid_arg "to_set"

  let equal v1 v2 = match v1,v2 with
    | Bot, Bot
    | Top, Top -> true
    | Set s1, Set s2 -> IntSet.equal s1 s2
    | _, _ -> false

  let join ?(modifies=ref false) v1 v2 =
    match v1,v2 with
      | Top, _
      | _, Bot ->
          v1
      | _, Top
      | Bot,_ ->
          modifies:=true;
          v2
      | Set s1, Set s2 ->
          let s = IntSet.union s1 s2
          in
            if IntSet.equal s s1
            then v1
            else
              (modifies := true;
               if IntSet.equal s s2
               then v2
               else Set s)

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else if equal v1 v2
    then v1
    else (modifies := true;v2)

  let get_analysis () v = v

  let pprint fmt = function
    | Bot -> Format.pp_print_string fmt "Bot"
    | Top -> Format.pp_print_string fmt "Top"
    | Set v ->
        Format.pp_print_string fmt "{";
        List.iter
          (fun cni ->
             let cn = Ptmap.find cni !class_names in
               Format.pp_print_string fmt (Javalib.JPrint.class_name cn ^";"))
          (IntSet.elements v);
        Format.pp_print_string fmt  "}"

end
