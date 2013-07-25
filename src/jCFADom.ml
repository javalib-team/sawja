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
open Javalib
open JBasics

type obj = JBirPP.t list * class_name

let obj_compare (lst1,cn1) (lst2,cn2) =
  let rec cmp_list l1 l2 =
    match l1, l2 with
      | [],[] -> 0
      | _l1,[] -> 1
      | [],_l2 -> -1
      | e1::l1, e2::l2 -> 
          (match JBirPP.compare e1 e2 with
             | 0 -> cmp_list l1 l2
             | i -> i)
  in
    match cn_compare cn1 cn2 with
      | 0 -> cmp_list lst1 lst2
      | i -> i

let obj_to_string (pplst, cn) =
  let str_pp = 
    List.fold_left 
      (fun str pp -> 
         Printf.sprintf "%s-[%s]" str (JBirPP.to_string pp)
      )
      ""
      pplst in
    Printf.sprintf "(%s ): %s" str_pp (cn_name cn) 


module DicoObjrMap = Map.Make(struct type t=obj let compare = obj_compare end)
let cur_hash = ref 0
let dicoObj = ref DicoObjrMap.empty

let new_hash _ =
  cur_hash := !cur_hash+1;
  !cur_hash

let get_hash obj =
  try DicoObjrMap.find obj !dicoObj
  with Not_found -> 
    let new_hash = new_hash () in
      dicoObj := DicoObjrMap.add obj new_hash !dicoObj;
      new_hash



module ObjSet = GenericSet.Make (struct type t = obj end)
module ObjMap = GenericMap.Make (struct type t = obj end)

module AbVSet = struct

  type t = 
      Set of (ObjSet.t) 
    | Primitive
    | Bot 
    | Top

  type analysisID = unit
  type analysisDomain = t	

  let bot = Bot

  let top = Top

  let primitive = Primitive

  let isPrimitive t =
    match t with
      | Primitive -> true
      | Top -> assert false
      | _ -> false

  let empty = Set (ObjSet.empty)

  let isBot set = 
    match set with 
      | Bot -> true
      | _ -> false

  let isTop set = 
    match set with
      | Top -> true
      | _ -> false

  let is_empty set = 
    match set with
      | Set s -> ObjSet.is_empty s 
      | Top -> assert false
      | _ -> false

  let singleton pp_lst cn = Set (ObjSet.add (get_hash (pp_lst, cn), (pp_lst, cn)) 
                                   ObjSet.empty)

  let equal set1 set2 =
    match set1, set2 with
      | Bot, Bot -> true
      | Primitive, Primitive -> true
      | Set s1, Set s2 -> ObjSet.equal s1 s2 
      | Top, Top -> true
      | _ -> false

  let inter set1 set2 = 
    match set1, set2 with
      | Bot,_ | _, Bot -> Bot
      | Primitive, Primitive -> Primitive
      | Set s1, Set s2 -> Set (ObjSet.inter s1 s2) 
      | Top, x | x, Top ->  x
      | _ -> assert false (*trying to intersect a primitive and a cn set*)

  let to_string_objset set = 
    let str = 
      ObjSet.fold
        (fun (_hash, obj) str ->
           Printf.sprintf "%s, %s" str (obj_to_string obj)
        )
        set
        "" 
    in 
      Printf.sprintf "{%s}" str



  let to_string set = 
    match set with
      | Bot -> "Bot"
      | Primitive -> "Primitive"
      | Set set -> to_string_objset set
      | Top -> "Top"



  let join ?(modifies=ref false) set1 set2 =
    match set1, set2 with
      | _, Bot -> set1
      | Top, _ -> Top
      | Bot, _ -> modifies:=true; set2
      | _, Top -> modifies:=true; Top
      | Primitive, Primitive -> Primitive
      | Set s1, Set s2 ->
          let union = (ObjSet.union s1 s2)  in
            if (ObjSet.equal union s1)
            then set1
            else (modifies:=true; Set union)
      | _ -> 
(*
         Printf.printf "set1: %s\n" (to_string set1);
         Printf.printf "set2: %s\n" (to_string set2);
         Printf.printf "Var set to bottom";
 *)
         Top 
(*             raise Safe.Domain.DebugDom (*trying to join a primitive and a cn set*)  *)

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else (if equal v1 v2
          then v1
          else (modifies := true;v2))

  let concretize set = 
    match set with
      | Bot | Primitive -> ClassSet.empty
      | Top -> assert false
      | Set st -> ObjSet.fold
                    (fun (_ , (_,cn)) concset ->
                       ClassSet.add cn concset
                    )
                    st
                    ClassSet.empty

  let filter_with_compatible' prog set cn rev=
    let open JProgram in
      match set with
        | Bot  -> Bot
        | Primitive | Top-> raise Safe.Domain.DebugDom (*cannot filter a primitive with a cn*)
        | Set s -> 
            let node = get_node prog cn in
            let s = 
              ObjSet.filter 
                (fun (_,(_,cn_in_set)) -> 
                   try 
                     let node_in_set = 
                       get_node prog cn_in_set 
                     in
                       (match (node_in_set, node, rev) with
                          | Class nd_in_set, Class nd, false ->
                              extends_class nd_in_set nd
                          | Class _nd_in_set, Interface _nd, false ->
                              extends node_in_set node
                          | Class nd_in_set, Class nd, true ->
                              not (extends_class nd_in_set nd)
                          | Class _nd_in_set, Interface _nd, true ->
                              not (extends node_in_set node)
                          | _ -> assert false (*cannot work with Interface*)
                       )
                   with Not_found -> true
                (*It can append for dynamic class which are badly
                 * inserted into the callgraph (I am thinking about array
                 * classes). 
                 * TODO: Handle more correctly array classes.*)
                )
                s
            in Set s

  let filter_with_compatible prog set cn = 
    filter_with_compatible' prog set cn false

  let filter_with_uncompatible prog set cn = 
    filter_with_compatible' prog set cn true


  let pprint_objset fmt set =
    Format.pp_print_string fmt "<";
    ObjSet.iter 
      (fun (_hash,(_pplst,cn)) -> 
         Format.pp_print_string fmt ((cn_name cn)^";")
      ) 
      set;
    Format.pp_print_string fmt ">"



  let pprint fmt set = 
    match set with
      | Bot ->  Format.pp_print_string fmt "Bot" 
      | Top ->  Format.pp_print_string fmt "Top" 
      | Primitive ->  Format.pp_print_string fmt "Primitive" 
      | Set set -> pprint_objset fmt set
      
  let get_analysis _ el = el

end

module AbFSet = struct
  type t = Set of AbVSet.t ObjMap.t | Bot 
  type analysisID = unit
  type analysisDomain = t


  let bot = Bot

  let empty = Set ObjMap.empty

  let isBot set = 
    match set with 
      | Bot -> true
      | _ -> false

  let is_empty set = 
    match set with
      | Set objm ->
          ObjMap.is_empty objm
      | _ -> false

  let equal set1 set2 =
    match set1, set2 with
      | Bot, Bot -> true
      | Bot, _ | _, Bot -> false
      | Set (map1), Set (map2) -> 
          ObjMap.equal AbVSet.equal map1 map2


  let inter set1 set2 = 
    match set1, set2 with
      | Bot,_ | _, Bot -> Bot
      | Set (map1), Set (map2) -> 
          let nmap = 
            ObjMap.fold
              (fun objk set1 nMap ->
                 try let set2 = ObjMap.find objk map2 in
                   ObjMap.add objk (AbVSet.inter set1 set2) nMap
                 with Not_found ->
                   nMap
              )
              map1
              ObjMap.empty in
            Set nmap
          

  let join ?(modifies=ref false) s1 s2 =
    match s1, s2 with
      | _, Bot -> s1
      | Bot, _ -> modifies:=true; s2
      | Set (m1), Set (m2) ->
          let union = 
            ObjMap.fold
              (fun objk set2 nMap ->
                 let set1 = 
                   try ObjMap.find objk m1 
                   with Not_found ->
                     AbVSet.Bot
                 in
                   ObjMap.add objk (AbVSet.join set1 set2) nMap
              )
              m2
              m1
          in
            if ObjMap.equal AbVSet.equal union m1
            then s1
            else (modifies:=true; Set union)

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else if equal v1 v2
    then v1
    else (modifies := true;v2)

  let static_field_dom = 
    let obj = ([],make_cn "static") in
      AbVSet.Set (ObjSet.add ((get_hash obj), obj) (ObjSet.empty))



  let var2fSet objAb varAb = 
    match objAb, varAb with
      | AbVSet.Bot, _ | _, AbVSet.Bot -> Bot
      | AbVSet.Primitive, _ -> assert false 
      | AbVSet.Top, _ -> assert false
      | _, AbVSet.Top -> raise Safe.Domain.DebugDom(*assert false primitive has not fields*)
      | AbVSet.Set objs, vars ->
          let nmap = 
            ObjSet.fold 
              (fun obj nmap ->
                 ObjMap.add obj vars nmap
              )
              objs
              ObjMap.empty in
            Set nmap
       


  let fSet2var fsAb objvset =
    match fsAb, objvset with
      | Bot,_ | _, AbVSet.Bot -> AbVSet.Bot
      | _, AbVSet.Primitive | _, AbVSet.Top -> raise Safe.Domain.DebugDom(*assert false primitive has not fields*)
      | Set fsAb, AbVSet.Set objvset -> 
          let nset = 
(*
          Printf.printf "in\n";
            ObjMap.iter 
              (fun objk set ->
                 let (id ,st) = objk in
                 Printf.printf "objk : %d %s \n" id (obj_to_string st);
                 Printf.printf "set: %s\n" (AbVSet.to_string set )
              )
              fsAb;

 *)

            ObjSet.fold 
              (fun objset nset ->
(*                  let (id ,st) = objset in 
                  Printf.printf "objset : %d %s \n" id (obj_to_string st); *)
                 try AbVSet.join (ObjMap.find objset fsAb) nset
                 with Not_found -> nset
              )
              objvset
              AbVSet.bot
          in
(*           Printf.printf "out\n"; *)
          nset



  let to_string t = 
    match t with 
      | Bot -> "Bot"
      | Set t ->
          let str =
            ObjMap.fold
              (fun (_hashk, objk) set str ->
                 Printf.sprintf "%s, %s:%s" str (obj_to_string objk) (AbVSet.to_string set)
              )
              t
              "" in
            Printf.sprintf "{ %s }" str



  let pprint fmt set = 
    match set with
      | Bot ->  Format.pp_print_string fmt "Bot" 
      | Set map -> 
          Format.pp_print_string fmt "[|";
          ObjMap.iter
            (fun (_hash,(_pplst,cn)) set ->
               let str= Printf.sprintf "%s: {\n" (cn_name cn) in
                 Format.pp_print_string fmt str;
                 AbVSet.pprint fmt set;
                 Format.pp_print_string fmt "}"
            )
            map;
          Format.pp_print_string fmt "|]"

  let get_analysis _ el = el


end

module AbLocals = struct
  include Safe.Domain.Local(AbVSet) 
  let set_var i abV dom =
    match (AbVSet.isBot abV ) with
      | true -> bot
      | _ -> set_var i abV dom

  let to_string t = 
    let buf = Buffer.create 200 in
    let buf_fmt = Format.formatter_of_buffer buf in
      pprint buf_fmt t;
      Format.pp_print_flush buf_fmt ();
    Buffer.contents buf
end


module AbMethod = struct

  type abm = {args: AbLocals.t ; return: AbVSet.t; exc_return: AbVSet.t}
 
  type t = 
    | Bot 
    | Reachable of abm
      
  type analysisID = unit
  type analysisDomain = t	

  let equal v1 v2 : bool =
    match v1, v2 with
      | Bot, Bot -> true
      | Bot, _ | _, Bot -> false
      | Reachable rv1, Reachable rv2 -> 
          AbLocals.equal rv1.args rv2.args
          && AbVSet.equal rv1.return rv2.return
          && AbVSet.equal rv1.exc_return rv2.exc_return
      
  let bot = Bot
    
  let isBot t = match t with | Bot -> true | _ -> false

  let init = Reachable ({args= AbLocals.init; return= AbVSet.bot; 
                         exc_return= AbVSet.bot})

  let get_args v = 
    match v with 
      | Bot -> assert false (*AbLocals.bot*)
      | Reachable v -> v.args

  let init_locals node ms abm =
    let from_args pos args = 
      let curAbs =(AbLocals.get_var pos args) in
        match AbVSet.isBot curAbs with
          | true -> AbVSet.top
          | false -> curAbs
    in
    match abm with
      | Bot ->  AbLocals.bot
      | Reachable s ->
          let (_vars, params) = 
            match (JProgram.get_method node ms) with
              | ConcreteMethod cm ->
                  (match cm.cm_implementation with
                     | Native -> assert false
                     | Java laz ->
                         let a3bir = (Lazy.force laz) in
                           (JBir.vars a3bir, JBir.params a3bir))
              | _ -> assert false
          in
          let pos = ref (-1) in
            List.fold_left
              (fun state (_,curvar) ->
                 pos:=!pos+1;
                 AbLocals.set_var (JBir.index curvar) (from_args !pos s.args) state  
              )
              AbLocals.init
              params



  let get_return v = 
    match v with
      | Bot -> AbVSet.bot
      | Reachable v -> v.return

  let get_exc_return v = 
    match v with
      | Bot -> AbVSet.bot
      | Reachable v -> v.exc_return

  let join_args v a =
    match v with
      | Bot -> Bot
      | Reachable rv -> Reachable {rv with args = AbLocals.join rv.args a;}

  let set_args v a =
    match v with
      | Bot -> Bot
      | Reachable rv -> Reachable {rv with args = a}
      
  let join_return v r =
    match v with
      | Bot -> Bot
      | Reachable v -> Reachable {v with return = AbVSet.join v.return r;}

 
  let join_exc_return v r =
    match v with
      | Bot -> Bot
      | Reachable v -> Reachable {v with exc_return = AbVSet.join v.exc_return r;}


  let join ?(modifies=ref false) 
      v1 v2  =
    if v1 == v2 
    then v1
    else
      match v1, v2 with
        | Bot, v2 -> modifies:=true;v2 
        | v1, Bot -> v1
        | Reachable ({args=ar1; return=r1; exc_return=excr1 }), 
          Reachable ({args=ar2; return=r2; exc_return=excr2}) -> 
            let (ma,mr, mer) = (ref false, ref false, ref false) in
            let nargs = AbLocals.join ~modifies:ma ar1 ar2 in
            let nreturn = AbVSet.join ~modifies:mr r1 r2 in
            let nexcreturn = AbVSet.join ~modifies:mr excr1 excr2 in
            modifies:= !ma || !mr || !mer;
            Reachable {args=nargs; return=nreturn; exc_return=nexcreturn} 


  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else if equal v1 v2
    then v1
    else (modifies := true;v2)

  let to_string t = 
    match t with 
      | Bot -> "Bot"
      | Reachable t ->
          let (args, ret, exc_ret) = (t.args,t.return,t.exc_return) in
          let str_arg =
            AbLocals.to_string args in
          let str_ret = AbVSet.to_string ret in
          let str_exc_ret = AbVSet.to_string exc_ret in
            Printf.sprintf "args: { %s }; ret : %s exc_ret: %s" str_arg str_ret
              str_exc_ret


  let pprint fmt t =
    let open Format in
    match t with
      | Bot -> pp_print_string fmt "Bot"
      | Reachable t ->
          let open Format in
            pp_open_hvbox fmt 0;
            pp_print_string fmt "args:";
            AbLocals.pprint fmt t.args;
            pp_print_space fmt ();
            pp_print_string fmt "return:";
            AbVSet.pprint fmt t.return;
            pp_print_space fmt ();
            pp_print_string fmt "exceptionnal return:";
            AbVSet.pprint fmt t.exc_return;
            pp_print_space fmt ();
            pp_close_box fmt ()

    let get_analysis () v = v    

end

module Var = Safe.Var.Make(Safe.Var.EmptyContext)

module AbField = (AbFSet:Safe.Domain.S
                   with type t = AbFSet.t
                   and type analysisDomain = AbFSet.t
                   and type analysisID = AbFSet.analysisID)

module AbPP = (AbLocals:Safe.Domain.S
                   with type t = AbLocals.t
                   and type analysisDomain = AbLocals.t
                   and type analysisID = AbLocals.analysisID)

module AbMeth = (AbMethod:Safe.Domain.S
                   with type t = AbMethod.t
                   and type analysisDomain = AbMethod.t
                   and type analysisID = AbMethod.analysisID)



module CFAState =  Safe.State.Make(Var)(Safe.Domain.Empty)(Safe.Domain.Empty)(AbField)(AbMeth)(AbPP)

module CFAConstraints = Safe.Constraints.Make(CFAState) 
