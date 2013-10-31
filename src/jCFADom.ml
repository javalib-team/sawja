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
open JType

  (*allocation site, identified by a list of pp points representing a partial
  * stack where the allocation occured and the object type.*)
type asite = JBirPP.t list * object_type

let asite_compare (lst1,cn1) (lst2,cn2) =
  let cmp_list l1 l2 =
    match l1, l2 with
      | [], [] -> 0
      | [], e2::_ -> 1
      | _e1::_, [] -> -1
      | e1::_, e2::_ -> JBirPP.compare e1 e2 
      | _ -> assert false (*In the current implementation is always a list with
            a 0 (for static field) or 1 element.*)
  in
    match obj_compare cn1 cn2 with
      | i -> i
      | 0 -> cmp_list lst1 lst2

let asite_to_string (pplst, obj) =
  let str_pp = 
    List.fold_left 
      (fun str pp -> 
         Printf.sprintf "%s-[%s]" str (JBirPP.to_string pp)
      )
      ""
      pplst in
    Printf.sprintf "(%s ): %s" str_pp (JPrint.object_type obj) 


module DicoSiteMap = Map.Make(struct type t=asite let compare = asite_compare end)
let cur_hash = ref 0
let dicoObj = ref DicoSiteMap.empty

let new_hash _ =
  cur_hash := !cur_hash+1;
  !cur_hash

let get_hash asite =
  try DicoSiteMap.find asite !dicoObj
  with Not_found -> 
    let new_hash = new_hash () in
      dicoObj := DicoSiteMap.add asite new_hash !dicoObj;
      new_hash



module SiteSet = GenericSet.Make (struct 
                                   type t = asite 
                                   let get_hash = get_hash 
                                 end)
module SiteMap = GenericMap.Make (struct 
                                   type t = asite
                                   let get_hash = get_hash
                                 end)

module AbVSet = struct

  type t = 
      Set of (SiteSet.t) 
    | Primitive
    | Bot 
    | Top

  type analysisID = unit
  type analysisDomain = t	

  let bot = Bot

  let primitive = Primitive

  let isPrimitive t =
    match t with
      | Primitive -> true
      | Top -> raise Safe.Domain.DebugDom
      | _ -> false

  let empty = Set (SiteSet.empty)

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
      | Set s -> SiteSet.is_empty s 
      | Top -> raise Safe.Domain.DebugDom
      | _ -> false

  let singleton pp_lst obj = Set (SiteSet.add (pp_lst, obj) SiteSet.empty)

  let equal set1 set2 =
    match set1, set2 with
      | Bot, Bot -> true
      | Primitive, Primitive -> true
      | Set s1, Set s2 -> SiteSet.equal s1 s2 
      | Top, Top -> true
      | _ -> false

  let inter set1 set2 = 
    match set1, set2 with
      | Bot,_ | _, Bot -> Bot
      | Primitive, Primitive -> Primitive
      | Set s1, Set s2 -> Set (SiteSet.inter s1 s2) 
      | Top, x | x, Top ->  x
      | _ -> raise Safe.Domain.DebugDom(*trying to intersect a primitive and a object set*)

  let to_string_siteset set = 
    let str = 
      SiteSet.fold
        (fun site str ->
           Printf.sprintf "%s, %s" str (asite_to_string site)
        )
        set
        "" 
    in 
      Printf.sprintf "{%s}" str



  let to_string set = 
    match set with
      | Bot -> "Bot"
      | Primitive -> "Primitive"
      | Set set -> to_string_siteset set
      | Top -> "Top"



  let join ?(modifies=ref false) set1 set2 =
    match set1, set2 with
      | _, Bot -> set1
      | Top, _ -> Top
      | Bot, _ -> modifies:=true; set2
      | _, Top -> modifies:=true; Top
      | Primitive, Primitive -> Primitive
      | Set s1, Set s2 ->
          let union = (SiteSet.union s1 s2)  in
            if (SiteSet.equal union s1)
            then set1
            else (modifies:=true; Set union)
      | _ -> 
         Top 

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else (if equal v1 v2
          then v1
          else (modifies := true;v2))

  let concretize set = 
    match set with
      | Bot | Primitive -> ObjectSet.empty
      | Top -> raise Safe.Domain.DebugDom
      | Set st -> SiteSet.fold
                    (fun (_,obj) concset ->
                       ObjectSet.add obj concset
                    )
                    st
                    ObjectSet.empty

  (*If [rev] is false, return the subset of [set] containing only object which
   * are subtype of [objt] (or equal to [objt]. 
   * If [rev] is true, return the subset of [set] containing only object which
   * are not subtype of [objt] (or which are not equal). 
  * *)
  let filter_with_compatible' prog set objt rev=
      match set with
        | Bot  -> Bot
        | Primitive | Top-> raise Safe.Domain.DebugDom (*cannot filter a primitive with a cn*)
        | Set s -> 
            let s = 
              SiteSet.filter 
                (fun (_,obj_in_set) -> 
                   match rev with
                     | false -> (obj_compare obj_in_set objt = 0) || 
                                subtype prog obj_in_set objt
                     | true -> (obj_compare obj_in_set objt <> 0) && 
                               not (subtype prog obj_in_set objt)
                )
                s
            in Set s

  let filter_with_compatible prog set objt = 
    filter_with_compatible' prog set objt false

  let filter_with_uncompatible prog set objt = 
    filter_with_compatible' prog set objt true


  let pprint_objset fmt set =
    Format.pp_print_string fmt "<";
    SiteSet.iter 
      (fun (_pplst,obj) -> 
         Format.pp_print_string fmt ((JPrint.object_type obj)^";")
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

  type t = Set of AbVSet.t SiteMap.t | Bot 
  type analysisID = unit
  type analysisDomain = t

  let bot = Bot

  let empty = Set SiteMap.empty

  let isBot set = 
    match set with 
      | Bot -> true
      | _ -> false

  let is_empty set = 
    match set with
      | Set objm -> SiteMap.is_empty objm
      | _ -> false

  let equal set1 set2 =
    match set1, set2 with
      | Bot, Bot -> true
      | Bot, _ | _, Bot -> false
      | Set (map1), Set (map2) -> 
          SiteMap.equal AbVSet.equal map1 map2


  let inter set1 set2 = 
    match set1, set2 with
      | Bot,_ | _, Bot -> Bot
      | Set (map1), Set (map2) -> 
          let nmap = 
            SiteMap.fold
              (fun objk set1 nMap ->
                 try let set2 = SiteMap.find objk map2 in
                   SiteMap.add objk (AbVSet.inter set1 set2) nMap
                 with Not_found ->
                   nMap
              )
              map1
              SiteMap.empty in
            Set nmap
          

  let join ?(modifies=ref false) s1 s2 =
    match s1, s2 with
      | _, Bot -> s1
      | Bot, _ -> modifies:=true; s2
      | Set (m1), Set (m2) ->
          let union = 
            SiteMap.fold
              (fun objk set2 nMap ->
                 let set1 = 
                   try SiteMap.find objk m1 
                   with Not_found ->
                     AbVSet.Bot
                 in
                   SiteMap.add objk (AbVSet.join set1 set2) nMap
              )
              m2
              m1
          in
            if SiteMap.equal AbVSet.equal union m1
            then s1
            else (modifies:=true; Set union)

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else if equal v1 v2
    then v1
    else (modifies := true;v2)

  let static_field_dom = 
    let site = ([],TClass (make_cn "static")) in
      AbVSet.Set (SiteSet.add site (SiteSet.empty))



  let var2fSet objAb varAb = 
    match objAb, varAb with
      | AbVSet.Bot, _ | _, AbVSet.Bot -> Bot
      | AbVSet.Primitive, _ -> raise Safe.Domain.DebugDom
      | AbVSet.Top, _ -> raise Safe.Domain.DebugDom
      | _, AbVSet.Top -> raise Safe.Domain.DebugDom(*assert false primitive has not fields*)
      | AbVSet.Set sites, vars ->
          let nmap = 
            SiteSet.fold 
              (fun site nmap ->
                 SiteMap.add site vars nmap
              )
              sites
              SiteMap.empty in
            Set nmap
       


  let fSet2var fsAb siteset =
    match fsAb, siteset with
      | Bot,_ | _, AbVSet.Bot -> AbVSet.Bot
      | _, AbVSet.Primitive | _, AbVSet.Top -> raise Safe.Domain.DebugDom(*assert false primitive has not fields*)
      | Set fsAb, AbVSet.Set siteset -> 
          let nset = 
            SiteSet.fold 
              (fun siteset nset ->
                 try AbVSet.join (SiteMap.find siteset fsAb) nset
                 with Not_found -> nset
              )
              siteset
              AbVSet.bot
          in
          nset



  let to_string t = 
    match t with 
      | Bot -> "Bot"
      | Set t ->
          let str =
            SiteMap.fold
              (fun objk set str ->
                 Printf.sprintf "%s, %s:%s" str (asite_to_string objk) (AbVSet.to_string set)
              )
              t
              "" in
            Printf.sprintf "{ %s }" str



  let pprint fmt set = 
    match set with
      | Bot ->  Format.pp_print_string fmt "Bot" 
      | Set map -> 
          Format.pp_print_string fmt "[|";
          SiteMap.iter
            (fun (_pplst,objtyp) set ->
               let str= Printf.sprintf "%s: {\n" (JPrint.object_type objtyp) in
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
      | Bot -> raise Safe.Domain.DebugDom(*AbLocals.bot*)
      | Reachable v -> v.args

  let init_locals node ms abm =
    match abm with
      | Bot ->  AbLocals.bot
      | Reachable s ->
          let  params = 
            match (JProgram.get_method node ms) with
              | ConcreteMethod cm ->
                  (match cm.cm_implementation with
                     | Native -> raise Safe.Domain.DebugDom
                     | Java laz ->
                         let a3bir = (Lazy.force laz) in
                           JBir.params a3bir)
              | _ -> raise Safe.Domain.DebugDom
          in
          let pos = ref (-1) in
            List.fold_left
              (fun state (_,curvar) ->
                 pos:=!pos+1;
                 AbLocals.set_var (JBir.index curvar) (AbLocals.get_var !pos s.args) state  
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
