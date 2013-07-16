open Javalib_pack
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

  type analysisID = unit
  type analysisDomain = t	

  let bot = Bot

  let primitive = Primitive

  let isPrimitive t =
    match t with
      | Primitive -> true
      | _ -> false

  let empty = Set (ObjSet.empty)

  let isBot set = 
    match set with 
      | Bot -> true
      | _ -> false

  let is_empty set = 
    match set with
      | Set s -> ObjSet.is_empty s 
      | _ -> false

  let singleton pp_lst cn = Set (ObjSet.add (get_hash (pp_lst, cn), (pp_lst, cn)) 
                                   ObjSet.empty)

  let equal set1 set2 =
    match set1, set2 with
      | Bot, Bot -> true
      | Primitive, Primitive -> true
      | Set s1, Set s2 -> ObjSet.equal s1 s2 
      | _ -> false

  let inter set1 set2 = 
    match set1, set2 with
      | Bot,_ | _, Bot -> Bot
      | Primitive, Primitive -> Primitive
      | Set s1, Set s2 -> Set (ObjSet.inter s1 s2) 
      | _ -> assert false (*trying to intersect a primitive and a cn set*)

  let join ?(modifies=ref false) set1 set2 =
    match set1, set2 with
      | _, Bot -> set1
      | Bot, _ -> modifies:=true; set2
      | Primitive, Primitive -> Primitive
      | Set s1, Set s2 ->
          let union = (ObjSet.union s1 s2)  in
            if (ObjSet.equal union s1)
            then set1
            else (modifies:=true; Set union)
      | _ -> assert false (*trying to join a primitive and a cn set*)

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else (if equal v1 v2
          then v1
          else (modifies := true;v2))

  let concretize set = 
    match set with
      | Bot | Primitive -> ClassSet.empty
      | Set st -> ObjSet.fold
                    (fun (_ , (_,cn)) concset ->
                       ClassSet.add cn concset
                    )
                    st
                    ClassSet.empty

  let filter_with_compatible prog set cn =
    let open JProgram in
      match set with
        | Bot  -> Bot
        | Primitive -> assert false (*cannot filter a primitive with a cn*)
        | Set s -> 
            let node = get_node prog cn in
            let s = 
              ObjSet.filter 
                (fun (_,(_,cn_in_set)) -> 
                   let node_in_set = get_node prog cn_in_set in
                     (match (node_in_set, node) with
                        | Class nd_in_set, Class nd ->
                            extends_class nd_in_set nd
                        | _ -> assert false (*cannot work with Interface*)
                     )
                )
                s
            in Set s



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
      | Primitive ->  Format.pp_print_string fmt "Primitive" 
      | Set set -> pprint_objset fmt set
      
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
    Printf.printf "entering AbVSet to_string";
    match set with
      | Bot -> "Bot"
      | Primitive -> "Primitive"
      | Set set -> to_string_objset set

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
                 let set1 = ObjMap.find objk m1 in
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
      | AbVSet.Primitive, _ -> assert false (*primitive has not fields*)
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
      | _, AbVSet.Primitive -> assert false
      | Set fsAb, AbVSet.Set objvset -> 
          Printf.printf "in\n";
          let nset = 
            ObjMap.iter 
              (fun objk _ ->
                 let (id ,st) = objk in
                 Printf.printf "objk : %d %s \n" id (obj_to_string st);
              )
              fsAb;


            ObjSet.fold 
              (fun objset nset ->
                 let (id ,st) = objset in
                 Printf.printf "objset : %d %s \n" id (obj_to_string st);
                 try AbVSet.join (ObjMap.find objset fsAb) nset
                 with Not_found -> nset
              )
              objvset
              AbVSet.empty
          in
          Printf.printf "out\n";
            if AbVSet.is_empty nset
            then assert false
            else nset


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
          ObjMap.iter
            (fun (_hash,(_pplst,cn)) set ->
               let str= Printf.sprintf "%s: {\n" (cn_name cn) in
                 Format.pp_print_string fmt str;
                 AbVSet.pprint fmt set;
                 Format.pp_print_string fmt "}"
            )
            map

  let get_analysis _ el = el


end

module AbLocals = struct
  include Safe.Domain.Local(AbVSet) 
  let to_string t = 
    let buf = Buffer.create 200 in
    let buf_fmt = Format.formatter_of_buffer buf in
      pprint buf_fmt t;
      Format.pp_print_flush buf_fmt ();
    Buffer.contents buf
end


module AbMethod = struct

  type abm = {args: AbLocals.t ; return: AbVSet.t;}
 
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
      
  let bot = Bot
    
  let isBot t = match t with | Bot -> true | _ -> false

  let init = Reachable ({args= AbLocals.init; return= AbVSet.bot})

  let get_args v = 
    match v with 
      | Bot -> AbLocals.bot
      | Reachable v -> v.args

  let get_return v = 
    match v with
      | Bot -> AbVSet.bot
      | Reachable v -> v.return

  let join_args v a =
    match v with
      | Bot -> Bot
      | Reachable rv -> Reachable {rv with args = AbLocals.join rv.args a;}
      
  let join_return v r =
    match v with
      | Bot -> Bot
      | Reachable v -> Reachable {v with return = AbVSet.join v.return r;}

  let join ?(modifies=ref false) 
      v1 v2  =
    if v1 == v2 
    then v1
    else
      match v1, v2 with
        | Bot, v2 -> modifies:=true;v2 
        | v1, Bot -> v1
        | Reachable ({args=ar1; return=r1}), Reachable ({args=ar2; return=r2}) -> 
            let (ma,mr) = (ref false, ref false) in
            let nargs = AbLocals.join ~modifies:ma ar1 ar2 in
            let nreturn = AbVSet.join ~modifies:mr r1 r2 in
            modifies:= !ma || !mr;
            Reachable {args=nargs; return=nreturn;} 


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
          let (args, ret) = (t.args,t.return) in
          let str_arg =
            AbLocals.to_string args in
          let str_ret = AbVSet.to_string ret in
            Printf.sprintf "args: { %s }; ret : %s " str_arg str_ret


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
