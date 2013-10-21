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


exception DebugDom

module type TRADUCTOR_ANALYSIS = sig
  type localID
  type localDomain
  type globalID
  type globalDomain
  val loc2gloID : localID -> globalID
  val loc2gloDomain : localDomain -> globalDomain
  val glo2locID : globalID -> localID
  val glo2locDomain : globalDomain -> localDomain
end


module Trad_Identity (TYPE:sig type id type dom end)= struct
  type localID = TYPE.id
  type localDomain = TYPE.dom
  type globalID = TYPE.id
  type globalDomain = TYPE.dom
  let loc2gloID : localID -> globalID = fun x -> x
  let loc2gloDomain : localDomain -> globalDomain = fun x -> x
  let glo2locID : globalID -> localID = fun x -> x
  let glo2locDomain : globalDomain -> localDomain = fun x -> x
end

module type S = sig
  (* combined sub-analysis domains (eg. D1.t * D2.t)*)
  type t

  (* combined sub-analyses IDs (Left of D1.analysisID | Right of D2.analysisID)*)
  type analysisID

  (* sub-analysis domains (eg. Left of D1.analysisDomain | Right of D2.analysisDomain) *)
  type analysisDomain

  val bot : t
  val isBot : analysisDomain -> bool
  val join : ?modifies:bool ref -> t -> t -> t
  val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
  val equal : t -> t -> bool
  val get_analysis : analysisID -> t -> analysisDomain
  val pprint : Format.formatter -> t -> unit
end


module Stack(Var:S) : sig
  type t = Bot | Top | Stack of Var.t list
  type analysisID = Var.analysisID
  type analysisDomain = t
  val bot : t
  val top : t
  val isBot : analysisDomain -> bool
  val isTop : analysisDomain -> bool
  val join : ?modifies:bool ref -> t -> t -> t
  val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
  val equal : t -> t -> bool
  val get_analysis : analysisID -> t -> analysisDomain
  val pprint : Format.formatter -> t -> unit
  val init : t
    (** initial (empty) stack *)
  val push : Var.t -> t -> t
  val pop_n : int -> t -> t
  val pop : t -> t
  val first : t -> Var.t
    (** raise [Invalid_argument] if the stack is empty. raise Failure if the
        stack is Top. *)
  val dup : t -> t
  val dupX1 : t -> t
  val dupX2 : t -> t
  val dup2 : t -> t
  val dup2X1 : t -> t
  val dup2X2 : t -> t
  val swap : t -> t

end = struct
  type t = Bot | Top | Stack of Var.t list
  type analysisID = Var.analysisID
  type analysisDomain = t
  let get_analysis _ v = v
  let bot = Bot
  let top = Top
  let init = Stack []
  let isBot v = (v == Bot)
  let isTop v = (v == Top)
  let equal v1 v2 = match v1,v2 with
    | v1,v2 when v1==v2 -> true
    | Stack v1, Stack v2 ->
        (List.length v1 == List.length v2)
        && List.for_all2 Var.equal v1 v2
    | Bot, Bot -> true
    | Top, Top -> true
    | _,_ -> false

  let join ?(modifies=ref false) v1 v2 = match v1,v2 with
    | v1,v2 when v1==v2 -> v1
    | Stack _, Bot
    | Bot, Bot
    | Top, _ -> v1
    | Bot, Stack _
    | _, Top -> modifies := true; v2
    | Stack l1, Stack l2 ->
        if (List.length l1 != List.length l2)
        then (modifies:= true; Top)
        else
          let l'=
            List.map2
              (fun e1 e2 -> Var.join ~modifies e1 e2)
              l1 l2
          in
            if List.for_all2 Var.equal l' l1
            then Stack l1
            else (modifies := true; Stack l')

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else if equal v1 v2
    then v1
    else (modifies := true;v2)

  let push v = function
    | Stack l -> Stack (v::l)
    | Bot -> Bot
    | Top -> Top
  let pop_n n = function
    | Stack l -> Stack (ExtList.List.drop n l)
    | Bot -> Bot
    | Top -> Top
  let pop v = pop_n 1 v

  let first = function
    | Stack (e::_) -> e
    | Top -> failwith "Stack.first impossible on top"
    | Bot -> Var.bot
    | Stack [] -> invalid_arg "Stack.first"

  let dup = function
    | Stack (e::_ as r) -> Stack (e::r)
    | Top -> Top
    | _ -> Bot
  let dupX1 = function
    | Stack (e1::e2::r) -> Stack (e1::e2::e1::r)
    | Top -> Top
    | _ -> Bot
  let dupX2 = function
    | Stack (e1::e2::e3::r) -> Stack (e1::e2::e3::e1::r)
    | Top -> Top
    | _ -> Bot

  let dup2 = function
    | Stack (e1::e2::_ as r) -> Stack (e1::e2::r)
    | Top -> Top
    | _ -> Bot
  let dup2X1 = function
    | Stack (e1::e2::e3::r) -> Stack (e1::e2::e3::e1::e2::r)
    | Top -> Top
    | _ -> Bot
  let dup2X2 = function
    | Stack (e1::e2::e3::e4::r) -> Stack (e1::e2::e3::e4::e1::e2::r)
    | Top -> Top
    | _ -> Bot

  let swap = function
    | Stack (e1::e2::r) -> Stack (e2::e1::r)
    | Top -> Top
    | _ -> Bot

  let pprint fmt = 
    let print_string = Format.pp_print_string fmt
    in function
      | Top -> print_string "Top"
      | Bot -> print_string "Bot"
      | Stack val_list ->
          List.iter
            (fun value -> Var.pprint fmt value;print_string "::")
            val_list;
          print_string "[]"

end

module Local (Var:S) :sig
  type t
  type analysisID = Var.analysisID
  type analysisDomain = t
  val bot : t
  val init : t
  val isBot : analysisDomain -> bool
  val join : ?modifies:bool ref -> t -> t -> t
  val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
  val equal : t -> t -> bool
  val get_analysis : analysisID -> t -> analysisDomain
  val pprint : Format.formatter -> t -> unit
  val get_var : int -> analysisDomain -> Var.t
  val set_var : int -> Var.t -> analysisDomain -> analysisDomain
end = struct
  type t = 
      Bot                       (*No map (Unreachable code)*)
    | Local of Var.t Ptmap.t    (*Map of local variable (in reachable code)*)

  type analysisID = Var.analysisID
  type analysisDomain = t
  let get_analysis _ v = v

  let bot = Bot
  let isBot = function Bot -> true | _ -> false

  let init = Local Ptmap.empty

  let join ?(modifies=ref false) v1 v2 = match v1,v2 with
    | v1,v2 when v1==v2 -> v1
    | _, Bot -> v1
    | Bot, _ -> modifies:=true; v2
    | Local l1, Local l2 ->
        let l' = Ptmap.merge Var.join l1 l2
        in
          if 0 == (Ptmap.compare
                     (fun v1 v2 -> if Var.equal v1 v2 then 0 else -1)
                     l1 l')
          then v1
          else (modifies := true; Local l')

  let equal v1 v2 = match v1,v2 with
    | v1, v2 when v1==v2 -> true
    | Bot, _
    | _, Bot -> false
    | Local l1, Local l2 ->
        0 == (Ptmap.compare
                (fun v1 v2 -> if Var.equal v1 v2 then 0 else -1)
                l1 l2)

  let join_ad ?(do_join=true) ?(modifies=ref false) v1 v2 =
    if do_join
    then join ~modifies v1 v2
    else if equal v1 v2
    then v1
    else (modifies := true;v2)

  let get_var v = function
    | Bot -> Var.bot
    | Local l ->
        try
          Ptmap.find v l
        with Not_found -> Var.bot

  let set_var v d = function
    | Bot -> Bot
    | Local l -> Local (Ptmap.add v d l)

  let pprint fmt =
    let print_string = Format.pp_print_string fmt
    in function
      | Bot -> print_string "Bot"
      | Local loc_map ->
          if Ptmap.is_empty loc_map
          then print_string "[||]"
          else
            let print_loc (i,loc) =
              print_string (string_of_int i^":");
              Var.pprint fmt loc
            and loc_list =
              List.sort
                (fun (i1,_) (i2,_) -> compare i1 i2)
                (Ptmap.fold (fun i l r -> (i,l)::r) loc_map [])
            in
              print_string "[|";
              print_loc (List.hd loc_list);
              List.iter
                (fun iloc -> print_string ";";print_loc iloc)
                (List.tl loc_list);
              print_string "|]"
end


module Combine
  (Left:S)
  (Right:S) :
sig

  include S

  module Trad_Left :
    functor (Trad:TRADUCTOR_ANALYSIS
             with type globalID = Left.analysisID
             and type globalDomain = Left.analysisDomain) ->
      (TRADUCTOR_ANALYSIS
       with type localID = Trad.localID
       and type localDomain = Trad.localDomain
       and type globalID = analysisID
       and type globalDomain = analysisDomain)

  module Trad_Right :
    functor (Trad:TRADUCTOR_ANALYSIS
             with type globalID = Right.analysisID
             and type globalDomain = Right.analysisDomain) ->
      (TRADUCTOR_ANALYSIS
       with type localID = Trad.localID
       and type localDomain = Trad.localDomain
       and type globalID = analysisID
       and type globalDomain = analysisDomain)

end =
struct

  type analysisID =
    | LID of Left.analysisID
    | RID of Right.analysisID
  type analysisDomain =
    | LDom of Left.analysisDomain
    | RDom of Right.analysisDomain

  module Trad_Left
    (Trad:TRADUCTOR_ANALYSIS
     with type globalID = Left.analysisID
     and type globalDomain = Left.analysisDomain) = struct
    type localID = Trad.localID
    type localDomain = Trad.localDomain
    type globalID = analysisID
    type globalDomain = analysisDomain
    let loc2gloID x = LID (Trad.loc2gloID x)
    let loc2gloDomain x = LDom (Trad.loc2gloDomain x)
    let glo2locID = function
      | LID x -> Trad.glo2locID x
      | RID _ -> invalid_arg "glo2locID"
    let glo2locDomain = function
      | LDom x -> Trad.glo2locDomain x
      | RDom _ -> invalid_arg "glo2locDomain"
  end

  module Trad_Right
    (Trad:TRADUCTOR_ANALYSIS
     with type globalID = Right.analysisID
     and type globalDomain = Right.analysisDomain) = struct
    type localID = Trad.localID
    type localDomain = Trad.localDomain
    type globalID = analysisID
    type globalDomain = analysisDomain
    let loc2gloID x = RID (Trad.loc2gloID x)
    let loc2gloDomain x = RDom (Trad.loc2gloDomain x)
    let glo2locID = function
      | RID x -> Trad.glo2locID x
      | LID _ -> invalid_arg "glo2locID"
    let glo2locDomain = function
      | RDom x -> Trad.glo2locDomain x
      | LDom _ -> invalid_arg "glo2locDomain"
  end

  type t = Left.t * Right.t

  let bot= (Left.bot,Right.bot)
  let isBot = function
    | RDom x -> Right.isBot x
    | LDom x -> Left.isBot x

  let join_ad ?(do_join=true) ?(modifies=ref false) (l,r) v =
    match v with
      | LDom v ->
	  let l' = Left.join_ad ~do_join ~modifies l v in
	    if !modifies
	    then (l',r)
	    else (l,r)
      | RDom v ->
	  let r' = Right.join_ad ~do_join ~modifies r v in
	    if !modifies
	    then (l,r')
	    else (l,r)

  let join ?(modifies=ref false) ((l1,r1) as v1) (l2,r2) =
    let res = (Left.join ~modifies l1 l2, Right.join ~modifies r1 r2)
    in
      if not !modifies
      then v1
      else res

  let equal (l1,r1) (l2,r2) = Left.equal l1 l2 && Right.equal r1 r2

  let get_analysis a (l,r) = match a with
    | LID a -> LDom (Left.get_analysis a l)
    | RID a -> RDom (Right.get_analysis a r)

  let pprint fmt (l,r) =
    Format.fprintf fmt "(@[<hv>%a,%a@])"
      Left.pprint l Right.pprint r
end


module Empty : S = struct
  type t = unit
  type analysisID = [`Void]
  type analysisDomain = [`VoidData]
  let bot = ()
  let isBot _ = true
  let join_ad ?do_join ?modifies () `VoidData = ignore do_join;ignore modifies
  let join  ?modifies () () = ignore modifies
  let equal () () = true
  let get_analysis `Void () = `VoidData
  let pprint fmt () = Format.pp_print_string fmt "()"
end

