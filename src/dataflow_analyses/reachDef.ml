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



module Lat = struct
  (* canonic lattice on (JBir.var -> Powerset(PC)) *)
  type t = Ptset.t Ptmap.t

  let bot = Ptmap.empty

  let join = Ptmap.merge Ptset.union

  let get m x = 
    try Ptmap.find x m
    with Not_found -> Ptset.empty

  let order m1 m2 =
    Ptmap.fold
      (fun i s b -> b && Ptset.subset s (get m2 i))
      m1 true

  let get m x = get m (JBir.index x)

  let to_string code ab =
    let print i =
      if i < 0 then "?" else string_of_int i in
    let set_to_string s =
      match List.map print (Ptset.elements s) with
	| [] -> "{}"
	| [x] -> Printf.sprintf "{%s}" x
	| x::q -> Printf.sprintf "{%s%s}" x (List.fold_right (fun x s -> ","^x^s) q "")	 in
      Ptmap.fold (fun i set -> Printf.sprintf "%s:%s %s"
		    (JBir.var_name_g ((JBir.vars code).(i)))
		    (set_to_string set)) ab ""
end
  
type pc = int
type transfer = 
  | Nop
  | KillGen of JBir.var * int

let transfer_to_string = function 
  | Nop -> "Nop"
  | KillGen (x,i) -> Printf.sprintf "KillGen(%s,%d)" (JBir.var_name_g x) i
      
let eval_transfer = function
  | Nop -> (fun ab -> ab)
  | KillGen (x,i) -> Ptmap.add (JBir.index x) (Ptset.singleton i) 

(* [gen_instrs last i] computes a list of transfert function [(f,j);...] with
   [j] the successor of [i] for the transfert function [f]. *)
let gen_instrs i = function
  | JBir.Ifd (_, j) -> [(Nop,j);(Nop,i+1)]
  | JBir.Goto j -> [Nop,j]
  | JBir.Throw _
  | JBir.Return _  -> []
  | JBir.AffectVar (x,_) 
  | JBir.NewArray (x,_,_)
  | JBir.New (x,_,_,_) 
  | JBir.InvokeStatic (Some x,_,_,_) 
  | JBir.InvokeVirtual (Some x,_,_,_,_) 
  | JBir.InvokeNonVirtual (Some x,_,_,_,_) -> [KillGen (x,i),i+1]
  | JBir.MonitorEnter _
  | JBir.MonitorExit _ -> [Nop,i+1]
  | JBir.AffectStaticField _
  | JBir.AffectField _
  | JBir.AffectArray _
  | JBir.InvokeStatic _
  | JBir.InvokeVirtual _ 
  | JBir.InvokeNonVirtual _ 
  | JBir.MayInit _ 
  | JBir.Check _
  | JBir.Formula _
  | JBir.Nop -> [Nop,i+1]

(* generate a list of transfer functions *)
let gen_symbolic (m:JBir.t) : (pc * transfer * pc) list = 
    JUtil.foldi 
      (fun i ins l ->
	 List.rev_append
	   (List.map (fun (c,j) -> (i,c,j)) (gen_instrs i ins))
	   l) 
      (List.map (fun (i,e) -> (i,Nop,e.JBir.e_handler)) (JBir.exception_edges m))
      (JBir.code m)

let unknown = -1 

let init params =
  List.fold_right
    (fun (_,x) -> Ptmap.add (JBir.index x) (Ptset.singleton unknown))
    params
    Ptmap.empty

let run m =
  let init = init (JBir.params m) in
    Iter.run 
      {
	Iter.bot = Lat.bot ;
	Iter.join = Lat.join;
	Iter.leq = Lat.order;
	Iter.eval = eval_transfer;
	Iter.normalize = (fun x -> x);
	Iter.size = Array.length (JBir.code m);
	Iter.workset_strategy = Iter.Incr;
	Iter.cstrs = gen_symbolic m;
	Iter.init_points = [0];
	Iter.init_value = (fun _ -> init); (* useless here since we iterate from bottom *)
	Iter.verbose = false;
	Iter.dom_to_string = Lat.to_string m;
	Iter.transfer_to_string = transfer_to_string
      }
let run2 m =
  let init = init (JBir.params m) in
    Iter2.run 
      {
	Iter2.bot = Lat.bot ;
	Iter2.join = Lat.join;
	Iter2.leq = Lat.order;
	Iter2.eval = eval_transfer;
	Iter2.normalize = (fun x -> x);
	Iter2.cstrs = gen_symbolic m;
	Iter2.verbose = false;
	Iter2.dom_to_string = Lat.to_string m;
	Iter2.transfer_to_string = transfer_to_string
      }



