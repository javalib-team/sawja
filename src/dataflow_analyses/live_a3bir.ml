(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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



module Env = struct
  (* lattice of powerset of JBir variables *)
  include Set.Make(struct type t = A3Bir.var let compare = compare end)

  let print_key  = A3Bir.var_name_g

  let bot = empty

  let tvars_in_expr = function
    | A3Bir.Const _ 
    | A3Bir.StaticField _ -> []
    | A3Bir.Var x
    | A3Bir.Field (x,_,_) 
    | A3Bir.Unop (_,x) -> [x]
    | A3Bir.Binop (_,x1,x2) -> [x1;x2]


  let to_string ab =
    let ab = elements ab in
      match List.map print_key ab with
	| [] -> "{}"
	| [x] -> Printf.sprintf "{%s}" x
	| x::q -> Printf.sprintf "{%s%s}" x (List.fold_right (fun x s -> ","^x^s) q "")	 
end
  
type transfer_fun =
  | GenVars of A3Bir.tvar list 
     (* [GenVars l] : generate the set of variables that appear in some expressions in list [l] *)
  | Kill of A3Bir.var
     (* [Kill x] : remove variable [x] *)
      
type transfer = transfer_fun list
type pc = int

let fun_to_string = function
  | GenVars e ->
      Printf.sprintf "GenVars(%s)" (String.concat "::" (List.map A3Bir.print_tvar e))
  | Kill x ->
      Printf.sprintf "Kill(%s)" (A3Bir.var_name_g x)

let transfer_to_string = function
  | [] -> ""
  | [f] -> fun_to_string f
  | f::q -> (fun_to_string f)^(List.fold_right (fun f s -> ";"^(fun_to_string f)^s) q "")
      
let eval_transfer = function
  | GenVars l -> (fun ab -> List.fold_right (fun (_,x) -> Env.add x) l ab)
  | Kill x -> fun ab -> Env.remove x ab

let rec all_expr_in_formula acc = function
  | A3Bir.BoolVar e -> e::acc
  | A3Bir.Atom (_,e1,e2) -> e1::e2::acc
  | A3Bir.And (f1,f2) | A3Bir.Or (f1,f2) -> all_expr_in_formula (all_expr_in_formula acc f1) f2
let all_expr_in_formula = all_expr_in_formula []

(* [gen_instrs last i] computes a list of transfert function
   [(f,j);...] with [j] the successor of [i] for the transfert
   function [f]. [last] is the end label of the method; *)
let gen_instrs last i = 
  function
  | A3Bir.Ifd ((_,e1,e2), j) -> 
      let gen = GenVars [e1; e2] in [([gen],j);([gen],i+1)]
  | A3Bir.Goto j -> [[],j]
  | A3Bir.Throw _
  | A3Bir.Return None  -> []
  | A3Bir.Return (Some e)  ->  [[GenVars [e]],last]
  | A3Bir.AffectVar (x,e) -> [[GenVars (Env.tvars_in_expr e); Kill x],i+1]
  | A3Bir.NewArray (x,_,le)
  | A3Bir.New (x,_,_,le) 
  | A3Bir.InvokeStatic (Some x,_,_,le) ->  [[GenVars le;Kill x],i+1]
  | A3Bir.InvokeVirtual (Some x,e,_,_,le) 
  | A3Bir.InvokeNonVirtual (Some x,e,_,_,le) -> [[GenVars (e::le); Kill x],i+1]
  | A3Bir.MonitorEnter e 
  | A3Bir.MonitorExit e -> [[GenVars [e]],i+1]
  | A3Bir.AffectStaticField (_,_,e) -> [[GenVars [e]],i+1]
  | A3Bir.AffectField (e1,_,_,e2) -> [[GenVars [e1; e2]],i+1]
  | A3Bir.AffectArray (e1,e2,e3) -> [[GenVars [e1; e2; e3]],i+1]
  | A3Bir.InvokeStatic (None,_,_,le) -> [[GenVars le],i+1]
  | A3Bir.InvokeVirtual (None,e,_,_,le) 
  | A3Bir.InvokeNonVirtual (None,e,_,_,le) -> [[GenVars (e::le)],i+1]
  | A3Bir.MayInit _ 
  | A3Bir.Nop -> [[],i+1]
  | A3Bir.Check c -> begin
      match c with
	| A3Bir.CheckArrayBound (e1,e2)
	| A3Bir.CheckArrayStore (e1,e2) -> [[GenVars [e1; e2]],i+1]
	| A3Bir.CheckNullPointer e
	| A3Bir.CheckNegativeArraySize e
	| A3Bir.CheckCast (e,_)
	| A3Bir.CheckArithmetic e -> [[GenVars [e]],i+1]
	| A3Bir.CheckLink _ -> [[],i+1]
    end
  | A3Bir.Formula (_,f) -> [[GenVars (all_expr_in_formula f)],i+1]

(* generate a list of transfer functions *)
let gen_symbolic (m:A3Bir.t) : (pc * transfer * pc) list = 
  let length = Array.length (A3Bir.code m) in
    JUtil.foldi 
      (fun i ins l ->
	 List.rev_append
	   (List.map (fun (c,j) -> (j,c,i)) (gen_instrs length i ins))
	   l) 
      (List.map (fun (i,e) -> (e.A3Bir.e_handler,[],i)) (A3Bir.exception_edges m))
      (A3Bir.code m)

let run m =
  Iter.run 
    {
      Iter.bot = Env.bot ;
      Iter.join = Env.union;
      Iter.leq = Env.subset;
      Iter.eval = List.fold_right eval_transfer;
      Iter.normalize = (fun x -> x);
      Iter.size = 1 + Array.length (A3Bir.code m);
      Iter.workset_strategy = Iter.Decr;
      Iter.cstrs = gen_symbolic m;
      Iter.init_points = [Array.length (A3Bir.code m)];
      Iter.init_value = (fun _ -> Env.empty); (* useless here since we iterate from bottom *)
      Iter.verbose = false;
      Iter.dom_to_string = Env.to_string;
      Iter.transfer_to_string = transfer_to_string
    }


let to_string = Env.to_string
