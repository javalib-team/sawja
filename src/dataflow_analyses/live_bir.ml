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


module Env = struct
  (* lattice of powerset of JBir variables *)
  include Set.Make(struct type t = JBir.var let compare v1 v2 = (JBir.index v1) - (JBir.index v2) end)

  let print_key  = JBir.var_name_g

  let bot = empty

  let rec vars acc = function
    | JBir.Const _ -> acc
    | JBir.Var (_,x) -> add x acc
    | JBir.Field (e,_,_) 
    | JBir.Unop (_,e) -> vars acc e
    | JBir.Binop (_,e1,e2) -> vars (vars acc e1) e2
    | JBir.StaticField _ -> acc

  (* [vars e] computes the set of variables that appear in expression [e]. *)
  let vars = vars empty

  let to_string ab =
    let ab = elements ab in
      match List.map print_key ab with
	| [] -> "{}"
	| [x] -> Printf.sprintf "{%s}" x
	| x::q -> Printf.sprintf "{%s%s}" x (List.fold_right (fun x s -> ","^x^s) q "")	 
end
  
type transfer_fun =
  | GenVars of JBir.expr list 
     (* [GenVars l] : generate the set of variables that appear in some expressions in list [l] *)
  | Kill of JBir.var
     (* [Kill x] : remove variable [x] *)
      
type transfer = transfer_fun list
type pc = int

let fun_to_string = function
  | GenVars e ->
      Printf.sprintf "GenVars(%s)" (String.concat "::" (List.map JBir.print_expr e))
  | Kill x ->
      Printf.sprintf "Kill(%s)" (JBir.var_name_g x)

let transfer_to_string = function
  | [] -> ""
  | [f] -> fun_to_string f
  | f::q -> (fun_to_string f)^(List.fold_right (fun f s -> ";"^(fun_to_string f)^s) q "")
      
let eval_transfer = function
  | GenVars l -> fun ab -> List.fold_right (fun e -> Env.union (Env.vars e)) l ab
  | Kill x -> fun ab -> Env.remove x ab

let rec all_expr_in_formula acc = function
  | JBir.BoolVar e -> e::acc
  | JBir.Atom (_,e1,e2) -> e1::e2::acc
  | JBir.And (f1,f2) | JBir.Or (f1,f2) -> all_expr_in_formula (all_expr_in_formula acc f1) f2
let all_expr_in_formula = all_expr_in_formula []

(* [gen_instrs last i] computes a list of transfert function
   [(f,j);...] with [j] the successor of [i] for the transfert
   function [f]. [last] is the end label of the method; *)
let gen_instrs last i = function
  | JBir.Ifd ((_,e1,e2), j) -> 
      let gen = GenVars [e1;e2] in [([gen],j);([gen],i+1)]
  | JBir.Goto j -> [[],j]
  | JBir.Throw _
  | JBir.Return None  -> []
  | JBir.Return (Some e)  ->  [[GenVars [e]],last]
  | JBir.AffectVar (x,e) -> [[GenVars [e]; Kill x],i+1]
  | JBir.NewArray (x,_,le)
  | JBir.New (x,_,_,le) 
  | JBir.InvokeStatic (Some x,_,_,le) ->  [[GenVars le;Kill x],i+1]
  | JBir.InvokeVirtual (Some x,e,_,_,le) 
  | JBir.InvokeNonVirtual (Some x,e,_,_,le) -> [[GenVars (e::le); Kill x],i+1]
  | JBir.MonitorEnter e 
  | JBir.MonitorExit e -> [[GenVars [e]],i+1]
  | JBir.AffectStaticField (_,_,e) -> [[GenVars [e]],i+1]
  | JBir.AffectField (e1,_,_,e2) -> [[GenVars [e1;e2]],i+1]
  | JBir.AffectArray (e1,e2,e3) -> [[GenVars [e1;e2;e3]],i+1]
  | JBir.InvokeStatic (None,_,_,le) -> [[GenVars le],i+1]
  | JBir.InvokeVirtual (None,e,_,_,le) 
  | JBir.InvokeNonVirtual (None,e,_,_,le) -> [[GenVars (e::le)],i+1]
  | JBir.MayInit _ 
  | JBir.Nop -> [[],i+1]
  | JBir.Check c -> begin
      match c with
	| JBir.CheckArrayBound (e1,e2)
	| JBir.CheckArrayStore (e1,e2) -> [[GenVars [e1;e2]],i+1]
	| JBir.CheckNullPointer e
	| JBir.CheckNegativeArraySize e
	| JBir.CheckCast (e,_)
	| JBir.CheckArithmetic e -> [[GenVars [e]],i+1]
	| JBir.CheckLink _ -> [[],i+1]
    end
  | JBir.Formula (_,f) -> [[GenVars (all_expr_in_formula f)],i+1]

(* generate a list of transfer functions *)
let gen_symbolic (m:JBir.t) : (pc * transfer * pc) list = 
  let length = Array.length (JBir.code m) in
    JUtil.foldi 
      (fun i ins l ->
	 List.rev_append
	   (List.map (fun (c,j) -> (j,c,i)) (gen_instrs length i ins))
	   l) 
      (List.map (fun (i,e) -> (e.JBir.e_handler,[],i)) (JBir.exception_edges m))
      (JBir.code m)

let run m =
  Iter.run 
    {
      Iter.bot = Env.bot ;
      Iter.join = Env.union;
      Iter.leq = Env.subset;
      Iter.eval = List.fold_right eval_transfer;
      Iter.normalize = (fun x -> x);
      Iter.size = 1 + Array.length (JBir.code m);
      Iter.workset_strategy = Iter.Decr;
      Iter.cstrs = gen_symbolic m;
      Iter.init_points = [Array.length (JBir.code m)];
      Iter.init_value = (fun _ -> Env.empty); (* useless here since we iterate from bottom *)
      Iter.verbose = false;
      Iter.dom_to_string = Env.to_string;
      Iter.transfer_to_string = transfer_to_string
    }


let to_string = Env.to_string
