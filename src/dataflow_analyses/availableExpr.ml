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



(* we reject exprs that are alias-sensible *)
let rec is_available_expr = function
  | JBir.Field _ 
  | JBir.StaticField _ 
  | JBir.Binop (JBir.ArrayLoad _,_,_) -> false
  | JBir.Binop (_,e1,e2) -> is_available_expr e1 && is_available_expr e2
  | JBir.Unop (_,e) -> is_available_expr e
  | JBir.Const _ 
  | JBir.Var _ -> true

(* vars that appear in an expression (not alias-sensible) *)
let rec vars = function
  | JBir.Field _ 
  | JBir.StaticField _ 
  | JBir.Binop (JBir.ArrayLoad _,_,_) -> assert false
  | JBir.Binop (_,e1,e2) -> Ptset.union (vars e1) (vars e2)
  | JBir.Unop (_,e) -> vars e
  | JBir.Const _ -> Ptset.empty
  | JBir.Var (_,x) -> Ptset.singleton (JBir.index x)

(* tests if [x] appears in [e] (not alias-sensible) *)
let var_in_expr x e =
  Ptset.mem (JBir.index x) (vars e)

module Lat = struct
  (* we compute set of couples [(x,e)] where expression [e] must be in variable [v] at the
     current program point *)
  include Set.Make(struct type t = JBir.var*JBir.expr let compare = compare end)

  (* bottom element *)
  let collect_affect_var bir =
    Array.fold_right
      (fun ins s ->
	 match ins with 
	   | JBir.AffectVar (x,e) -> 
	       if is_available_expr e then add (x,e) s else s
	   | _ -> s)
      (JBir.code bir) empty
    
  let print_key (x,e) = 
    Printf.sprintf "(%s,%s)" (JBir.var_name_g x) (JBir.print_expr e)  

  let to_string ab = 
    Printf.sprintf "{%s}" (JUtil.print_list_sep "::" print_key (elements ab))

  let bottom = empty

  let kill x ab = 
    filter (fun (x0,e0) -> not (JBir.var_equal x x0) && not (var_in_expr x e0)) ab 

  let gen x e ab = 
    if var_in_expr x e then ab else add (x,e) ab

end

type transfer =
  | Assign of JBir.var*JBir.expr
  | Kill of JBir.var
  | Nop

type pc = int

let transfer_to_string = function
  | Assign (x,e) ->
      Printf.sprintf "%s:=%s" (JBir.var_name_g x) (JBir.print_expr e)
  | Kill x ->
      Printf.sprintf "%s:= ?" (JBir.var_name_g x) 
  | Nop -> "Nop"

let eval_transfer = function
  | Assign (x,e) -> (fun ab -> Lat.gen x e (Lat.kill x ab))
  | Kill x -> Lat.kill x
  | Nop -> (fun ab -> ab)

let gen_instrs i = function
  | JBir.Ifd (_, j) -> [(Nop,j);(Nop,i+1)]
  | JBir.Goto j -> [Nop,j]
  | JBir.Throw _
  | JBir.Return _  -> []
  | JBir.AffectVar (x,e) -> 
      let tf = if is_available_expr e then Assign (x,e) else Kill x in
	[tf,i+1]
  | JBir.NewArray (x,_,_)
  | JBir.New (x,_,_,_) 
  | JBir.InvokeVirtual (Some x,_,_,_,_) 
  | JBir.InvokeNonVirtual (Some x,_,_,_,_)
  | JBir.InvokeStatic (Some x,_,_,_) ->  [Kill x,i+1]
  | JBir.MonitorEnter _
  | JBir.MonitorExit _
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

let run m =
  Iter.run 
    {
      Iter.bot = Lat.collect_affect_var m;
      Iter.join = Lat.inter;
      Iter.leq = (fun x y -> Lat.subset y x);
      Iter.eval = eval_transfer;
      Iter.normalize = (fun x -> x);
      Iter.size = Array.length (JBir.code m);
      Iter.workset_strategy = Iter.Incr;
      Iter.cstrs = gen_symbolic m;
      Iter.init_points = [0];
      Iter.init_value = (fun _ -> Lat.empty);
      Iter.verbose = false;
      Iter.dom_to_string = Lat.to_string;
      Iter.transfer_to_string = transfer_to_string
    }


