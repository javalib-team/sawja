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

open JBirSSA

(* we reject exprs that are alias-sensible *)
let rec is_available_expr = function
  | Field _ 
  | StaticField _ 
  | Binop (ArrayLoad _,_,_) -> false
  | Binop (_,e1,e2) -> is_available_expr e1 && is_available_expr e2
  | Unop (_,e) -> is_available_expr e
  | Const _ 
  | Var _ -> true

(* vars that appear in an expression (not alias-sensible) *)
let rec vars = function
  | Field _ 
  | StaticField _ 
  | Binop (ArrayLoad _,_,_) -> Ptset.empty
  | Binop (_,e1,e2) -> Ptset.union (vars e1) (vars e2)
  | Unop (_,e) -> vars e
  | Const _ -> Ptset.empty
  | Var (_,x) -> Ptset.singleton (JBirSSA.index x)

(* tests if [x] appears in [e] (not alias-sensible) *)
let var_in_expr x e =
  Ptset.mem (JBirSSA.index x) (vars e)

module Lat = struct
  (* we compute set of expression where expression [e] must be
     affected in the current variable *)
  include Set.Make(struct type t = expr let compare = compare end)

  (* bottom element *)
  let collect_affect_var bir =
    Array.fold_right
      (fun ins s ->
	 match ins with 
	   | AffectVar (_x,e) -> 
	       if is_available_expr e then add e s else s
	   | _ -> s)
      (code bir) empty
    
  let print_key e = 
    Printf.sprintf "(%s)" (print_expr e)  

  let to_string ab = 
    Printf.sprintf "{%s}" (JUtil.print_list_sep "::" print_key (elements ab))

  let bottom = empty

  let gen e ab = 
    add e ab

end

(* allow to keep value (0) for entrypoint *)
let index = fun v -> (index v) + 1

type transfer =
  | Assign of expr
  | Nop

type pc = int

let transfer_to_string = function
  | Assign e ->
      Printf.sprintf "%s" (print_expr e)
  | Nop -> "Nop"

(* Note: we do not need to kill available expression since one
   variable is assigned only one time !*)
let eval_transfer = function
  | Assign v -> (fun ab -> Lat.gen v ab)
  | Nop -> (fun ab -> ab)

(* generate transfer function for an instruction *)
let gen_instrs csts = function
  | AffectVar (x,e) -> 
      let tf = if is_available_expr e then Assign e else Nop in
	(* link affected variable to entrypoint (0) *)
	(0,tf,index x)::csts
  | NewArray (x,_,_)
  | New (x,_,_,_) 
  | InvokeVirtual (Some x,_,_,_,_) 
  | InvokeNonVirtual (Some x,_,_,_,_)
  | InvokeStatic (Some x,_,_,_) ->  (0,Nop,index x)::csts
  | _  -> csts

(* generate transfer functions for phi nodes*)
let gen_phi_nodes csts phis = 
  List.fold_left
    (fun li phi -> 
       (let i0 = index phi.def in
	 VarSet.fold
	   (fun v l -> 
	      (index v,Nop,i0)::l)
	   (VarSet.of_array phi.use)
	   li))
    csts
    phis

(* generate a list of transfer functions *)
let gen_symbolic (m:t) : (pc * transfer * pc) list = 
  let init_csts = 
    (* index of catch vars ... *)
    let index_catch_var = 
      List.fold_left
	(fun s e -> Ptset.add (index (e.e_catch_var)) s)
	Ptset.empty
	(exc_tbl m)
    in
      (* link variables never affected in SSA to entrypoint *)
    List.fold_left
      (fun l (_,v) -> 
	 (0,Nop,index v)::l)
      (List.map (fun i -> (0,Nop,i)) (Ptset.elements index_catch_var))
      (params m)
  in
    (* reverse list to be in same order than instructions *)
    List.rev
      (JUtil.foldi 
	 (fun i ins l ->
	    gen_phi_nodes (gen_instrs l ins) (phi_nodes m).(i)
	 )
	 init_csts
	 (code m))

      

let run m =
  let res = 
    Iter.run 
      {
        Iter.bot = Lat.collect_affect_var m;
        Iter.join = Lat.inter;
        Iter.leq = (fun x y -> Lat.subset y x);
        Iter.eval = eval_transfer;
        Iter.normalize = (fun x -> x);
        Iter.size = (let (_, max_idx) = JBirSSA.ssa_index m in
          max_idx  +2);
	Iter.workset_strategy = Iter.Incr;
	Iter.cstrs = gen_symbolic m;
	Iter.init_points = [0];
	Iter.init_value = (fun _ -> Lat.empty);
	Iter.verbose = false;
	Iter.dom_to_string = Lat.to_string;
	Iter.transfer_to_string = transfer_to_string
      }
  in
    (* retrieve real index of variables *)
    fun i -> res (i+1)


let to_string = Lat.to_string
