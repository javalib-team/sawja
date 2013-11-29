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


(** manager of the workset iterator.
    ['var] is the type of equation variables.
    ['dom] is the type of abstract elements.
    ['tf] is the type of transfer functions. *)
type ('var,'dom,'tf) manager = {
  string_of_var : 'var -> string;   (** gives to each variable a string representation (for debug) *)
  hash_var : 'var -> int;           (** gives to each variable an unique hash value *)
  bot : 'dom;                       (** bottom element *)
  join : 'dom -> 'dom -> 'dom;      (** binary least upper bound *)
  leq :  'dom -> 'dom -> bool;      (** partial order test *)
  normalize :  'dom -> 'dom;        (** normalize after each join (identity is correct) *)
  eval : 'tf -> 'dom list -> 'dom;  (** evaluates a transfer function *)  
	 size : int; (* intra analysis will be computed for points in [0 .. size-1] *)
  workset_strategy : workset_strategy;
  cstrs : ('var list * 'tf * 'var) list; (** constraints on which iterate*)
  verbose : bool;
  dom_to_string : 'dom -> string;
  transfer_to_string : 'tf -> string
}

let rec f_list f acc = function
    [] -> acc
  | x::q -> f_list f (f acc x) q

let run man =
  let join_list = f_list man.join man.bot in

  let initial_workset = (* the set of all variables *)
    let add s v = Ptset.add (man.hash_var v) s in
    List.fold_left 
      (fun s (args,_,dst) -> List.fold_left add s (dst::args)) 
      Ptset.empty man.cstrs in

  (* we compute the maximum hash value of the variables in the system *)
  let max_elt = Ptset.max_elt initial_workset in
  let size = max_elt +1 in

  (* we build the dependence graph between variables *)
  (* for each variable [v], [succ.(hash_var v)] contains the set of variable that 
     depends on it *)
  (* for each variable [v], [transf.(hash_var v)] contains the set of pair [(tf,args)]
     such that [v] is constrained by the transfert function [tf] applied on the
     variables [args] *)
  let succs = Array.make size Ptset.empty in
  let transfs = Array.make size [] in
  let _ = List.iter
	    (fun (args,tf,dst) -> (* tf(args) <= dst *)
	      let dst_idx = man.hash_var dst in
	      List.iter (fun arg -> 
		let arg_idx = man.hash_var arg in
		succs.(arg_idx) <- Ptset.add (dst_idx) succs.(arg_idx)
	      ) args;
	      transfs.(dst_idx) <- (tf,args)::transfs.(dst_idx))
	    man.cstrs in

  let current = Array.make size man.bot in

  let choose ws = Ptset.min_elt ws in (* TODO : topological sort *)

  let rec loop ws =
    if Ptset.is_empty ws then ()
    else
      let i = choose ws in
      let ws = Ptset.remove i ws in
      let v = join_list 
	(List.map (fun (tf,args) -> 
	  man.eval tf 
	    (List.map (fun arg -> current.(man.hash_var arg)) args))
	   transfs.(i)) in
      if man.verbose then begin
	Printf.printf "choosing %d...\n" i;
	Printf.printf "%d transfert functions:\n" (List.length transfs.(i));
	List.iter (fun (tf,args) -> Printf.printf "  (%s): %s\n" 
	  (Sawja_pack.JUtil.print_list_sep "," man.string_of_var args)
	  (man.transfer_to_string tf)) transfs.(i);
	Printf.printf "old(i): ";
	let s = (man.dom_to_string current.(i)) in
	Printf.printf "%s\n" s;
	Printf.printf "new(i) (before normalization): ";
	let s = (man.dom_to_string v) in
	Printf.printf "%s\n" s;
      end;
      let v = man.normalize v in
      if man.verbose then begin
	Printf.printf "new(i): ";
	let s = (man.dom_to_string v) in
	Printf.printf "%s\n" s;
      end;
      if man.leq v current.(i) then loop ws
      else begin
	if man.verbose then begin
	  Printf.printf "adding";
	  Ptset.iter (Printf.printf " %d") succs.(i);
	  print_newline ()
	end;
	current.(i) <- v;
	loop (Ptset.union ws succs.(i)) 
      end in
    loop initial_workset;
  (fun v -> current.(man.hash_var v))
