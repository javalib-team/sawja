(*
 * This file is part of SAWJA
 * Copyright (c)2013 David Pichardie (ENS Rennes)
 * Copyright (c)2016 David Pichardie (ENS Rennes)
 * Copyright (c)2016 Laurent Guillo (CNRS)
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
  bot : 'dom;                       (** bottom element *)
  join : 'dom -> 'dom -> 'dom;      (** binary least upper bound *)
  leq :  'dom -> 'dom -> bool;      (** partial order test *)
  normalize :  'dom -> 'dom;        (** normalize after each join (identity is correct) *)
  eval : 'tf -> 'dom list -> 'dom;  (** evaluates a transfer function *)  
  is_id : 'tf -> bool;
  is_strict : 'tf -> bool;          (** for each transfer function, tells if it 
					is strict (i.e. f(bot,...,bot)=bot). [false] is always a safe answer *)
  cstrs : 'tf list;                 (** constraints on which iterate*)
  target : 'tf -> 'var;             (** the variable that is targeted by a constraint *)
  args : 'tf -> 'var list;          (** the variables that are used by a constraint *)
  verbose : bool;
  dom_to_string : 'dom -> string;
  transfer_to_string : 'tf -> string;
  transfer_to_dot_string : 'tf -> string;
  update_transfer : 'tf -> 'var -> 'var -> 'tf
}


module PriorityQueue = struct 

  module S = Map.Make(
    struct 
      type t = int
      let compare = compare
    end)

  type 'a t = { rank : 'a -> int; mutable set: 'a S.t }

  (* build an initial empty queue for the given ranking function *)
  let make rank = { rank = rank; set = S.empty }

  let push (elt:'a) (q:'a t) : unit =
    q.set <- S.add (q.rank elt) elt q.set

  let pop (q:'a t) : 'a =
    let (min_key,min_elt) = S.min_binding q.set in
    q.set <- S.remove min_key q.set;
    min_elt

  let is_empty (q:'a t) : bool = S.is_empty q.set

end

module TopologicalSort = struct (* TODO : creer une librairie pour les graphes ?? *)

  (* [run size iter_adj] return the rank of every node of a graph, in a 
     weak topological order. The graph is given by 
     - its set of nodes [0,...,size -1]
     - its adjency iterator [iter_adj] such that
        [iter s f] iters function [f] on all successors of the node [s] 
  *)
  let run size iter_adj : int array = 
    let n = size in
    let q = Stack.create () in
    let date = ref 1 in (* clock *)
    let marked = Array.make n false in
    let seen = Array.make n false in
    let rank = Array.make n (-1) in
    let visit s =
      marked.(s) <- true;
      Stack.push s q;
      while not (Stack.is_empty q) do
	let i = Stack.pop q in
	if seen.(i) then 
	  begin
	    rank.(i) <- n - !date;
	    incr date
	  end
	else
	  begin
	    seen.(i) <- true;
	    Stack.push i q;
	    iter_adj i
	      (fun j -> if not marked.(j) then begin
		marked.(j) <- true;
		Stack.push j q
              end)
	  end
      done in
    for i=0 to n-1 do
      if not marked.(i) then visit i
    done;
    rank

  let _test () =
    let edges = [12,11; 11,10; 10,9; 9,8; 8,7; 7,6; 6,5; 5,1; 6,4; 4,1; 14,15; 13,14; 4,10] in
    let iter_adj l i f = List.iter (fun (j1,j2) -> if i=j1 then f j2) l in
    let n = 16 in
    let rank = run n (iter_adj edges) in
    List.iter
      (fun (i,j) -> Printf.printf "edge s%d (rank=%d) --> s%d (rank=%d) %s\n" i rank.(i) j rank.(j)
	(if rank.(i) > rank.(j) then "(*)" else ""))
      edges
    
end

module CycleDetection = struct

  (* Tarjan's strongly connected components algorithm *)
  let run  size iter_adj : int array =
    let max_scc_length = ref 0 in
    let date = ref 0 in
    let debut = Array.make size 0 in
    let scc = Array.make size (-1) in
    let stack = Stack.create () in
    let rec visite i = 
      incr date;
      debut.(i) <- !date;
      let min = ref !date in
      Stack.push i stack;
      iter_adj i
	(fun j -> 
	   let m = if debut.(j)=0 then visite j else debut.(j) in
	   if m < !min && scc.(j) = -1 then
		 min := m);
      if !min=debut.(i) then
	begin
	  let k = ref (Stack.pop stack) in
	  let scc_length = ref 0 in
	  while (!k<>i) do
	    scc.(!k) <- i;
	    k := Stack.pop stack;
	    scc_length := !scc_length + 1;
	  done;
	  scc.(i) <- i;
(*	  if !scc_length > 0 then
	    Printf.printf "%d:scc_length= %d\n" i !scc_length;*)
	  if !scc_length > ! max_scc_length then max_scc_length := !scc_length;
	end;
      !min
    in
    Array.iteri
      (fun i d -> if d=0 then ignore (visite i)) debut;
    scc

  let _test () =
    let f = open_out "test.dot" in
    let edges = [12,11; 11,10; 10,9; 9,8; 8,7; 7,6; 6,5; 5,1; 6,4; 4,1; 14,15; 13,14; 4,10; 15, 12; 11, 13; 1,5 ] in
    let iter_adj l i f = List.iter (fun (j1,j2) -> if i=j1 then f j2) l in
    let n = 16 in
    Printf.fprintf f "digraph test {\n";
    List.iter
      (fun (i,j) -> Printf.fprintf f " s%d -> s%d;\n" i j)
      edges;
    Printf.fprintf f "}\n";
    close_out f;
    let scc = run n (iter_adj edges) in    
    scc


  end
						      
(* We will use a hash table to give an unique number to each constraint
   variable *)
module HashCons= struct

  type 'a t = {
    table : ('a,int * 'a) Hashtbl.t;
    table_aux : (int, 'a) Hashtbl.t;
    mutable next : int
  }

  let create () = {
    table = Hashtbl.create 251;
    table_aux = Hashtbl.create 251;
    next = -1
  }

  let make (htab: 'a t) x =
    try Hashtbl.find htab.table x
    with Not_found ->
      htab.next <- htab.next +1;
      let x' = (htab.next,x) in
      Hashtbl.add htab.table x x';
      Hashtbl.add htab.table_aux  htab.next x;
      x'

  let make_unsafe htab x =
    Hashtbl.find htab.table x
      
  (* current number of hashed objects *)
  let size htab = htab.next +1

  let index (i,_) = i

  let var_of_hash (htab: 'a t) (hash:int): 'var =
    Hashtbl.find htab.table_aux hash

  (*  let hashtbl2assoc_list htab = Hashtbl.fold (fun _ value l-> value :: l) htab.table []*)
				     
		      
end

exception UndefinedVar of string

let rec f_list f acc = function
    [] -> acc
  | x::q -> f_list f (f acc x) q

let run (man:('var,'dom,'tf) manager) =
  let join_list l =
    match l with
      [] -> man.bot
    | [x] -> x
    | _ -> f_list man.join man.bot l in


  (* We transform the list of constraints (of type ['tf list])
     into a list of elements [(args, cstr, target): ('var list) * ('tf list) * 'var] *)
  let htab = HashCons.create () in
  let i = ref 0 in
  let cstrs' = List.rev_map 
		 (fun tf -> incr i;
			    (*Printf.printf "%d" !i; print_newline();*)
			    (List.map (HashCons.make htab) (man.args tf),tf,HashCons.make htab (man.target tf)))
    man.cstrs in

(*  let rec var2hash_lst l =
    match l with
    | [] -> []
    | hd::tl ->
       incr i;
       Printf.printf "%d" !i; print_newline();
       (List.map (HashCons.make htab) (man.args hd), hd, HashCons.make htab (man.target hd))
		  :: var2hash_lst tl
  in
  let cstrs' = var2hash_lst man.cstrs in*)

  
(*  let cstrs' = ref [] in
  List.iter
    (fun tf ->
       incr i;
       Printf.printf "%d" !i; print_newline();
     cstrs' := (List.map (HashCons.make htab) (man.args tf),tf,HashCons.make htab (man.target tf))::!cstrs')
    man.cstrs;*)
  
  (*  Printf.printf "after   let cstrs' = List.map \n";print_newline();*)


  let hash_var = HashCons.index in
  let string_of_var (_,x) = man.string_of_var x in

  
  let size = HashCons.size htab in

  (* we build the dependence graph between variables *)
  (* for each variable [v], [succ.(hash_var v)] contains the list of variable that 
     depends on it *)
  (* for each variable [v], [transf.(hash_var v)] contains the set of pair [(tf,args)]
     such that [v] is constrained by the transfert function [tf] applied on the
     variables [args] *)
  let succs = Array.make size [] in
  let id_succs = Array.make size [] in (* restriction on id edges *)
  let transfs = Array.make size [] in
  let init_targets = ref [] in (* the list of vars that
				  are the target of a non strict transfert function *)
  let _ = List.iter
	    (fun (args,tf,dst) -> (* tf(args) <= dst *)
	      let dst_idx = hash_var dst in
	      List.iter
		(fun arg -> 
		 let arg_idx = hash_var arg in
		 succs.(arg_idx) <- dst :: succs.(arg_idx);
		 if man.is_id tf then
		   id_succs.(arg_idx) <- dst :: id_succs.(arg_idx)
		)
		args;
	      transfs.(dst_idx) <- (tf,args)::transfs.(dst_idx);
	      if not (man.is_strict tf) then init_targets := dst :: !init_targets)
	    cstrs' in

  let nb_arity1 = ref 0 in
  Array.iter
    (fun tfl -> match tfl with
		| [(tf,[_])] when man.is_id tf -> incr nb_arity1
		| _ -> ())
    transfs;
  Printf.printf "# of symbolic variable that could be merged: %d / %d\n" !nb_arity1 size;

  (* we build a scc of the dependency graph, just considering identity edges *)
  let id_scc = 
    let iter_adj i f = List.iter (fun v -> f (hash_var v)) id_succs.(i) in
    CycleDetection.run size iter_adj in


(*  Array.iteri
    (fun i repr -> Printf.printf "iteri (%d, %d)\n" i repr)
    id_scc;*)
(*  let l =   HashCons.hashtbl2assoc_list htab in
  Printf.printf "length l = %d\n" (List.length l);
  List.iter
    (fun (i, v) -> Printf.printf "((%d, %s)\n"   i (man.string_of_var v))
    
    l;*)
  
  
(*  let nb_var_in_cycle = ref 0 in
  Array.iteri
    (fun i repr -> if i<>repr then
		     begin
		       Printf.printf "%d:%d\n" repr i;
		       incr nb_var_in_cycle;
		     end
    )
    id_scc;
  Printf.printf "# of symbolic variable that could be merged by cycle detection: %d / %d\n" !nb_var_in_cycle size;*)

(*  let display_scc_from_head head =
    (* display variables belonging to the scc whose the index of the head is given as parameter *)
    let head_var = List.assoc head l in
    Printf.printf "head %d:%s\n" head (man.string_of_var head_var);
    Array.iteri
      (fun i e -> if e = head
		  then
		    let i_var = List.assoc i l in
		    Printf.printf "\t %d:%s\n" head (man.string_of_var i_var)
      )
      id_scc in
  display_scc_from_head 0;*)
		  

  
  (*  Printf.printf "just before   let topo_rank = \n";*)
  (* we build a topological sort of the dependency graph *)
  let topo_rank = 
    let iter_adj i f = List.iter (fun v -> f (hash_var v)) succs.(i) in
    TopologicalSort.run size iter_adj in
  (* the workset will be managed by a priority queue, based on
     topological ranks *)
  let workset : (int * 'var) PriorityQueue.t = PriorityQueue.make (fun v -> topo_rank.(hash_var v)) in

  let _ = (* build the initial workset *)
    List.iter (fun v -> PriorityQueue.push v workset) !init_targets in


  



  let current = Array.make size man.bot in

  let rec loop () =
    if PriorityQueue.is_empty workset then ()
    else
      (* we choose the variable with the minimal topological rank
	 in the current workset *)
      let i = PriorityQueue.pop workset in 
      let i_idx = hash_var i in

      if man.verbose then begin
	Printf.printf "> choosing variable %s...\n" (string_of_var i);
	Printf.printf " > %d transfert functions:\n" (List.length transfs.(i_idx));
	List.iter (fun (tf,args) -> Printf.printf "  (%s): %s\n" 
	  (JUtil.print_list_sep "," string_of_var args)
	  (man.transfer_to_string tf)) transfs.(i_idx);
	Printf.printf " > old(%s): " (string_of_var i);
	let s = (man.dom_to_string current.(i_idx)) in
	Printf.printf "%s\n" s;
      end;

      let v = join_list 
	(List.map 
	   (fun (tf,args) -> 
	     let args_val = List.map (fun arg -> current.(hash_var arg)) args in
	     man.eval tf args_val) 
	   transfs.(i_idx)) in

      if man.verbose then begin
	Printf.printf " > new(%s) (before normalization): " (string_of_var i);
	let s = (man.dom_to_string v) in
	Printf.printf "%s\n" s;
      end;

      let v = man.normalize v in

      if man.verbose then begin
	Printf.printf " > new(i): ";
	let s = (man.dom_to_string v) in
	Printf.printf "%s\n" s;
      end;

      if man.leq v current.(i_idx) then loop ()
      else begin
	if man.verbose then begin
	  Printf.printf " > adding %d variables" (List.length succs.(i_idx));
	  if succs.(i_idx)<>[] then 
	    Printf.printf "(%s)" 
	      (JUtil.print_list_sep "," string_of_var succs.(i_idx));
	  Printf.printf " to the workset\n"
	end;
	current.(i_idx) <- v;
	(*	current.(i_idx) <- man.join v current.(i_idx); *)

	List.iter 
	  (fun v -> PriorityQueue.push v workset)
	  succs.(i_idx);
	loop ()
      end in
    loop ();
  (fun v -> 
    try 
      let v' = HashCons.make_unsafe htab v in
      current.(hash_var v')
    with Not_found ->
      begin
	raise (UndefinedVar (man.string_of_var v))
      end

  )

(** manager of the workset iterator - inplace version.
    ['var] is the type of equation variables.
    ['dom] is the type of abstract elements.
    ['tf] is the type of transfer functions. *)
type ('var,'dom,'tf) mutable_manager = {
  m_string_of_var : 'var -> string;   (** gives to each variable a string representation (for debug) *)
  m_bot : unit -> 'dom;               (** bottom element *)
  m_is_top : 'dom -> bool;            (** if [m_is_top val = true] then [val=TOP] *)
  m_eval_and_join :
   'tf -> 'dom list -> 'dom -> bool;  (** evaluates a transfer function and join the result
  				          with the last argument (in-place modification).
				          returns [false] is the update does not change the previous value *)  
  m_eval :
   'tf -> 'dom list -> 'dom -> bool;  (** evaluates a transfer function and put the result
  				          in the last argument (in-place modification).
				          returns [false] is the update does not change the previous value *)  
  m_is_id : 'tf -> bool;
  m_is_strict : 'tf -> bool;          (** for each transfer function, tells if it 
					is strict (i.e. f(bot,...,bot)=bot). [false] is always a safe answer *)
  m_cstrs : 'tf list;                 (** constraints on which iterate*)
  m_target : 'tf -> 'var;             (** the variable that is targeted by a constraint *)
  m_args : 'tf -> 'var list;          (** the variables that are used by a constraint *)


  m_verbose : bool;
  m_dom_to_string : 'dom -> string;
  m_transfer_to_string : 'tf -> string;
  m_transfer_to_dot_string : 'tf -> string;
  m_update_transfer : 'tf -> 'var -> 'var -> 'tf

}



  

  
					 
let m_run (man:('var,'dom,'tf) mutable_manager) =

  Printf.printf "begin of m_run\n";
  (* We transform the list of constraints (of type ['tf list])
     into a list of elements [(args, cstr, target): ('var list) * ('tf list) * 'var] *)
  let init_htab = HashCons.create () in

  let cstrs' = List.rev_map 
		 (fun tf ->
		  (List.map (HashCons.make init_htab) (man.m_args tf),
			     tf,
			     HashCons.make init_htab (man.m_target tf)))
		 man.m_cstrs in
  let cstrs' = Array.of_list cstrs' in

  let hash_var = HashCons.index in

  let init_size = HashCons.size init_htab in
  Printf.printf "# of var before reduction : %d\n" init_size;
  (*  let size_before_reduction = size_0 in*)

  for i = 0 to init_size-1 do 
     let var  = HashCons.var_of_hash init_htab i in
     Printf.printf "[DBG]: var.(%d) %s\n"
		   i
		   (man.m_string_of_var var);
  done;

  
  
  
  (* we build the dependence graph between variables *)
  let arity = Array.make init_size 0 in
  let cstr = Array.make init_size [] in


  (*  let used_as_target = Array.make size 0 in*)
  
  let init_targets = ref [] in (* the list of non strict transfert function *)
  let _ = Array.iteri
	    (fun i (args,tf,dst) -> (* tf(args) <= dst *)
	     let dst_idx = hash_var dst in
	     arity.(dst_idx) <- 1 + arity.(dst_idx);

	     List.iter (fun arg -> 
			let arg_idx = hash_var arg in
			cstr.(arg_idx) <- i :: cstr.(arg_idx)
		       ) args;
	     if not (man.m_is_strict tf) then init_targets := i :: !init_targets)
	    cstrs' in


  (* begin of specific code to remove constraints whose arguments and tager have the same parent *)  

 let parent: int array = Array.init init_size (fun i -> i) in
  let rang: int array = Array.make init_size (0) in
  let rec find (x:int) :int =
    let p = parent.(x) in
    if p != x
    then
      parent.(x) <- find p;
    parent.(x)	in
  let union x y =
    let xRoot = find x in
    let yRoot = find y in
    if  xRoot != yRoot
    then
      if rang.(xRoot) < rang.(yRoot)
      then
	parent.(xRoot) <- yRoot
      else
	if rang.(xRoot) > rang.(yRoot)
	then
	  parent.(yRoot) <- xRoot
	else
	  begin
	    parent.(yRoot) <- xRoot;
	    rang.(xRoot) <- rang.(xRoot) + 1;
	  end in
  Array.iter
    (fun (args, tf, target) ->
     if man.m_is_id tf
     then
       let target_idx = hash_var target in
       let arg_idx = hash_var (List.nth args 0) in (* warning : is it always a 1-element list? *)
       if arity.(target_idx) = 1
       then
	 union arg_idx target_idx
  
    )
    cstrs';

  (* for each variable displays its parent *)
  let nb_var_with_a_parent = ref 0 in
  Array.iteri
    (fun var_idx parent ->
     if parent != -1
     then
       begin
	 if var_idx != parent then incr nb_var_with_a_parent;
	 let arg = HashCons.var_of_hash init_htab var_idx in
	 let parent_var = HashCons.var_of_hash init_htab parent in
	 Printf.printf "%s a pour parent %s\n"
		       (man.m_string_of_var arg)
		       (man.m_string_of_var parent_var)
       end
    )
    parent;
  Printf.printf "nb_var_with_a_parent = %d\n" !nb_var_with_a_parent;


  (* create a new array of constraints *)
  let new_cstr_lst = ref [] in
  Array.iter
    (fun (args, tf, target) ->
    
     if not (man.m_is_id tf)
     then
       (* the current tf is not id, so keep the current constraint as it is *)
       new_cstr_lst := tf :: !new_cstr_lst
     else
       (* the current tf is id *)
       let target_idx = hash_var target in
       let arg_idx = hash_var (List.nth args 0) in (* warning : is it always a 1-element list? *)
       let parent_arg = parent.(arg_idx) in
       let parent_target = parent.(target_idx) in
       if parent_arg != parent_target
       then
	 if parent_arg != arg_idx
	 then
	   (* update the tf  with the parent as argument *)
	   let old_var = HashCons.var_of_hash init_htab arg_idx in
	   let new_var = HashCons.var_of_hash init_htab parent_arg in
	   let tf' = man.m_update_transfer tf old_var new_var in
	   new_cstr_lst :=  tf' :: !new_cstr_lst;
	   (*	   Printf.printf "update tf %s with %s\n" (man.m_transfer_to_string tf) (man.m_transfer_to_string tf');*)
   	 else
	   new_cstr_lst := tf :: !new_cstr_lst
       else
	 (* the argument and the target ahve the same parent *)
	 (* keep the constraint if the target is the parent *)
	 if parent_target = target_idx || parent_arg == arg_idx then
	   new_cstr_lst := tf :: !new_cstr_lst
    
    )
    cstrs';


  let outchan  = open_out ("iter2.dot") in
  output_string outchan "digraph {\n";
  List.iter
    (fun tf ->
     
     let str = man.m_transfer_to_dot_string tf in
	 output_string outchan str
    )
    !new_cstr_lst;
  output_string outchan "\n}\n";
  close_out outchan;

  (* duplicate (...)  the code to generate the new array of constraints *)

  let htab = HashCons.create () in

  let cstrs' = List.rev_map 
		 (fun tf ->
		  (List.map (HashCons.make htab) (man.m_args tf),tf,HashCons.make htab (man.m_target tf)))
		 !new_cstr_lst in
  let cstrs' = Array.of_list cstrs' in

  let hash_var = HashCons.index in

  let size = HashCons.size htab in
  Printf.printf "# of var once reduced : %d, # of elimnated var : %d\n" size (init_size-size);

  for i = 0 to size-1 do 
     let var  = HashCons.var_of_hash htab i in
     Printf.printf "[DBG]: var.(%d) %s\n"
		   i
		   (man.m_string_of_var var);
  done;
  

  (* we build the dependence graph between variables *)
  let nb_of_cstr = Array.length cstrs' in
  let arity = Array.make size 0 in
  let cstr = Array.make size [] in

  (*  let used_as_target = Array.make size 0 in*)
  
  let init_targets = ref [] in (* the list of non strict transfert function *)
  let _ = Array.iteri
	    (fun i (args,tf,dst) -> (* tf(args) <= dst *)
	     let dst_idx = hash_var dst in
	     arity.(dst_idx) <- 1 + arity.(dst_idx);

	     List.iter (fun arg -> 
			let arg_idx = hash_var arg in
			cstr.(arg_idx) <- i :: cstr.(arg_idx)
		       ) args;
	     if not (man.m_is_strict tf) then init_targets := i :: !init_targets)
	    cstrs' in

  (* end of duplication *)


(*  let size = init_size in
  let htab = init_htab in*)
  
  let nb_of_cstr = Array.length cstrs' in
   
   let succs = Array.map (fun (_,_,dst) -> cstr.(hash_var dst)) cstrs' in
   (* we build a topological sort of the dependency graph *)
   let topo_rank = 
     let iter_adj i f = List.iter f succs.(i) in
     TopologicalSort.run nb_of_cstr iter_adj in
  (* the workset will be managed by a priority queue, based on
     topological ranks *)
   let workset : int PriorityQueue.t =
     PriorityQueue.make (fun cstr -> topo_rank.(cstr)) in



  let _ = (* build the initial workset *)
    List.iter (fun cstr -> PriorityQueue.push cstr workset) !init_targets in

  let current = Array.init size (fun _ -> man.m_bot ()) in
  let nb_loop = ref 0 in
  let rec loop () =
    incr nb_loop;

    if PriorityQueue.is_empty workset then ()
    else
      (* we choose the constraint with the minimal topological rank
	 in the current workset *)
      let cstr = PriorityQueue.pop workset in 
      let (args,tf,dst) = cstrs'.(cstr) in
      let dst_idx = hash_var dst in
      let v_dst = current.(dst_idx) in
      let v_args = List.map (fun arg -> current.(hash_var arg)) args in
      let changed =
	if arity.(dst_idx) > 1
	then man.m_eval_and_join tf v_args v_dst
	else man.m_eval tf v_args v_dst in

   
      let cstr_target_is_top cstr =
        let (_,_,target) = cstrs'.(cstr) in
        let val_target = current.(hash_var target) in
        man.m_is_top val_target in
      
      if not changed then loop ()
      else begin
	  List.iter 
	    (fun cstr' ->
              if not (cstr_target_is_top cstr')
              then PriorityQueue.push cstr' workset)
	    succs.(cstr);
	  loop ()
	end
  in
  loop ();
  (* lg : debug *)
(*  Printf.printf "%d" !nb_loop; print_newline();
  Array.iteri
    (fun i pt ->
     let var  = HashCons.var_of_hash htab i in
     Printf.printf "[DBG]: var.(%d)%s:%s\n"
		   i
		   (man.m_string_of_var var)
		   (man.m_dom_to_string pt);
    )
    current;*)
    

  
  (* lg : end of debug *)

  
(*  (fun v -> 
       try 
	 let v' = HashCons.make_unsafe htab v in
	 Printf.printf "hash_var v' = %d\n";
	 current.(hash_var v')
       with Not_found ->
	 begin
	   raise (UndefinedVar (man.m_string_of_var v))
	 end
  )*)
     (fun v -> 
      try
	Printf.printf "before 	 let v' = HashCons.make_unsafe init_htab v in\n";
	 let v' = HashCons.make_unsafe init_htab v in
	 let idx = (hash_var v') in
	 let p = parent.(idx) in
	 Printf.printf "before 	 	 let p_v = HashCons.var_of_hash init_htab p in\n";
	  Printf.printf "idx:%d, p:%d, \n" idx p ;
	 let p_v = HashCons.var_of_hash init_htab p in
	 let (idx', _)= HashCons.make_unsafe htab p_v  in
	 (*	 Printf.printf "idx:%d, p:%d, idx':%d\n" idx p idx';*)
	 current.(idx')
       with Not_found ->
	 begin
	   raise (UndefinedVar (man.m_string_of_var v))
	 end
      )



let mutable_manager_of_manager man = {
  m_string_of_var = man.string_of_var;
  m_bot = (fun ()-> ref man.bot);
  m_eval_and_join = (fun tf args dst ->
    let v = man.join !dst (man.eval tf (List.map (fun p -> !p) args)) in
    let changed = not (man.leq v !dst) in
    if changed then dst := v;
    changed);
  m_eval = (fun tf args dst ->
    let v = man.eval tf (List.map (fun p -> !p) args) in
    let changed = not (man.leq v !dst) in
    if changed then dst := v;
    changed);
  m_is_id = man.is_id;
  m_is_top = (fun _ -> false); (* this always safe to implement it like this *)
  m_is_strict = man.is_strict;
  m_cstrs = man.cstrs;
  m_target = man.target;
  m_args = man.args;
  m_verbose = man.verbose;
  m_dom_to_string = (fun v -> man.dom_to_string !v);
  m_transfer_to_string = man.transfer_to_string;
  m_transfer_to_dot_string =  man.transfer_to_dot_string;
  m_update_transfer = man.update_transfer
}
