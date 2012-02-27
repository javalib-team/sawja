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


open Javalib_pack
open JBasics
open Javalib

include Cmn

let dominator instr_array preds =
    let all = 
      JUtil.foldi 
	(fun i _ -> Ptset.add i) (Ptset.singleton (-1)) instr_array 
    in
    let dom = Array.init
      (Array.length instr_array)
      (fun _ -> all) in
    let get_dom i =
      if i < 0 then Ptset.singleton (-1) else dom.(i) in
    let rec inter_list = function
	[] -> assert false
      | [x] -> get_dom x
      | x::q -> Ptset.inter (get_dom x) (inter_list q) in
    let change = ref true in
      while !change do
	change := false;
	Array.iteri
	  (fun i _ -> 
	     let new_s = 
		 Ptset.add i (inter_list (preds i)) 
	     in
	       if not (Ptset.subset dom.(i) new_s) then
		 begin
		   dom.(i) <- new_s;
		   change := true;
		 end)
	  dom	     
      done;
      dom
	
  (* build dominator tree *)
  let make_idom_tree aux =
    let assoc_list = 
      JUtil.foldi (fun i s l -> (Ptset.choose s,i)::l) [] aux in
    let assoc_list = List.sort (fun (i,_) (j,_) -> compare i j) assoc_list in
    let rec children i = function
	[] -> []
      | (j,p)::q ->
	  let c = compare i j in
	    if c>0 then children i q
	    else if c=0 then p::(children i q)
	    else [] in
      (fun i -> children i assoc_list)

  (* immediate dominator *)
  let idom dom =
    let n = Array.length dom in
    let dom_strict = Array.init n (fun i -> Ptset.remove i dom.(i)) in
    let aux = Array.init n (fun i -> dom_strict.(i)) in
      for i=0 to (n-1) do
	let workset = ref (Ptset.remove (-1) dom_strict.(i)) in
	  while not (Ptset.is_empty !workset) do
	    let j = Ptset.max_elt !workset in
	      workset := Ptset.diff !workset dom.(j);
	      aux.(i) <- Ptset.diff aux.(i) dom_strict.(j)
	  done;
      done;
      (fun i -> 
	 let s = aux.(i) in
	   assert (Ptset.cardinal s = 1);
	   Ptset.choose s), make_idom_tree aux
	  
  (* dominance frontier set 
     See: 
     Cooper, Keith D.; Harvey, Timothy J.; and Kennedy, Ken (2001). 
     A Simple, Fast Dominance Algorithm *)
  let domf n preds idom = 
    let domf = Array.make (n+1) Ptset.empty in
      for i=0 to (n-1) do
	let preds = preds i in
	let idom_i = idom i in
	  if List.length preds > 1 then
	    List.iter
	      (fun p -> 
		 let runner = ref p in
		   while !runner <> idom_i do
		     domf.(!runner+1) <- Ptset.add i domf.(!runner+1);
		     runner := idom !runner 
		   done
	      )
	      preds 
      done;
      (fun i -> domf.(i+1))

  let show_digraph instr_array succs =
    let f = open_out "debug.dot" in
      Printf.fprintf f "digraph debug {\n";
      Array.iteri
	(fun i _ ->
	   List.iter 
	     (fun j -> Printf.fprintf f "  n%d -> n%d;\n" i j)
	     (succs i))
	instr_array;
      Printf.fprintf f "}\n";
      close_out f  

  (* see:  
     Cytron, Ron; Ferrante, Jeanne; Rosen, Barry K.; Wegman, Mark N.; 
     and Zadeck, F. Kenneth (1991). 
     "Efficiently computing static single assignment form and the 
     control dependence graph". 
     ACM Transactions on Programming Languages and Systems 13 (4): 451–490.*)
  let place_phi_nodes vars n var_defs domf live =
    let place = ref Ptmap.empty in
    let place_node n v =
      place := Ptmap.add ~merge:Ptset.union n (Ptset.singleton v) !place in
    let iter_count = ref 0 in
    let has_already = Array.make (n+1) 0 in
    let work = Array.make (n+1) 0 in
    let workset = ref Ptset.empty in
      Ptmap.iter
	(fun v defs -> 
	   incr iter_count;
	   Ptset.iter 
	     (fun x -> 
		work.(x+1) <- !iter_count;
		workset := Ptset.add x !workset)
	     defs;
	   while not (Ptset.is_empty !workset) do
	     let x = Ptset.choose !workset in
	       workset := Ptset.remove x !workset;
	       Ptset.iter
		 (fun y -> 
		    if has_already.(y+1) < !iter_count then 
		      begin
			if live y vars.(v) then place_node y v;
			has_already.(y+1) <- !iter_count;
 			if work.(y+1) < !iter_count then 
			  begin
			    workset := Ptset.add y !workset;
			    work.(y+1) <- !iter_count
			  end
		      end)
		 (domf x)
	   done)
	var_defs;
      !place

  let debug_code print_instr params vars code exc_tbl phi_nodes children var_defs search_h succs =
    Printf.printf "params(%s)\n"
      (JUtil.print_list_sep ","
	 (List.map 
	    (fun (_,x) -> var_name_g x) params));
    Array.iteri 
      (fun i op -> 
	 Printf.printf "[%s]%3d: %s\n"
	   (JUtil.print_list_sep " "
	      (List.map 
		 (fun v -> var_name_g (vars.(v))) 
		 (Ptmap.fold (fun v _ l -> v::l) (phi_nodes i) [])))
	   i (print_instr op))
      code;
    List.iter
      (fun e -> Printf.printf " [%d, %d] --> %d\n" e.e_start e.e_end e.e_handler)
      exc_tbl;
    Printf.printf "var_def:\n";
    Ptmap.iter
      (fun v defs ->
	 Printf.printf "   %s: {%s}\n"
	   (var_name_g (vars.(v)))
	   (JUtil.print_list_sep "," (List.map string_of_int (Ptset.elements defs)))
      ) var_defs;
    Printf.printf "search: %s\n" 
      (JUtil.print_list_sep "::" 
	 (List.map 
	    (fun x ->
	       Printf.sprintf "%d(%s)"
		 x 
		 (JUtil.print_list_sep " " (List.map string_of_int (children x)))) search_h));
    show_digraph code succs


  (* Compute the rights indexes for each variable use and def.
     See:  
     Cytron, Ron; Ferrante, Jeanne; Rosen, Barry K.; Wegman, Mark N.; 
     and Zadeck, F. Kenneth (1991). 
     "Efficiently computing static single assignment form and the 
     control dependence graph". 
     ACM Transactions on Programming Languages and Systems 13 (4): 451–490.*)
  let rename def_bcvar use_bcvars print_instr params vars code exc_tbl var_defs children preds succs phi_nodes =
    let c = ref (Ptmap.map (fun _ -> 0) var_defs) in
    let s = ref (Ptmap.map (fun _ -> []) var_defs) in
    let rename_use = Array.make (Array.length code) Ptmap.empty in
    let rename_def = ref Ptmap.empty in
    let rename_def_phi = ref Ptmap.empty in
    let phi_nodes = 
      Ptmap.mapi 
	(fun n s -> 
	   let n_preds = List.length (preds n) in
	     Ptset.fold 
	       (fun v -> Ptmap.add v (Array.make n_preds (-1))) 
	       s 
	       Ptmap.empty) 
      phi_nodes 
    in
    let phi_nodes i =
      try Ptmap.find i phi_nodes with Not_found -> Ptmap.empty in
    let search_h = ref [] in
    let top_s i x = 
      try
	(match Ptmap.find x !s  with
	   | [] -> 
	       Printf.printf "ERROR top(s(%s)) in %d\n" 
		 (var_name_g (vars.(x))) i;
	       debug_code print_instr params vars code exc_tbl phi_nodes children var_defs !search_h succs;
	       assert false
	   | i::_ -> i)
      with Not_found -> 
	Printf.printf "ERROR s(%s) not found at node %d\n" 
	  (var_name_g (vars.(x))) i;
	debug_code print_instr params vars code exc_tbl phi_nodes children var_defs !search_h succs;
	assert false in
    let pop_s x = 
      try
	(match Ptmap.find x !s  with
	   | [] -> assert false
	   | _::q -> s := Ptmap.add x q !s)
      with Not_found -> assert false in
    let rec search x =
      search_h := x :: !search_h;
     (* def : set of variables that are defined in x*)
      let def = if x<0 then
        (* at entry point, the set contains all parameters *)
        (List.fold_right (fun (_,x) -> Ptset.add (index x))
	   params Ptset.empty)
      else def_bcvar code.(x) in
	Ptmap.iter
	  (fun v _ -> 
	     let xmap = 
	       try Ptmap.find x !rename_def_phi
	       with Not_found -> Ptmap.empty
	     in
	     let i = Ptmap.find v !c in
	       rename_def_phi := 
		 Ptmap.add 
		   x (Ptmap.add v i xmap) (* at point x, v |-> v_i *)
		   !rename_def_phi;
	       s := Ptmap.add v (i::(Ptmap.find v !s)) !s;
	       c := Ptmap.add v (i+1) !c)
	  (phi_nodes x);
	if x>=0 then begin
	  let var_defs = use_bcvars code.(x) in
	    rename_use.(x) <-
	      Ptset.fold 
	      (fun v -> Ptmap.add v (top_s x v)) var_defs Ptmap.empty
	end;
	Ptset.iter
	  (fun v ->
	     let xmap = 
	       try Ptmap.find x !rename_def
	       with Not_found -> Ptmap.empty
	     in
	     let i = Ptmap.find v !c in
	       rename_def := 
		 Ptmap.add 
		   x (Ptmap.add v i xmap) 
		   !rename_def;
	       s := Ptmap.add v (i::(Ptmap.find v !s)) !s;
	       c := Ptmap.add v (i+1) !c)
	  def;
	List.iter
	  (fun y -> 
	     let preds = preds y in
	     let index_x = JUtil.find_index x preds in
	     let phi = phi_nodes y in
	       Ptmap.iter (fun v args -> args.(index_x) <- top_s y v) phi)
	  (succs x);
	List.iter search (children x);
	Ptset.iter pop_s def;
	Ptmap.iter
	  (fun v _ -> pop_s v)
	  (phi_nodes x)
    in
      search (-1);
      (fun i -> 
	 let xmap = Ptmap.find i !rename_def in
	   fun v -> Ptmap.find v xmap), 
    (fun i -> 
	 let xmap = Ptmap.find i !rename_def_phi in
	   fun v -> Ptmap.find v xmap),
    (fun i -> (rename_use.(i))),
    phi_nodes


  let run ir_code live =
    (*
      let rd = ReachDef.run ir_code in
      let jump_target = jump_target ir_code in
    *)    
    let n = Array.length ir_code.IR.code in
    let preds = IR2SSA.preds ir_code in
    let succs = IR2SSA.succs ir_code in
    let dom = dominator ir_code.IR.code preds in
    let (idom,children) = idom dom in
    let domf = domf n preds idom in
    let var_defs = IR2SSA.var_defs ir_code in
    let phi_nodes = place_phi_nodes ir_code n var_defs domf live in
    let rename = rename ir_code var_defs children preds succs phi_nodes in
    let phi_nodes i =
      try
	List.map
	  (fun v -> ir_code.IR.vars.(v))
	  (Ptset.elements (Ptmap.find i phi_nodes)) 
      with Not_found -> [] in
      ((fun i -> dom.(i)),idom,domf,phi_nodes,var_defs,rename,preds)

  let to_string s =
    Printf.sprintf "{%s}"
      (JUtil.print_list_sep_map "," string_of_int (Ptset.elements s))

  let vars_to_string s =
    Printf.sprintf "{%s}"
      (JUtil.print_list_sep_map "," IR.var_name_g s)

  let debug ir_code (dom,idom,domf,phi_nodes,var_defs,(_rename_def,_rename_def_phi, rename_use,phi_nodes'),_preds) =
    let jump_target = IR.jump_target ir_code in
    let var_defs = 
      JUtil.foldi
	(fun pc _ def_map -> 
	   let var_list = phi_nodes pc in
	   List.fold_left
	     (fun def_m v -> 
		let v_i = IR.index v in
		  Ptmap.add ~merge:Ptset.union v_i 
		    (Ptset.singleton pc) def_m)
	     def_map
	     var_list
	)
	var_defs
	ir_code.IR.code
    in
      Ptmap.iter
	(fun v defs ->
	   Printf.printf "  %s:" (IR.var_name_g (ir_code.IR.vars.(v)));
	   Ptset.iter (Printf.printf " %d") defs;
	   print_newline ()) var_defs;
      Array.iteri 
	(fun i op -> 
	   Printf.printf "     --> DOM[%d]: %s\n" i
	     (to_string (dom i));
	   Printf.printf "     --> IDOM[%d]: %d\n" i
	     (idom i);
	   Printf.printf "     --> DOMF[%d]: %s\n" i
	     (to_string (domf i));
	   Printf.printf "     --> PHI[%d]: %s\n" i
	     (vars_to_string (phi_nodes i));
	   (*(try Printf.printf "Def: %d\n" (rename_def i)
	    with Not_found -> ());*)
	   let rename_use = rename_use i in
	     Printf.printf "Use:";
	     Ptmap.iter
	       (fun v i -> Printf.printf " %s_[%d]" 
		  (IR.var_name_g ir_code.IR.vars.(v)) i)
	       rename_use;
	     print_newline ();		 
	     let phi_nodes = phi_nodes' i in
	       Ptmap.iter 
		 (fun v args -> 
		    let v = IR.var_name_g ir_code.IR.vars.(v) in
		      Printf.printf "      %s := PHI(%s)\n"
			v (JUtil.print_list_sep "," (List.map (Printf.sprintf "%s_%d" v) (Array.to_list args))))
		 phi_nodes;
	       Printf.printf "%s%3d: %s\n"
		 (if jump_target.(i) then "x" else " ")
		 i (IR.print_instr op))
	ir_code.IR.code;
      print_newline ()

  let transform_from_ir (ir_code:IR.t) =
    let live = IR2SSA.live_analysis ir_code in
    let run = run ir_code live in
    let debug i msg = 
      Printf.printf "-----------------\nFailure %s line %d\n-----------------\n" msg i;
      debug ir_code run in
    let (_,_,_,_,_,
	 (rename_def, rename_def_phi, rename_use,phi_nodes'),preds) = run in
    let dico = Var.make_dictionary () in
    let make_var = Var.make_var dico in
    let def i x = 
      if IR.var_ssa x then make_var x 0
      else 
	try make_var x (rename_def i (IR.index x)) 
	with Not_found -> debug i "def lookup"; assert false in
    let use i = 
      let rename_use = try rename_use i with Not_found -> debug i "use lookup"; assert false in
	function x -> 
	  if IR.var_ssa x then make_var x 0
	  else
	    try make_var x (Ptmap.find (IR.index x) rename_use) 
	    with Not_found -> debug i (Printf.sprintf "use var %s lookup" (IR.var_name_g x)); assert false in
    let phi_nodes i =
      try
	Ptmap.fold
	  (fun v args l -> 
	     let x_ir = ir_code.IR.vars.(v) in
	     let x = make_var x_ir (rename_def_phi i v) in
	     let (vars_u,vars_u_s) =  
	       let vset = ref Var.VarSet.empty in
		 Array.map 
		   (fun vi -> 
		      let var = make_var x_ir vi in
			vset:= Var.VarSet.add var !vset;
			var) 
		   args
		   ,!vset
	     in
	       {
		 TSSA.def = x;
		 TSSA.use = vars_u;
		 TSSA.use_set = vars_u_s;
	       }::l)
	  (phi_nodes' i) []
      with Not_found -> debug i "phi lookup"; assert false in
    let code = Array.mapi
      (fun i -> IR2SSA.map_instr (def i) (use i)) ir_code.IR.code in
    let exc_t = List.map (IR2SSA.map_exception_handler make_var) ir_code.IR.exc_tbl in
    let params = 
      List.map 
	(fun (t,x) -> (t, make_var x 0)) ir_code.IR.params
    in
    let preds = 
      Array.init (Array.length code) (fun i -> Array.of_list (preds i))
    in
    let phi_nodes = 
      Array.init (Array.length code) phi_nodes
    in
    let vars = 
      if Array.length ir_code.IR.vars = 0
      then [||]
      else Var.make_array_var dico ir_code.IR.vars.(0)
    in
      {
	TSSA.vars = vars;
	TSSA.params = params;
	TSSA.code  = code;
	TSSA.preds = preds;
	TSSA.phi_nodes = phi_nodes;
	TSSA.exc_tbl = exc_t;
	TSSA.line_number_table = ir_code.IR.line_number_table;
	TSSA.pc_bc2ir = ir_code.IR.pc_bc2ir;
	TSSA.pc_ir2bc = ir_code.IR.pc_ir2bc
      }



