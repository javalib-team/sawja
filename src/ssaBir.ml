open Javalib_pack
open JBasics
open Javalib


(* TODO:

 - index variables ?
 - what do we do with ssa_index of variables already in ssa form

*)

module type IRSig = sig
  (** Abstract data type for variables *)
  type var

  (** [var_equal v1 v2] is equivalent to [v1 = v2], but is faster.  *)
  val var_equal : var -> var -> bool

  (** [var_orig v] is [true] if and only if the variable [v] was already used at
      bytecode level. *)
  val var_orig : var -> bool

  (** Used only for internal transformations. *)
  val var_ssa : var -> bool

  (** [var_name v] returns a string representation of the variable [v]. *)
  val var_name : var -> string

  (** [var_name_debug v] returns, if possible, the original variable name of [v], 
      if the initial class was compiled using debug information. *)
  val var_name_debug : var -> string option

  (** [var_name_g v] returns a string representation of the variable [v]. 
      If the initial class was compiled using debug information, original 
      variable names are build on this information. It is equivalent to
      [var_name_g x = match var_name_debug with Some s -> s | _ -> var_name x] *)
  val var_name_g : var -> string

  (** [bc_num v] returns the local var number if the variable comes from the initial bytecode program. *)
  val bc_num : var -> int option

  (** [index v] returns the hash value of the given variable. *)
  val index : var -> int

  type instr

  val print_instr : ?show_type:bool -> instr -> string

  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : JBasics.class_name option;
    e_catch_var : var
  }

  (** [t] is the parameter type for JBir methods. *)
  type t = {
    vars : var array;  
    (** All variables that appear in the method. [vars.(i)] is the variable of
	index [i]. *)
    params : (JBasics.value_type * var) list;
    (** [params] contains the method parameters (including the receiver this for
	virtual methods). *)
    code : instr array;
    (** Array of instructions the immediate successor of [pc] is [pc+1].  Jumps
	are absolute. *)
    exc_tbl : exception_handler list;
    (** [exc_tbl] is the exception table of the method code. Jumps are
	absolute. *)
    line_number_table : (int * int) list option;
    (** [line_number_table] contains debug information. It is a list of pairs
	[(i,j)] where [i] indicates the index into the bytecode array at which the
	code for a new line [j] in the original source file begins.  *)
    pc_bc2ir : int Ptmap.t;
    (** map from bytecode code line to ir code line (very sparse). *)
    pc_ir2bc : int array; 
    (** map from ir code line to bytecode code line *)
  }

  (** [jump_target m] indicates whether program points are join points or not in [m]. *)
  val jump_target : t -> bool array

  (** [instr_succs pp instr] returns the normal successors of instruction [instr] at ir code line [pp].*)
  val instr_succs: int -> instr -> int list

  (** [exception_edges m] returns a list of edges [(i,e);...] where
      [i] is an instruction index in [m] and [e] is a handler whose
      range contains [i]. *)
  val exception_edges :  t -> (int * exception_handler) list

  module InstrRep : functor(Var : Cmn.VarSig) -> Bir.InstrSig

end

module Var (IR:IRSig) = 
struct
  module IR = IR
  type var = IR.var * int

  let var_equal (v1,i1) (v2,i2) =
    IR.var_equal v1 v2 && i1=i2

  let var_orig (v,_) = IR.var_orig v

  let var_name_debug (v,_) = IR.var_name_debug v

  let var_name (v,i) = Printf.sprintf "%s_%d" (IR.var_name v) i

  let var_name_g (v,i) = Printf.sprintf "%s_%d" (IR.var_name_g v) i

  let bc_num (v,_)  = IR.bc_num v

  let var_origin = fst

  let var_ssa_index = snd
end

module type VarSig =
sig
  module IR : IRSig
  type var = IR.var * int
  val var_equal : var -> var -> bool
  val var_orig : var -> bool
  val var_name_debug: var -> string option
  val var_name: var -> string
  val var_name_g: var -> string
  val bc_num: var -> int option
  val var_origin : var -> IR.var
  val var_ssa_index : var -> int
end

module T (Var:VarSig) 
  (Instr:Bir.InstrSig with type var_i = Var.var) 
  (Exc:Cmn.ExceptionSig with type var_e = Var.var) =
struct
  module Var_t = Var
  module Instr_t = Instr
  module Exc_t = Exc
  type t = {
    params : (JBasics.value_type * Var_t.var) list;
    code : Instr.instr array;
    phi_nodes : (Var_t.var * Var_t.var array) list array;
    (** Array of phi nodes assignments. Each phi nodes assignments at point [pc] must
	be executed before the corresponding [code.(pc)] instruction. *)
    exc_tbl : Exc.exception_handler list;
    line_number_table : (int * int) list option;
    pc_bc2ir : int Ptmap.t;
    pc_ir2bc : int array; 
  }

  let jump_target code =
    let jump_target = Array.make (Array.length code.code) false in
      List.iter (fun e -> jump_target.(e.Exc.e_handler) <- true) code.exc_tbl;
      Array.iter
	(fun instr ->
	   match Instr.instr_jump_to instr with
	       Some n -> jump_target.(n) <- true;
	     | None -> ())
	code.code;
      jump_target

  let print_phi_node (x,args) =
    Printf.sprintf "%s := PHI(%s)"
      (Var_t.var_name_g x)
      (JUtil.print_list_sep_map "," Var_t.var_name_g (Array.to_list args))

  let print_phi_nodes l =
    JUtil.print_list_sep_map "; " print_phi_node l

  let app_phi_nodes l s =
    if l = [] then s
    else (print_phi_nodes l)^"; "^s

  let rec print_code phi_nodes code i acc =
    if i<0 then acc
    else print_code phi_nodes code (i-1)
      (Printf.sprintf "%3d: %s" i 
	 (app_phi_nodes phi_nodes.(i) (Instr.print_instr code.(i)))::acc)

  let print m =
    let size = Array.length (m.code) in
      print_code m.phi_nodes m.code (size-1) []
	
  let exception_edges m = Exc.exception_edges' m.code m.exc_tbl 
end

module type IR2SsaSig = sig
  module IR : IRSig
  module Var_SSA : VarSig
  module Instr_SSA : Bir.InstrSig
  module Exc_SSA : Cmn.ExceptionSig with type var_e = Var_SSA.var
  val use_bcvars : IR.instr -> Ptset.t
  val def_bcvar : IR.instr -> Ptset.t
  val var_defs : IR.t -> Ptset.t Ptmap.t
  val map_instr : (IR.var -> Var_SSA.var) -> (IR.var -> Var_SSA.var) -> IR.instr -> Instr_SSA.instr
  val map_exception_handler : IR.exception_handler -> Exc_SSA.exception_handler
  val live_analysis : IR.t -> int -> IR.var -> bool
end
 
module type TSsaSig = 
sig
  module Var_t : VarSig
  module Instr_t : Bir.InstrSig
  module Exc_t : Cmn.ExceptionSig with type var_e = Var_t.var
  type t = {
  params : (JBasics.value_type * Var_t.var) list;
    (** [params] contains the method parameters (including the receiver this for
	virtual methods). *)
    code : Instr_t.instr array;
    (** Array of instructions the immediate successor of [pc] is [pc+1].  Jumps
	are absolute. *)
    phi_nodes : (Var_t.var * Var_t.var array) list array;
    (** Array of phi nodes assignments. Each phi nodes assignments at point [pc] must
	be executed before the corresponding [code.(pc)] instruction. *)
    exc_tbl : Exc_t.exception_handler list;
    (** [exc_tbl] is the exception table of the method code. Jumps are
	absolute. *)
    line_number_table : (int * int) list option;
    (** [line_number_table] contains debug information. It is a list of pairs
	[(i,j)] where [i] indicates the index into the bytecode array at which the
	code for a new line [j] in the original source file begins.  *)
    pc_bc2ir : int Ptmap.t;
    (** map from bytecode code line to ir code line (very sparse). *)
    pc_ir2bc : int array; 
    (** map from ir code line to bytecode code line *)
  }  
end 


	
  

module SSA 
  (IR:IRSig) 
  (VarSSA:VarSig with module IR = IR) 
  (Instr:Bir.InstrSig with type var_i = VarSSA.var) 
  (Exc:Cmn.ExceptionSig with type var_e = VarSSA.var)
  (TSSA:TSsaSig with module Var_t = VarSSA 
		 and module Instr_t = Instr 
		 and module Exc_t = Exc)
  (IR2SSA:IR2SsaSig
   with module IR = IR 
   and module Var_SSA = VarSSA
   and module Instr_SSA = Instr
   and module Exc_SSA = Exc)
  = 
struct 
  let preds m =
    let preds = Array.make (Array.length m.IR.code) Ptset.empty in
    let add_pred i j = preds.(i) <- Ptset.add j preds.(i) in
      add_pred 0 (-1);
      Array.iteri 
	(fun i ins ->
	   match IR.instr_succs i ins with
	     | n::j::[] -> add_pred n i; add_pred j i
	     | n::[] -> add_pred n i
	     | [] -> ()
	     | _ -> assert false)
	m.IR.code;
      List.iter
	(fun (i,e) -> add_pred e.IR.e_handler i) (IR.exception_edges m);
      let preds = Array.map Ptset.elements preds in
      let preds i = preds.(i) in
	preds

  let succs m =
    let succs = Array.make (Array.length m.IR.code) Ptset.empty in
    let add i j = succs.(i) <- Ptset.add j succs.(i) in
      Array.iteri 
	(fun i ins ->
	   match IR.instr_succs i ins with
	     | n::j::[] -> add i n; add i j
	     | n::[] -> add i n
	     | [] -> ()
	     | _ -> assert false) 
	m.IR.code;
      List.iter
	(fun (i,e) -> add i e.IR.e_handler) (IR.exception_edges m);
      let succs = Array.map Ptset.elements succs in
      let succs i =
	if i=(-1) then [0] else succs.(i) in
	succs

  let dominator m preds =
    let all = 
      JUtil.foldi 
	(fun i _ -> Ptset.add i) (Ptset.singleton (-1)) m.IR.code 
    in
    let dom = Array.init
      (Array.length m.IR.code)
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
	     let new_s = Ptset.add i (inter_list (preds i)) in
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
	  done
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



  (* see:  
     Cytron, Ron; Ferrante, Jeanne; Rosen, Barry K.; Wegman, Mark N.; 
     and Zadeck, F. Kenneth (1991). 
     "Efficiently computing static single assignment form and the 
     control dependence graph". 
     ACM Transactions on Programming Languages and Systems 13 (4): 451–490.*)
  let place_phi_nodes m n var_defs domf live =
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
			if live y m.IR.vars.(v) then place_node y v;
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

  let show_digraph m succs =
    let f = open_out "debug.dot" in
      Printf.fprintf f "digraph debug {\n";
      Array.iteri
	(fun i _ ->
	   List.iter 
	     (fun j -> Printf.fprintf f "  n%d -> n%d;\n" i j)
	     (succs i))
	m.IR.code;
      Printf.fprintf f "}\n";
      close_out f

  let debug_code m phi_nodes children vars search_h succs =
    Printf.printf "params(%s)\n"
      (JUtil.print_list_sep ","
	 (List.map 
	    (fun (_,x) -> IR.var_name_g x) m.IR.params));
    Array.iteri 
      (fun i op -> 
	 Printf.printf "[%s]%3d: %s\n"
	   (JUtil.print_list_sep " "
	      (List.map 
		 (fun v -> IR.var_name_g (m.IR.vars.(v))) 
		 (Ptmap.fold (fun v _ l -> v::l) (phi_nodes i) [])))
	   i (IR.print_instr op))
      m.IR.code;
    List.iter
      (fun e -> Printf.printf " [%d, %d] --> %d\n" e.IR.e_start e.IR.e_end e.IR.e_handler)
      m.IR.exc_tbl;
    Printf.printf "var_def:\n";
    Ptmap.iter
      (fun v defs ->
	 Printf.printf "   %s: {%s}\n"
	   (IR.var_name_g (m.IR.vars.(v)))
	   (JUtil.print_list_sep "," (List.map string_of_int (Ptset.elements defs)))
      ) vars;
    Printf.printf "search: %s\n" 
      (JUtil.print_list_sep "::" 
	 (List.map 
	    (fun x ->
	       Printf.sprintf "%d(%s)"
		 x 
		 (JUtil.print_list_sep " " (List.map string_of_int (children x)))) search_h));
    show_digraph m succs


  (* Compute the rights indexes for each variable use and def.
     See:  
     Cytron, Ron; Ferrante, Jeanne; Rosen, Barry K.; Wegman, Mark N.; 
     and Zadeck, F. Kenneth (1991). 
     "Efficiently computing static single assignment form and the 
     control dependence graph". 
     ACM Transactions on Programming Languages and Systems 13 (4): 451–490.*)
  let rename m vars children preds succs phi_nodes =
    let c = ref (Ptmap.map (fun _ -> 0) vars) in
    let s = ref (Ptmap.map (fun _ -> []) vars) in
    let rename_use = Array.make (Array.length m.IR.code) Ptmap.empty in
    let rename_def = ref Ptmap.empty in
    let phi_nodes = Ptmap.mapi (fun n s -> 
				  let n_preds = List.length (preds n) in
				    Ptset.fold (fun v -> Ptmap.add v (Array.make n_preds (-1))) s Ptmap.empty) phi_nodes in
    let phi_nodes i =
      try Ptmap.find i phi_nodes with Not_found -> Ptmap.empty in
    let search_h = ref [] in
    let top_s i x = 
      try
	(match Ptmap.find x !s  with
	   | [] -> 
	       Printf.printf "ERROR top(s(%s)) in %d\n" 
		 (IR.var_name_g (m.IR.vars.(x))) i;
	       debug_code m  phi_nodes children vars !search_h succs;
	       assert false
	   | i::_ -> i)
      with Not_found -> 
	Printf.printf "ERROR s(%s) not found at node %d\n" 
	  (IR.var_name_g (m.IR.vars.(x))) i;
	debug_code m  phi_nodes children vars !search_h succs;      
	assert false in
    let pop_s x = 
      try
	(match Ptmap.find x !s  with
	   | [] -> assert false
	   | _::q -> s := Ptmap.add x q !s)
      with Not_found -> assert false in
    let rec search x =
      search_h := x :: !search_h;
      let def = if x<0 then
        (List.fold_right (fun (_,x) -> Ptset.add (IR.index x))
	   m.IR.params Ptset.empty)
      else IR2SSA.def_bcvar m.IR.code.(x) in
	Ptmap.iter
	  (fun v _ -> 
	     let i = Ptmap.find v !c in
	       rename_def := Ptmap.add x i !rename_def;
	       s := Ptmap.add v (i::(Ptmap.find v !s)) !s;
	       c := Ptmap.add v (i+1) !c)
	  (phi_nodes x);
	if x>=0 then begin
	  let vars = IR2SSA.use_bcvars m.IR.code.(x) in
	    rename_use.(x) <-
	      Ptset.fold 
	      (fun v -> Ptmap.add v (top_s x v)) vars Ptmap.empty
	end;
	Ptset.iter
	  (fun v ->
	     let i = Ptmap.find v !c in
	       rename_def := Ptmap.add x i !rename_def;
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
      (fun i -> Ptmap.find i !rename_def),
    (fun i -> (rename_use.(i))),
    phi_nodes

  let run ir_code live =
    (*
      let rd = ReachDef.run ir_code in
      let jump_target = jump_target ir_code in
    *)    
    let n = Array.length ir_code.IR.code in
    let preds = preds ir_code in
    let succs = succs ir_code in
    let dom = dominator ir_code preds in
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
      ((fun i -> dom.(i)),idom,domf,phi_nodes,var_defs,rename)

  let to_string s =
    Printf.sprintf "{%s}"
      (JUtil.print_list_sep_map "," string_of_int (Ptset.elements s))

  let vars_to_string s =
    Printf.sprintf "{%s}"
      (JUtil.print_list_sep_map "," IR.var_name_g s)

  let debug ir_code (dom,idom,domf,phi_nodes,var_defs,(rename_def,rename_use,phi_nodes')) =
    let jump_target = IR.jump_target ir_code in
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
	   (try Printf.printf "Def: %d\n" (rename_def i)
	    with Not_found -> ());
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
    let run = run ir_code (IR2SSA.live_analysis ir_code) in
    let debug i msg = 
      Printf.printf "-----------------\nFailure %s line %d\n-----------------\n" msg i;
      debug ir_code run in
    let (_,_,_,_,_,(rename_def,rename_use,phi_nodes')) = run in
    let def i x = 
      if IR.var_ssa x then (x,0)
      else try (x,rename_def i) with Not_found -> debug i "def lookup"; assert false in
    let use i = 
      let rename_use = try rename_use i with Not_found -> debug i "use lookup"; assert false in
	function x -> 
	  if IR.var_ssa x then (x,0)
	  else
	    try (x,Ptmap.find (IR.index x) rename_use) 
	    with Not_found -> debug i (Printf.sprintf "use var %s lookup" (IR.var_name_g x)); assert false in
    let phi_nodes i =
      try
	Ptmap.fold
	  (fun v args l -> 
	     let x_ir = ir_code.IR.vars.(v) in
	     let x = (x_ir,rename_def i) in
	     let args = Array.map (fun i -> (x_ir,i)) args in
	       (x,args)::l)
	  (phi_nodes' i) []
      with Not_found -> debug i "phi lookup"; assert false in
    let code = Array.mapi
      (fun i -> IR2SSA.map_instr (def i) (use i)) ir_code.IR.code in
    let exc_t = List.map IR2SSA.map_exception_handler ir_code.IR.exc_tbl in
      {
	TSSA.params = List.map (fun (t,x) -> (t,(x,0))) ir_code.IR.params;
	TSSA.code  = code;
	TSSA.phi_nodes = Array.init (Array.length code) phi_nodes;
	TSSA.exc_tbl = exc_t;
	TSSA.line_number_table = ir_code.IR.line_number_table;
	TSSA.pc_bc2ir = ir_code.IR.pc_bc2ir;
	TSSA.pc_ir2bc = ir_code.IR.pc_ir2bc
      }

end