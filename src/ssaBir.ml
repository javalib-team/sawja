open Javalib_pack
open JBasics
open JCode 
open Javalib
open JBir

(* TODO:

 - index variables ?
 - what do we do with ssa_index of variables already in ssa form

*)

module Var = 
  struct
    type var = JBir.var * int

    let var_equal (v1,i1) (v2,i2) =
      var_equal v1 v2 && i1=i2

    let var_orig (v,_) = var_orig v

    let var_name_debug (v,_) = var_name_debug v

    let var_name (v,i) = Printf.sprintf "%s_%d" (var_name v) i

    let var_name_g (v,i) = Printf.sprintf "%s_%d" (var_name_g v) i

    let bc_num (v,_)  = bc_num v

    let var_origin = fst

    let var_ssa_index = snd
  end



module T (Var:Cmn.VarSig) (Instr:Bir.InstrSig) 
  (Exc:Cmn.ExceptionSig with type v=Var.var) =
  struct
    type t = {
      params : (JBasics.value_type * Var.var) list;
      code : Instr.instr array;
      phi_nodes : (Var.var * Var.var array) list array;
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
	(Var.var_name_g x)
	(JUtil.print_list_sep_map "," Var.var_name_g (Array.to_list args))

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



let preds m =
  let preds = Array.make (Array.length m.code) Ptset.empty in
  let add_pred i j = preds.(i) <- Ptset.add j preds.(i) in
    add_pred 0 (-1);
    Array.iteri 
      (fun i ins ->
	 match ins with
	   | Ifd (_ , j) -> add_pred (i+1) i; add_pred j i
	   | Goto j -> add_pred j i
	   | Throw _
	   | Return _ -> ()
	   | _ -> add_pred (i+1) i) m.code;
    List.iter
      (fun (i,e) -> add_pred e.e_handler i) (exception_edges m);
    let preds = Array.map Ptset.elements preds in
    let preds i = preds.(i) in
      preds

let succs m =
  let succs = Array.make (Array.length m.code) Ptset.empty in
  let add i j = succs.(i) <- Ptset.add j succs.(i) in
    Array.iteri 
      (fun i ins ->
	 match ins with
	   | Ifd (_ , j) -> add i (i+1); add i j
	   | Goto j -> add i j
	   | Throw _
	   | Return _ -> ()
	   | _ -> add i (i+1)) m.code;
    List.iter
      (fun (i,e) -> add i e.e_handler) (exception_edges m);
    let succs = Array.map Ptset.elements succs in
    let succs i =
      if i=(-1) then [0] else succs.(i) in
      succs

let dominator m preds =
    let all = JUtil.foldi (fun i _ -> Ptset.add i) (Ptset.singleton (-1)) m.code in
    let dom = Array.init
		(Array.length m.code)
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

let var_defs m =
  JUtil.foldi
    (fun i ins -> 
       match ins with
	 | AffectVar (x,_) 
	 | NewArray (x,_,_)
	 | New (x,_,_,_) 
	 | InvokeStatic (Some x,_,_,_)
	 | InvokeVirtual (Some x,_,_,_,_) 
	 | InvokeNonVirtual (Some x,_,_,_,_) 
	   -> if var_ssa x  then (fun m->m) else Ptmap.add ~merge:Ptset.union (index x) (Ptset.singleton i)
	 | _ -> fun m -> m)
    (List.fold_right
       (fun (_,x) -> Ptmap.add (index x) (Ptset.singleton (-1)))
       m.params Ptmap.empty)
    m.code 

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
		      if live y m.vars.(v) then place_node y v;
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

let use_bcvars =
  let rec vars acc = function
    | Const _ -> acc
    | Var (_,x) -> if var_ssa x then acc else Ptset.add (index x) acc 
    | Field (e,_,_) 
    | Unop (_,e) -> vars acc e
    | Binop (_,e1,e2) -> vars (vars acc e1) e2
    | StaticField _ -> acc in
    function
      | AffectField (e1,_,_,e2) 
      | Ifd ((_,e1,e2), _) -> vars (vars Ptset.empty e1) e2
      | Goto _ 
      | MayInit _ 
      | Nop 
      | Return None -> Ptset.empty
      | Throw e 
      | Return (Some e)
      | AffectVar (_,e) 
      | MonitorEnter e 
      | MonitorExit e
      | AffectStaticField (_,_,e) -> vars Ptset.empty e
      | NewArray (_,_,le)
      | New (_,_,_,le) 
      | InvokeStatic (_,_,_,le) -> List.fold_left vars Ptset.empty le
      | InvokeVirtual (_,e,_,_,le) 
      | InvokeNonVirtual (_,e,_,_,le) -> List.fold_left vars Ptset.empty (e::le)
      | AffectArray (e1,e2,e3) -> vars (vars (vars Ptset.empty e1) e2) e3
      | Check c -> begin
	  match c with
	    | CheckArrayBound (e1,e2)
	    | CheckArrayStore (e1,e2) -> vars (vars Ptset.empty e1) e2
	    | CheckNullPointer e
	    | CheckNegativeArraySize e
	    | CheckCast (e,_)
	    | CheckArithmetic e -> vars Ptset.empty e
	    | CheckLink _ -> Ptset.empty
	end

let def_bcvar = function
  | AffectVar (v,_) 
  | NewArray (v,_,_)
  | New (v,_,_,_) 
  | InvokeStatic (Some v,_,_,_)
  | InvokeVirtual (Some v,_,_,_,_) 
  | InvokeNonVirtual (Some v,_,_,_,_) 
    -> if var_ssa v then Ptset.empty else Ptset.singleton (index v) 
  | _ -> Ptset.empty

let show_digraph m succs =
  let f = open_out "debug.dot" in
    Printf.fprintf f "digraph debug {\n";
    Array.iteri
      (fun i _ ->
	 List.iter 
	   (fun j -> Printf.fprintf f "  n%d -> n%d;\n" i j)
	   (succs i))
      m.code;
    Printf.fprintf f "}\n";
    close_out f

let debug_code m phi_nodes children vars search_h succs =
  Printf.printf "params(%s)\n"
    (JUtil.print_list_sep ","
       (List.map 
	  (fun (_,x) -> var_name_g x) m.params));
  Array.iteri 
    (fun i op -> 
       Printf.printf "[%s]%3d: %s\n"
	 (JUtil.print_list_sep " "
	    (List.map 
	       (fun v -> var_name_g (m.vars.(v))) 
	       (Ptmap.fold (fun v _ l -> v::l) (phi_nodes i) [])))
	 i (print_instr op))
    m.code;
  List.iter
    (fun e -> Printf.printf " [%d, %d] --> %d\n" e.e_start e.e_end e.e_handler)
    m.exc_tbl;
  Printf.printf "var_def:\n";
  Ptmap.iter
    (fun v defs ->
       Printf.printf "   %s: {%s}\n"
	 (var_name_g (m.vars.(v)))
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
  let rename_use = Array.make (Array.length m.code) Ptmap.empty in
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
	     Printf.printf "ERROR top(s(%s)) in %d\n" (var_name_g (m.vars.(x))) i;
	     debug_code m  phi_nodes children vars !search_h succs;
	     assert false
	 | i::_ -> i)
    with Not_found -> 
      Printf.printf "ERROR s(%s) not found at node %d\n" (var_name_g (m.vars.(x))) i;
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
               (List.fold_right (fun (_,x) -> Ptset.add (index x))
		  m.params Ptset.empty)
              else def_bcvar m.code.(x) in
      Ptmap.iter
	(fun v _ -> 
	   let i = Ptmap.find v !c in
	     rename_def := Ptmap.add x i !rename_def;
	     s := Ptmap.add v (i::(Ptmap.find v !s)) !s;
	     c := Ptmap.add v (i+1) !c)
	(phi_nodes x);
      if x>=0 then begin
	let vars = use_bcvars m.code.(x) in
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
  let n = Array.length ir_code.code in
  let preds = preds ir_code in
  let succs = succs ir_code in
  let dom = dominator ir_code preds in
  let (idom,children) = idom dom in
  let domf = domf n preds idom in
  let var_defs = var_defs ir_code in
  let phi_nodes = place_phi_nodes ir_code n var_defs domf live in
  let rename = rename ir_code var_defs children preds succs phi_nodes in
  let phi_nodes i =
    try
      List.map
	(fun v -> ir_code.vars.(v))
	(Ptset.elements (Ptmap.find i phi_nodes)) 
    with Not_found -> [] in
    ((fun i -> dom.(i)),idom,domf,phi_nodes,var_defs,rename)

let to_string s =
  Printf.sprintf "{%s}"
    (JUtil.print_list_sep_map "," string_of_int (Ptset.elements s))

let vars_to_string s =
  Printf.sprintf "{%s}"
    (JUtil.print_list_sep_map "," var_name_g s)

let debug ir_code (dom,idom,domf,phi_nodes,var_defs,(rename_def,rename_use,phi_nodes')) =
  let jump_target = jump_target ir_code in
    Ptmap.iter
      (fun v defs ->
	 Printf.printf "  %s:" (var_name_g (ir_code.vars.(v)));
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
	     (fun v i -> Printf.printf " %s_[%d]" (var_name_g ir_code.vars.(v)) i)
	     rename_use;
	   print_newline ();		 
	   let phi_nodes = phi_nodes' i in
	     Ptmap.iter 
	       (fun v args -> 
		  let v = var_name_g ir_code.vars.(v) in
		    Printf.printf "      %s := PHI(%s)\n"
		      v (JUtil.print_list_sep "," (List.map (Printf.sprintf "%s_%d" v) (Array.to_list args))))
	       phi_nodes;
	     Printf.printf "%s%3d: %s\n"
	       (if jump_target.(i) then "x" else " ")
	       i (print_instr op))
      ir_code.code;
    print_newline ()

include Var
module Exception = Cmn.Exception (Var)
module Instr = JBir.InstrRep (Var)
include Exception
include T (Var) (Instr) (Exception)
include Instr

let map_expr f =
  let rec aux expr = 
    match expr with
      | JBir.Const c -> Const c
      | JBir.StaticField (c,f) -> StaticField (c,f)
      | JBir.Field (e,c,f) -> Field (aux e,c,f)
      | JBir.Var (t,x) -> Var (t,f x)
      | JBir.Unop (s,e) -> Unop (s,aux e)
      | JBir.Binop (s,e1,e2) -> Binop (s,aux e1,aux e2)
  in aux

let map_instr def use =
  let use = map_expr use in
    function
  | JBir.AffectField (e1,c,f0,e2) -> Instr.AffectField (use e1,c,f0,use e2)
  | JBir.Ifd ((c,e1,e2), pc) -> Ifd ((c,use e1,use e2), pc) 
  | JBir.Goto i -> Goto i
  | JBir.Throw e -> Throw (use e) 
  | JBir.MayInit c -> MayInit c
  | JBir.Nop -> Nop
  | JBir.Return None -> Return None
  | JBir.Return (Some e) -> Return (Some (use e))
  | JBir.AffectVar (x,e) -> AffectVar (def x,use e)
  | JBir.MonitorEnter e -> MonitorEnter (use e)
  | JBir.MonitorExit e -> MonitorExit (use e)
  | JBir.AffectStaticField (c,f0,e) -> AffectStaticField (c,f0,use e)
  | JBir.NewArray (x,t,le) -> NewArray (def x,t,List.map (use) le)
  | JBir.New (x,c,lt,le) -> New (def x,c,lt,List.map (use) le)
  | JBir.InvokeStatic (None,c,ms,le) -> InvokeStatic (None,c,ms,List.map (use) le)
  | JBir.InvokeStatic (Some x,c,ms,le) -> InvokeStatic (Some (def x),c,ms,List.map (use) le)
  | JBir.InvokeVirtual (None,e,c,ms,le) -> InvokeVirtual (None,use e,c,ms,List.map (use) le)
  | JBir.InvokeVirtual (Some x,e,c,ms,le) -> InvokeVirtual (Some (def x),use e,c,ms,List.map (use) le)
  | JBir.InvokeNonVirtual (None,e,c,ms,le) -> InvokeNonVirtual (None,use e,c,ms,List.map (use) le)
  | JBir.InvokeNonVirtual (Some x,e,c,ms,le) -> InvokeNonVirtual (Some (def x),use e,c,ms,List.map (use) le)
  | JBir.AffectArray (e1,e2,e3) -> AffectArray (use e1,use e2,use e3)
  | JBir.Check c -> Check begin
      match c with
	| JBir.CheckArrayBound (e1,e2) -> CheckArrayBound (use e1,use e2)
	| JBir.CheckArrayStore (e1,e2) -> CheckArrayStore (use e1,use e2)
	| JBir.CheckNullPointer e -> CheckNullPointer (use e)
	| JBir.CheckNegativeArraySize e -> CheckNegativeArraySize (use e)
	| JBir.CheckCast (e,t) -> CheckCast (use e,t)
	| JBir.CheckArithmetic e -> CheckArithmetic (use e)
	| JBir.CheckLink op -> CheckLink op
    end

let map_exception_handler e = {
  e_start = e.JBir.e_start;
  e_end = e.JBir.e_end;
  e_handler = e.JBir.e_handler;
  e_catch_type = e.JBir.e_catch_type;
  e_catch_var = (e.JBir.e_catch_var,0)
}


let transform_from_bir ir_code =
  let live = Live.run ir_code in
  let live i x = Live.Env.mem x (live i) in
  let run = run ir_code live in
  let debug i msg = 
    Printf.printf "-----------------\nFailure %s line %d\n-----------------\n" msg i;
    debug ir_code run in
  let (_,_,_,_,_,(rename_def,rename_use,phi_nodes')) = run in
  let def i x = 
    if JBir.var_ssa x then (x,0)
    else try (x,rename_def i) with Not_found -> debug i "def lookup"; assert false in
  let use i = 
    let rename_use = try rename_use i with Not_found -> debug i "use lookup"; assert false in
      function x -> 
	if JBir.var_ssa x then (x,0)
	else
	  try (x,Ptmap.find (JBir.index x) rename_use) 
	  with Not_found -> debug i (Printf.sprintf "use var %s lookup" (JBir.var_name_g x)); assert false in
  let phi_nodes i =
    try
      Ptmap.fold
	(fun v args l -> 
	   let x_ir = ir_code.JBir.vars.(v) in
	   let x = (x_ir,rename_def i) in
	   let args = Array.map (fun i -> (x_ir,i)) args in
	     (x,args)::l)
	(phi_nodes' i) []
    with Not_found -> debug i "phi lookup"; assert false in
  let code = Array.mapi
    (fun i -> map_instr (def i) (use i)) ir_code.JBir.code in
  let exc_t = List.map map_exception_handler ir_code.JBir.exc_tbl in
    {
      params = List.map (fun (t,x) -> (t,(x,0))) ir_code.JBir.params;
      code  = code;
      phi_nodes = Array.init (Array.length code) phi_nodes;
      exc_tbl = exc_t;
      line_number_table = ir_code.JBir.line_number_table;
      pc_bc2ir = ir_code.JBir.pc_bc2ir;
      pc_ir2bc = ir_code.JBir.pc_ir2bc
    }

let transform ?(bcv=false) cm code = 
  transform_from_bir (JBir.transform ~bcv:bcv cm code)
