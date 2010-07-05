open Javalib_pack
open JBasics
open JCode 
open Javalib
open JBir

(* TODO:

 - index variables ?
 - what do we do with ssa_index of variables already in ssa form

*)

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
    (JUtil.print_list_sep_map "," JBir.var_name_g s)

let debug ir_code (dom,idom,domf,phi_nodes,var_defs,(rename_def,rename_use,phi_nodes')) =
  let jump_target = JBir.jump_target ir_code in
    Ptmap.iter
      (fun v defs ->
	 Printf.printf "  %s:" (JBir.var_name_g (ir_code.JBir.vars.(v)));
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
	     (fun v i -> Printf.printf " %s_[%d]" (JBir.var_name_g ir_code.JBir.vars.(v)) i)
	     rename_use;
	   print_newline ();		 
	   let phi_nodes = phi_nodes' i in
	     Ptmap.iter 
	       (fun v args -> 
		  let v = JBir.var_name_g ir_code.JBir.vars.(v) in
		    Printf.printf "      %s := PHI(%s)\n"
		      v (JUtil.print_list_sep "," (List.map (Printf.sprintf "%s_%d" v) (Array.to_list args))))
	       phi_nodes;
	     Printf.printf "%s%3d: %s\n"
	       (if jump_target.(i) then "x" else " ")
	       i (JBir.print_instr op))
      ir_code.JBir.code;
    print_newline ()

let print_phi (target,args) = 
  Printf.sprintf "%s := PHI(%s)" (var_name_g target) 
    (JUtil.print_list_sep_map "," var_name_g (Array.to_list args))


type const =
    [ `ANull
    | `Byte of int
    | `Class of JBasics.object_type
    | `Double of float
    | `Float of float
    | `Int of int32
    | `Long of int64
    | `Short of int
    | `String of string ]

type var = JBir.var * int

let var_equal (v1,i1) (v2,i2) =
  var_equal v1 v2 && i1=i2

let var_orig (v,_) = var_orig v

let var_name (v,i) = Printf.sprintf "%s_%d" (var_name v) i

let var_name_g (v,i) = Printf.sprintf "%s_%d" (var_name_g v) i

let bc_num (v,_)  = bc_num v

let var_origin = fst

let var_ssa_index = snd

type expr =
    Const of JBir.const
  | Var of JBasics.value_type * var
  | Unop of JBir.unop * expr
  | Binop of JBir.binop * expr * expr
  | Field of expr * JBasics.class_name * JBasics.field_signature
  | StaticField of JBasics.class_name * JBasics.field_signature

let basic_to_num = function
  | `Int2Bool -> `Int
  | `Long -> `Long
  | `Double -> `Double
  | `Float -> `Float

let rec type_of_expr = function
  | Var (t,_) -> t
  | Field (_,_,f)
  | StaticField (_,f) -> fs_type f
  | Const i -> begin
      match i with
	| `ANull
	| `Class _
	| `String _ -> TObject (TClass java_lang_object)
	| `Byte _
	| `Short _
	| `Int _ -> TBasic `Int
	| `Double _ -> TBasic `Double
	| `Float _ -> TBasic `Float
	| `Long  _ -> TBasic `Long
    end
  | Unop (Cast t,_) -> TObject t
  | Unop (u,_) ->
      TBasic
	(match u with
	   | Neg t -> basic_to_num t
	   | Conv c ->
	       (match c with
		  | I2L | F2L | D2L -> `Long
		  | I2F | L2F | D2F -> `Float
		  | I2D | L2D | F2D -> `Double
		  | L2I | F2I | D2I | I2B | I2C | I2S -> `Int)
	   | ArrayLength
	   | InstanceOf _ -> `Int
	   | _ -> assert false)
  | Binop (ArrayLoad t,_,_) -> t
  | Binop (b,_,_) ->
      TBasic
      (match b with
	 | ArrayLoad _ -> assert false
	 | Add t
	 | Sub t
	 | Mult t
	 | Div t
	 | Rem t ->
	     (match t with
		| `Int2Bool -> `Int
		| `Long -> `Long
		| `Double -> `Double
		| `Float -> `Float)
	 | IShl | IShr  | IAnd | IOr  | IXor | IUshr -> `Int
	 | LShl | LShr | LAnd | LOr | LXor | LUshr -> `Long
	 | CMP _ -> `Int)

type check =
  | CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr * JBasics.object_type
  | CheckArithmetic of expr

type instr =
    Nop
  | AffectVar of var * expr
  | AffectArray of expr * expr * expr
  | AffectField of expr * JBasics.class_name * JBasics.field_signature * expr
  | AffectStaticField of JBasics.class_name * JBasics.field_signature * expr
  | Goto of int
  | Ifd of ([ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr) * int
  | Throw of expr
  | Return of expr option
  | New of var * JBasics.class_name * JBasics.value_type list * expr list
  | NewArray of var * JBasics.value_type * expr list
  | InvokeStatic of var option * JBasics.class_name *  JBasics.method_signature * expr list
  | InvokeVirtual of var option * expr * JBir.virtual_call_kind * JBasics.method_signature * expr list
  | InvokeNonVirtual of var option * expr * JBasics.class_name * JBasics.method_signature * expr list
  | MonitorEnter of expr
  | MonitorExit of expr 
  | MayInit of JBasics.class_name
  | Check of check

type exception_handler = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : JBasics.class_name option;
  e_catch_var : var
}

type t = {
  params : (JBasics.value_type * var) list;
  code : instr array;
  phi_nodes : (var * var array) list array;
  (** Array of phi nodes assignments. Each phi nodes assignments at point [pc] must
      be executed before the corresponding [code.(pc)] instruction. *)
  exc_tbl : exception_handler list;
  line_number_table : (int * int) list option;
  pc_bc2ir : int Ptmap.t;
  pc_ir2bc : int array; 
}

let jump_target code =
  let jump_target = Array.make (Array.length code.code) false in
    List.iter (fun e -> jump_target.(e.e_handler) <- true) code.exc_tbl;
    Array.iter
      (fun instr ->
	 match instr with
	   | Ifd (_, n)
	   | Goto n -> jump_target.(n) <- true;
	   | _ -> ())
      code.code;
    jump_target

let exception_edges m =
  JUtil.foldi 
      (fun i _ l ->
	 List.rev_append
	   (List.map 
	      (fun e -> (i,e))
	      (List.filter (fun e -> e.e_start <= i && i < e.e_end) m.exc_tbl))
	   l)
      [] 
      m.code

let print_handler exc = 
  Printf.sprintf "      [%d-%d] -> %d (%s %s)" exc.e_start exc.e_end exc.e_handler
    (match exc.e_catch_type with
       | None -> "_"
       | Some cl -> JPrint.class_name cl)
    (var_name_g exc.e_catch_var)

let print_field ?(long_fields=false) c f =
  if long_fields then
    Printf.sprintf "<%s:%s>" (JPrint.class_name c) (fs_name f)
  else (fs_name f)

let bracket b s =
  if b then s else Printf.sprintf "(%s)" s


let print_unop = function
  | Neg t -> Printf.sprintf "%cNeg" (JDumpBasics.jvm_basic_type t)
  | Conv conv ->
      begin
	match conv with
	  | I2L -> "I2L"  | I2F -> "I2F"  | I2D -> "I2D"
	  | L2I -> "L2I"  | L2F -> "L2F"  | L2D -> "L2D"
	  | F2I -> "F2I"  | F2L -> "F2L"  | F2D -> "F2D"
	  | D2I -> "D2I"  | D2L -> "D2L"  | D2F -> "D2F"
	  | I2B -> "I2B"  | I2C -> "I2C"  | I2S -> "I2S"
      end
  | ArrayLength -> "ArrayLength"
  | InstanceOf ot -> Printf.sprintf "InstanceOf %s" (Javalib.JPrint.object_type ot)
  | Cast _ -> assert false

let print_typ t =
  let bt2ss = function
    | `Long -> "J"
    | `Float -> "F"
    | `Double -> "D"
    | `Int -> "I"
    | `Short -> "S"
    | `Char -> "C"
    | `Byte -> "B"
    | `Bool -> "Z"
  in
  let rec ot2ss = function
    | TClass _ -> "O"
    | TArray t -> "["^ vt2ss t
  and vt2ss = function
    | TBasic t -> bt2ss t
    | TObject t -> ot2ss t
  in vt2ss t

let print_const = function
  | `ANull -> "null"
  | `Int i -> Printf.sprintf "%ld" i
  | `Long i -> Printf.sprintf "%Ld" i
  | `Float f -> Printf.sprintf "%f" f
  | `Double f -> Printf.sprintf "%f" f
  | `Byte n -> Printf.sprintf "%d" n
  | `Short a -> Printf.sprintf "%d " a
  | `Class c -> Printf.sprintf "%s" (JDumpBasics.object_value_signature c)
  | `String s -> Printf.sprintf "'%s'" s

let print_binop = function
  | ArrayLoad _ -> Printf.sprintf "ArrayLoad"
  | Add t -> Printf.sprintf "%cAdd" (JDumpBasics.jvm_basic_type t)
  | Sub t -> Printf.sprintf "%cSub" (JDumpBasics.jvm_basic_type t)
  | Mult t -> Printf.sprintf "%cMult" (JDumpBasics.jvm_basic_type t)
  | Div t -> Printf.sprintf "%cDiv" (JDumpBasics.jvm_basic_type t)
  | Rem t -> Printf.sprintf "%cRem" (JDumpBasics.jvm_basic_type t)
  | IShl -> "IShl"  | IShr -> "IShr"  | LShl -> "LShl"
  | LShr -> "LShr"  | IAnd -> "And"  | IOr -> "IOr"
  | IXor -> "IXor"  | IUshr -> "IUshr"  | LAnd -> "LAnd"
  | LOr -> "LOr"  | LXor -> "LXor"  | LUshr -> "LUshr"
  | CMP c -> Printf.sprintf "CMP %s"
      (match c with
	   DG -> "DG"
	 | DL -> "DL"
	 | FG -> "FG"
	 | FL -> "FL"
	 | L -> "L"
      )

let rec print_expr ?(show_type=true) first_level = function
  | Var (t,x) -> 
      if show_type then Printf.sprintf "%s:%s" (var_name_g x) (print_typ t)
      else (var_name_g x)
  | Field (e,c,f) -> Printf.sprintf "%s.%s" (print_expr ~show_type:show_type false e) (print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Const i -> print_const i
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_expr ~show_type:show_type false e)
  | Unop (Cast t,e) -> Printf.sprintf "(%s) %s" (print_typ (TObject t)) (print_expr ~show_type:show_type true e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_expr ~show_type:show_type true e)
  | Binop (ArrayLoad t,e1,e2) -> 
      if show_type then Printf.sprintf "%s[%s]:%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type true e2) (print_typ t)
      else Printf.sprintf "%s[%s]" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type true e2)
  | Binop (Add _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s+%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (Sub _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s-%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (Mult _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s*%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (Div _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s/%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (op,e1,e2) -> Printf.sprintf "%s(%s,%s)" (print_binop op) (print_expr ~show_type:show_type true e1) (print_expr ~show_type:show_type true e2)

let print_cmp ?(show_type=true) (c,e1,e2) =
  match c with
    | `Eq -> Printf.sprintf "%s == %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Ne -> Printf.sprintf "%s != %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Lt -> Printf.sprintf "%s < %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Ge -> Printf.sprintf "%s >= %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Gt -> Printf.sprintf "%s > %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Le -> Printf.sprintf "%s <= %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)

let rec print_list_sep_rec sep pp = function
  | [] -> ""
  | x::q -> sep^(pp x)^(print_list_sep_rec sep pp q)

let rec print_list_sep_list_rec sep pp = function
  | [] -> []
  | x::q -> (sep^(pp x))::(print_list_sep_list_rec sep pp q)

let print_list_sep sep pp = function
  | [] -> ""
  | x::q -> (pp x)^(print_list_sep_rec sep pp q)

let print_list_sep_list sep pp = function
  | [] -> []
  | x::q -> (pp x)::(print_list_sep_list_rec sep pp q)

let print_instr ?(show_type=true) = function
  | Nop -> "nop"
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr ~show_type:show_type true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr ~show_type:show_type true e)
  | AffectField (e1,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_expr ~show_type:show_type false e1) (print_field c f) (print_expr ~show_type:show_type true e2)
  | AffectArray (e1,e2,e3) -> Printf.sprintf "%s[%s] := %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type true e2) (print_expr ~show_type:show_type true e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | Throw e -> Printf.sprintf "throw %s" (print_expr ~show_type:show_type false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr ~show_type:show_type false e)
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_expr ~show_type:show_type true e)) le)
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s)" (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s)" (var_name_g x) (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | InvokeVirtual (r,e1,_,ms,le) ->
      Printf.sprintf "%s%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr ~show_type:show_type false e1) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | InvokeNonVirtual (r,e1,kd,ms,le) ->
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr ~show_type:show_type false e1) (JPrint.class_name kd) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | MonitorEnter e -> Printf.sprintf "monitorenter(%s)" (print_expr ~show_type:show_type true e)
  | MonitorExit e -> Printf.sprintf "monitorexit(%s)" (print_expr ~show_type:show_type true e)
  | MayInit c -> Printf.sprintf "mayinit %s" (JPrint.class_name c)
  | Check c ->
      begin
	match c with
	    CheckNullPointer e -> Printf.sprintf "notnull %s" (print_expr ~show_type:show_type true e)
	  | CheckArrayBound (a,i) -> Printf.sprintf "checkbound %s[%s]"  (print_expr ~show_type:show_type true a) (print_expr ~show_type:show_type true i)
	  | CheckArrayStore (a,v) -> Printf.sprintf "checkstore %s[] <- %s"  (print_expr ~show_type:show_type true a) (print_expr ~show_type:show_type true v)
	  | CheckNegativeArraySize e -> Printf.sprintf "checknegsize %s" (print_expr ~show_type:show_type true e)
	  | CheckCast (e,t) -> Printf.sprintf "checkcast %s:%s" (print_expr ~show_type:show_type true e) (JDumpBasics.object_value_signature t)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_expr ~show_type:show_type true e)
      end

let print_expr ?(show_type=true) = print_expr ~show_type:show_type true

let print_phi_node (x,args) =
  Printf.sprintf "%s := PHI(%s)"
    (var_name_g x)
    (JUtil.print_list_sep_map "," var_name_g (Array.to_list args))

let print_phi_nodes l =
  JUtil.print_list_sep_map "; " print_phi_node l

let app_phi_nodes l s =
  if l = [] then s
  else (print_phi_nodes l)^"; "^s

let rec print_code phi_nodes code i acc =
  if i<0 then acc
  else print_code phi_nodes code (i-1)
    (Printf.sprintf "%3d: %s" i 
       (app_phi_nodes phi_nodes.(i) (print_instr code.(i)))::acc)

let print m =
  let size = Array.length (m.code) in
    print_code m.phi_nodes m.code (size-1) []

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
  | JBir.AffectField (e1,c,f0,e2) -> AffectField (use e1,c,f0,use e2)
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
    {
      params = List.map (fun (t,x) -> (t,(x,0))) ir_code.JBir.params;
      code  = code;
      phi_nodes = Array.init (Array.length code) phi_nodes;
      exc_tbl = List.map map_exception_handler ir_code.JBir.exc_tbl;
      line_number_table = ir_code.JBir.line_number_table;
      pc_bc2ir = ir_code.JBir.pc_bc2ir;
      pc_ir2bc = ir_code.JBir.pc_ir2bc
    }

let transform ?(bcv=false) cm code = 
  transform_from_bir (JBir.transform ~bcv:bcv cm code)
