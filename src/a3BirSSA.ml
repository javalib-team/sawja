include SsaBir.T (SsaBir.Var(A3Bir)) (A3Bir.InstrRep (SsaBir.Var(A3Bir)))
include A3Bir.InstrRep (SsaBir.Var(A3Bir))    
include SsaBir.Var(A3Bir)

module A3Bir2SSA = struct
  let use_bcvars =
    let vars acc = function
      | A3Bir.Const _ -> acc
      | A3Bir.Var (_,x) -> if A3Bir.var_ssa x then acc else Ptset.add (A3Bir.index x) acc 
    in
    let expr acc = function
      | A3Bir.BasicExpr e	
      | A3Bir.Field (e,_,_) 
      | A3Bir.Unop (_,e) -> vars acc e
      | A3Bir.Binop (_,e1,e2) -> vars (vars acc e1) e2
      | A3Bir.StaticField _ -> acc 
    in
      function
	| A3Bir.AffectVar (_,e) 
	| A3Bir.AffectStaticField (_,_,e) -> expr Ptset.empty e
	| A3Bir.AffectField (e1,_,_,e2) 
	| A3Bir.Ifd ((_,e1,e2), _) -> vars (vars Ptset.empty e1) e2
	| A3Bir.Goto _ 
	| A3Bir.MayInit _ 
	| A3Bir.Nop 
	| A3Bir.Return None -> Ptset.empty
	| A3Bir.Throw e 
	| A3Bir.Return (Some e)
	| A3Bir.MonitorEnter e 
	| A3Bir.MonitorExit e -> vars Ptset.empty e
	| A3Bir.NewArray (_,_,le)
	| A3Bir.New (_,_,_,le) 
	| A3Bir.InvokeStatic (_,_,_,le) -> List.fold_left vars Ptset.empty le
	| A3Bir.InvokeVirtual (_,e,_,_,le) 
	| A3Bir.InvokeNonVirtual (_,e,_,_,le) -> List.fold_left vars Ptset.empty (e::le)
	| A3Bir.AffectArray (e1,e2,e3) -> vars (vars (vars Ptset.empty e1) e2) e3
	| A3Bir.Check c -> begin
	    match c with
	      | A3Bir.CheckArrayBound (e1,e2)
	      | A3Bir.CheckArrayStore (e1,e2) -> vars (vars Ptset.empty e1) e2
	      | A3Bir.CheckNullPointer e
	      | A3Bir.CheckNegativeArraySize e
	      | A3Bir.CheckCast (e,_)
	      | A3Bir.CheckArithmetic e -> vars Ptset.empty e
	      | A3Bir.CheckLink _ -> Ptset.empty
	  end

  let def_bcvar = function
    | A3Bir.AffectVar (v,_) 
    | A3Bir.NewArray (v,_,_)
    | A3Bir.New (v,_,_,_) 
    | A3Bir.InvokeStatic (Some v,_,_,_)
    | A3Bir.InvokeVirtual (Some v,_,_,_,_) 
    | A3Bir.InvokeNonVirtual (Some v,_,_,_,_) 
      -> if A3Bir.var_ssa v then Ptset.empty else Ptset.singleton (A3Bir.index v) 
    | _ -> Ptset.empty

  let var_defs m =
    JUtil.foldi
      (fun i ins -> 
	 match ins with
	   | A3Bir.AffectVar (x,_) 
	   | A3Bir.NewArray (x,_,_)
	   | A3Bir.New (x,_,_,_) 
	   | A3Bir.InvokeStatic (Some x,_,_,_)
	   | A3Bir.InvokeVirtual (Some x,_,_,_,_) 
	   | A3Bir.InvokeNonVirtual (Some x,_,_,_,_) 
	     -> if A3Bir.var_ssa x  then (fun m->m) else Ptmap.add ~merge:Ptset.union (A3Bir.index x) (Ptset.singleton i)
	   | _ -> fun m -> m)
      (List.fold_right
	 (fun (_,x) -> Ptmap.add (A3Bir.index x) (Ptset.singleton (-1)))
	 m.A3Bir.params Ptmap.empty)
      m.A3Bir.code 	

  let map_instr def use =
    let map_basic_expr f = 
      function
	| A3Bir.Const c -> Const c
	| A3Bir.Var (t,x) -> Var (t,f x)
    in
    let map_expr f =
      let map_basic_expr = map_basic_expr f in
      function
	| A3Bir.BasicExpr e -> BasicExpr (map_basic_expr e)  
	| A3Bir.StaticField (c,fs) -> StaticField (c,fs)
	| A3Bir.Field (e,c,fs) -> Field (map_basic_expr e,c,fs)	  
	| A3Bir.Unop (s,e) -> Unop (s,map_basic_expr e)
	| A3Bir.Binop (s,e1,e2) -> 
	    Binop (s,map_basic_expr e1,map_basic_expr e2)
    in
    let use_b = map_basic_expr use in
    let use = map_expr use in
      function
	| A3Bir.AffectField (e1,c,f0,e2) -> AffectField (use_b e1,c,f0,use_b e2)
	| A3Bir.Ifd ((c,e1,e2), pc) -> Ifd ((c,use_b e1,use_b e2), pc) 
	| A3Bir.Goto i -> Goto i
	| A3Bir.Throw e -> Throw (use_b e) 
	| A3Bir.MayInit c -> MayInit c
	| A3Bir.Nop -> Nop
	| A3Bir.Return None -> Return None
	| A3Bir.Return (Some e) -> Return (Some (use_b e))
	| A3Bir.AffectVar (x,e) -> AffectVar (def x,use e)
	| A3Bir.MonitorEnter e -> MonitorEnter (use_b e)
	| A3Bir.MonitorExit e -> MonitorExit (use_b e)
	| A3Bir.AffectStaticField (c,f0,e) -> AffectStaticField (c,f0,use e)
	| A3Bir.NewArray (x,t,le) -> NewArray (def x,t,List.map (use_b) le)
	| A3Bir.New (x,c,lt,le) -> New (def x,c,lt,List.map (use_b) le)
	| A3Bir.InvokeStatic (None,c,ms,le) -> InvokeStatic (None,c,ms,List.map (use_b) le)
	| A3Bir.InvokeStatic (Some x,c,ms,le) -> InvokeStatic (Some (def x),c,ms,List.map (use_b) le)
	| A3Bir.InvokeVirtual (None,e,c,ms,le) -> InvokeVirtual (None,use_b e,c,ms,List.map (use_b) le)
	| A3Bir.InvokeVirtual (Some x,e,c,ms,le) -> InvokeVirtual (Some (def x),use_b e,c,ms,List.map (use_b) le)
	| A3Bir.InvokeNonVirtual (None,e,c,ms,le) -> InvokeNonVirtual (None,use_b e,c,ms,List.map (use_b) le)
	| A3Bir.InvokeNonVirtual (Some x,e,c,ms,le) -> InvokeNonVirtual (Some (def x),use_b e,c,ms,List.map (use_b) le)
	| A3Bir.AffectArray (e1,e2,e3) -> AffectArray (use_b e1,use_b e2,use_b e3)
	| A3Bir.Check c -> Check begin
	    match c with
	      | A3Bir.CheckArrayBound (e1,e2) -> CheckArrayBound (use_b e1,use_b e2)
	      | A3Bir.CheckArrayStore (e1,e2) -> CheckArrayStore (use_b e1,use_b e2)
	      | A3Bir.CheckNullPointer e -> CheckNullPointer (use_b e)
	      | A3Bir.CheckNegativeArraySize e -> CheckNegativeArraySize (use_b e)
	      | A3Bir.CheckCast (e,t) -> CheckCast (use_b e,t)
	      | A3Bir.CheckArithmetic e -> CheckArithmetic (use_b e)
	      | A3Bir.CheckLink op -> CheckLink op
	  end

  let map_exception_handler e = {
    e_start = e.A3Bir.e_start;
    e_end = e.A3Bir.e_end;
    e_handler = e.A3Bir.e_handler;
    e_catch_type = e.A3Bir.e_catch_type;
    e_catch_var = (e.A3Bir.e_catch_var,0)
  }

  
  let live_analysis ir_code i x = 
    let live = Live_a3bir.run ir_code in
      Live_a3bir.Env.mem x (live i)

end

module SsaA3Bir = SsaBir.SSA 
  (A3Bir) 
  (SsaBir.T (SsaBir.Var(A3Bir)) (A3Bir.InstrRep (SsaBir.Var(A3Bir))))
  (struct 
     include A3Bir2SSA
     type ir_t = A3Bir.t
     type ir_var = A3Bir.var
     type ir_instr = A3Bir.instr
     type ir_exc_h = A3Bir.exception_handler
     type ssa_var = var
     type ssa_instr = instr
     type ssa_exc_h = exception_handler
   end)


let transform_from_a3bir = SsaA3Bir.transform_from_ir

let transform ?(bcv=false) cm code = 
  SsaA3Bir.transform_from_ir (A3Bir.transform ~bcv:bcv cm code)
