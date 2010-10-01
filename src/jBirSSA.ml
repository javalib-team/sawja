include SsaBir.T (SsaBir.Var(JBir)) (JBir.InstrRep (SsaBir.Var(JBir)))
include JBir.InstrRep (SsaBir.Var(JBir))  
include SsaBir.Var(JBir)



module JBir2SSA = struct
  let use_bcvars =
    let rec vars acc = function
      | JBir.Const _ -> acc
      | JBir.Var (_,x) -> if JBir.var_ssa x then acc else Ptset.add (JBir.index x) acc 
      | JBir.Field (e,_,_) 
      | JBir.Unop (_,e) -> vars acc e
      | JBir.Binop (_,e1,e2) -> vars (vars acc e1) e2
      | JBir.StaticField _ -> acc in
      function
	| JBir.AffectField (e1,_,_,e2) 
	| JBir.Ifd ((_,e1,e2), _) -> vars (vars Ptset.empty e1) e2
	| JBir.Goto _ 
	| JBir.MayInit _ 
	| JBir.Nop 
	| JBir.Return None -> Ptset.empty
	| JBir.Throw e 
	| JBir.Return (Some e)
	| JBir.AffectVar (_,e) 
	| JBir.MonitorEnter e 
	| JBir.MonitorExit e
	| JBir.AffectStaticField (_,_,e) -> vars Ptset.empty e
	| JBir.NewArray (_,_,le)
	| JBir.New (_,_,_,le) 
	| JBir.InvokeStatic (_,_,_,le) -> List.fold_left vars Ptset.empty le
	| JBir.InvokeVirtual (_,e,_,_,le) 
	| JBir.InvokeNonVirtual (_,e,_,_,le) -> List.fold_left vars Ptset.empty (e::le)
	| JBir.AffectArray (e1,e2,e3) -> vars (vars (vars Ptset.empty e1) e2) e3
	| JBir.Check c -> begin
	    match c with
	      | JBir.CheckArrayBound (e1,e2)
	      | JBir.CheckArrayStore (e1,e2) -> vars (vars Ptset.empty e1) e2
	      | JBir.CheckNullPointer e
	      | JBir.CheckNegativeArraySize e
	      | JBir.CheckCast (e,_)
	      | JBir.CheckArithmetic e -> vars Ptset.empty e
	      | JBir.CheckLink _ -> Ptset.empty
	  end

  let def_bcvar = function
    | JBir.AffectVar (v,_) 
    | JBir.NewArray (v,_,_)
    | JBir.New (v,_,_,_) 
    | JBir.InvokeStatic (Some v,_,_,_)
    | JBir.InvokeVirtual (Some v,_,_,_,_) 
    | JBir.InvokeNonVirtual (Some v,_,_,_,_) 
      -> if JBir.var_ssa v then Ptset.empty else Ptset.singleton (JBir.index v) 
    | _ -> Ptset.empty

  let var_defs m =
    JUtil.foldi
      (fun i ins -> 
	 match ins with
	   | JBir.AffectVar (x,_) 
	   | JBir.NewArray (x,_,_)
	   | JBir.New (x,_,_,_) 
	   | JBir.InvokeStatic (Some x,_,_,_)
	   | JBir.InvokeVirtual (Some x,_,_,_,_) 
	   | JBir.InvokeNonVirtual (Some x,_,_,_,_) 
	     -> if JBir.var_ssa x  then (fun m->m) else Ptmap.add ~merge:Ptset.union (JBir.index x) (Ptset.singleton i)
	   | _ -> fun m -> m)
      (List.fold_right
	 (fun (_,x) -> Ptmap.add (JBir.index x) (Ptset.singleton (-1)))
	 m.JBir.params Ptmap.empty)
      m.JBir.code 

  let map_instr def use =
    let map_expr f =
      let rec aux expr = 
	match expr with
	  | JBir.Const c -> Const c
	  | JBir.StaticField (c,fs) -> StaticField (c,fs)
	  | JBir.Field (e,c,fs) -> Field (aux e,c,fs)
	  | JBir.Var (t,x) -> Var (t,f x)
	  | JBir.Unop (s,e) -> Unop (s,aux e)
	  | JBir.Binop (s,e1,e2) -> Binop (s,aux e1,aux e2)
      in aux 
    in
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
	      | JBir.CheckLink op -> CheckLink op
	  end

  let map_exception_handler e = {
    e_start = e.JBir.e_start;
    e_end = e.JBir.e_end;
    e_handler = e.JBir.e_handler;
    e_catch_type = e.JBir.e_catch_type;
    e_catch_var = (e.JBir.e_catch_var,0)
  }

  
  let live_analysis ir_code i x = 
    let live = Live_bir.run ir_code in
      Live_bir.Env.mem x (live i)

end


module SsaJBir = SsaBir.SSA 
  (JBir) 
  (SsaBir.T (SsaBir.Var(JBir)) (JBir.InstrRep (SsaBir.Var(JBir))))
  (struct 
     include JBir2SSA
     type ir_t = JBir.t
     type ir_var = JBir.var
     type ir_instr = JBir.instr
     type ir_exc_h = JBir.exception_handler
     type ssa_var = var
     type ssa_instr = instr
     type ssa_exc_h = exception_handler
   end)
(* Common parts*)


let transform_from_bir = SsaJBir.transform_from_ir

let transform ?(bcv=false) cm code = 
    transform_from_bir (JBir.transform ~bcv:bcv cm code)

