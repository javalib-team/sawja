module Var = SsaBir.Var(A3Bir)
module Exception = Cmn.Exception (Var)
module Instr = A3Bir.InstrRep (Var)    
module SsaT = SsaBir.T (Var) (Instr) (Exception)

module A3Bir2SSA = struct
  open A3Bir
  module IR = A3Bir
  module Var_SSA = Var
  module Instr_SSA = Instr
  module Exc_SSA = Exception
  let use_bcvars =
    let vars acc = function
      | Const _ -> acc
      | Var (_,x) -> if var_ssa x then acc else Ptset.add (index x) acc 
    in
    let expr acc = function
      | BasicExpr e	
      | Field (e,_,_) 
      | Unop (_,e) -> vars acc e
      | Binop (_,e1,e2) -> vars (vars acc e1) e2
      | StaticField _ -> acc 
    in
      function
	| AffectField (e1,_,_,e2) 
	| Ifd ((_,e1,e2), _) -> vars (vars Ptset.empty e1) e2
	| Goto _ 
	| MayInit _ 
	| Nop 
	| Return None -> Ptset.empty
	| AffectStaticField (_,_,e)
	| AffectVar (_,e) -> expr Ptset.empty e
	| Throw e 
	| Return (Some e)
	| MonitorEnter e 
	| MonitorExit e -> vars Ptset.empty e
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

  let map_instr def use =
    let map_basic_expr f = 
      function
	| Const c -> Instr.Const c
	| Var (t,x) -> Instr.Var (t,f x)
    in
    let map_expr f =
      let map_basic_expr = map_basic_expr f in
      function
	| BasicExpr e -> Instr.BasicExpr (map_basic_expr e)  
	| StaticField (c,fs) -> Instr.StaticField (c,fs)
	| Field (e,c,fs) -> Instr.Field (map_basic_expr e,c,fs)	  
	| Unop (s,e) -> Instr.Unop (s,map_basic_expr e)
	| Binop (s,e1,e2) -> 
	    Instr.Binop (s,map_basic_expr e1,map_basic_expr e2)
    in
    let use_b = map_basic_expr use in
    let use = map_expr use in
      function
	| AffectField (e1,c,f0,e2) -> Instr.AffectField (use_b e1,c,f0,use_b e2)
	| Ifd ((c,e1,e2), pc) -> Instr.Ifd ((c,use_b e1,use_b e2), pc) 
	| Goto i -> Instr.Goto i
	| Throw e -> Instr.Throw (use_b e) 
	| MayInit c -> Instr.MayInit c
	| Nop -> Instr.Nop
	| Return None -> Instr.Return None
	| Return (Some e) -> Instr.Return (Some (use_b e))
	| AffectVar (x,e) -> Instr.AffectVar (def x,use e)
	| MonitorEnter e -> Instr.MonitorEnter (use_b e)
	| MonitorExit e -> Instr.MonitorExit (use_b e)
	| AffectStaticField (c,f0,e) -> Instr.AffectStaticField (c,f0,use e)
	| NewArray (x,t,le) -> Instr.NewArray (def x,t,List.map (use_b) le)
	| New (x,c,lt,le) -> Instr.New (def x,c,lt,List.map (use_b) le)
	| InvokeStatic (None,c,ms,le) -> Instr.InvokeStatic (None,c,ms,List.map (use_b) le)
	| InvokeStatic (Some x,c,ms,le) -> Instr.InvokeStatic (Some (def x),c,ms,List.map (use_b) le)
	| InvokeVirtual (None,e,c,ms,le) -> Instr.InvokeVirtual (None,use_b e,c,ms,List.map (use_b) le)
	| InvokeVirtual (Some x,e,c,ms,le) -> Instr.InvokeVirtual (Some (def x),use_b e,c,ms,List.map (use_b) le)
	| InvokeNonVirtual (None,e,c,ms,le) -> Instr.InvokeNonVirtual (None,use_b e,c,ms,List.map (use_b) le)
	| InvokeNonVirtual (Some x,e,c,ms,le) -> Instr.InvokeNonVirtual (Some (def x),use_b e,c,ms,List.map (use_b) le)
	| AffectArray (e1,e2,e3) -> Instr.AffectArray (use_b e1,use_b e2,use_b e3)
	| Check c -> Instr.Check begin
	    match c with
	      | CheckArrayBound (e1,e2) -> Instr.CheckArrayBound (use_b e1,use_b e2)
	      | CheckArrayStore (e1,e2) -> Instr.CheckArrayStore (use_b e1,use_b e2)
	      | CheckNullPointer e -> Instr.CheckNullPointer (use_b e)
	      | CheckNegativeArraySize e -> Instr.CheckNegativeArraySize (use_b e)
	      | CheckCast (e,t) -> Instr.CheckCast (use_b e,t)
	      | CheckArithmetic e -> Instr.CheckArithmetic (use_b e)
	      | CheckLink op -> Instr.CheckLink op
	  end

  let map_exception_handler e = {
    Exception.e_start = e.e_start;
    Exception.e_end = e.e_end;
    Exception.e_handler = e.e_handler;
    Exception.e_catch_type = e.e_catch_type;
    Exception.e_catch_var = (e.e_catch_var,0)
  }

  
  let live_analysis ir_code i x = 
    let live = Live_a3bir.run ir_code in
      Live_a3bir.Env.mem x (live i)

end


module SsaA3Bir = SsaBir.SSA (A3Bir) (Var) (Instr) (Exception) (SsaT) (A3Bir2SSA)
(* Common parts*)


include Var
include SsaT
include Exception
include Instr

let transform_from_a3bir = SsaA3Bir.transform_from_ir

let transform ?(bcv=false) cm code = 
  SsaA3Bir.transform_from_ir (A3Bir.transform ~bcv:bcv cm code)
