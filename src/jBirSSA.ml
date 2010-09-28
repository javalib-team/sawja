
module Var = SsaBir.Var(JBir)
(*module JBirIR = (JBir:SsaBir.IRSig with type var = JBir.var
				   and type instr = JBir.instr
				   and type exception_handler 
				     = JBir.exception_handler
				   and type t = JBir.t
				   and module InstrRep = JBir.InstrRep)
module VarR = (Var:SsaBir.VarSig with module IR = JBir)*)

module Exception = Cmn.Exception (Var)
module Instr = JBir.InstrRep (Var)    
module SsaT = SsaBir.T (Var) (Instr) (Exception)

module JBir2SSA = struct
  open JBir
  module IR = JBir
  module Var_SSA = Var
  module Instr_SSA = Instr
  module Exc_SSA = Exception
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
    let map_expr f =
      let rec aux expr = 
	match expr with
	  | Const c -> Instr.Const c
	  | StaticField (c,fs) -> Instr.StaticField (c,fs)
	  | Field (e,c,fs) -> Instr.Field (aux e,c,fs)
	  | Var (t,x) -> Instr.Var (t,f x)
	  | Unop (s,e) -> Instr.Unop (s,aux e)
	  | Binop (s,e1,e2) -> Instr.Binop (s,aux e1,aux e2)
      in aux 
    in
    let use = map_expr use in
      function
	| AffectField (e1,c,f0,e2) -> Instr.AffectField (use e1,c,f0,use e2)
	| Ifd ((c,e1,e2), pc) -> Instr.Ifd ((c,use e1,use e2), pc) 
	| Goto i -> Instr.Goto i
	| Throw e -> Instr.Throw (use e) 
	| MayInit c -> Instr.MayInit c
	| Nop -> Instr.Nop
	| Return None -> Instr.Return None
	| Return (Some e) -> Instr.Return (Some (use e))
	| AffectVar (x,e) -> Instr.AffectVar (def x,use e)
	| MonitorEnter e -> Instr.MonitorEnter (use e)
	| MonitorExit e -> Instr.MonitorExit (use e)
	| AffectStaticField (c,f0,e) -> Instr.AffectStaticField (c,f0,use e)
	| NewArray (x,t,le) -> Instr.NewArray (def x,t,List.map (use) le)
	| New (x,c,lt,le) -> Instr.New (def x,c,lt,List.map (use) le)
	| InvokeStatic (None,c,ms,le) -> Instr.InvokeStatic (None,c,ms,List.map (use) le)
	| InvokeStatic (Some x,c,ms,le) -> Instr.InvokeStatic (Some (def x),c,ms,List.map (use) le)
	| InvokeVirtual (None,e,c,ms,le) -> Instr.InvokeVirtual (None,use e,c,ms,List.map (use) le)
	| InvokeVirtual (Some x,e,c,ms,le) -> Instr.InvokeVirtual (Some (def x),use e,c,ms,List.map (use) le)
	| InvokeNonVirtual (None,e,c,ms,le) -> Instr.InvokeNonVirtual (None,use e,c,ms,List.map (use) le)
	| InvokeNonVirtual (Some x,e,c,ms,le) -> Instr.InvokeNonVirtual (Some (def x),use e,c,ms,List.map (use) le)
	| AffectArray (e1,e2,e3) -> Instr.AffectArray (use e1,use e2,use e3)
	| Check c -> Instr.Check begin
	    match c with
	      | CheckArrayBound (e1,e2) -> Instr.CheckArrayBound (use e1,use e2)
	      | CheckArrayStore (e1,e2) -> Instr.CheckArrayStore (use e1,use e2)
	      | CheckNullPointer e -> Instr.CheckNullPointer (use e)
	      | CheckNegativeArraySize e -> Instr.CheckNegativeArraySize (use e)
	      | CheckCast (e,t) -> Instr.CheckCast (use e,t)
	      | CheckArithmetic e -> Instr.CheckArithmetic (use e)
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
    let live = Live_bir.run ir_code in
      Live_bir.Env.mem x (live i)

end


module SsaJBir = SsaBir.SSA (JBir) (Var) (Instr) (Exception) (SsaT) (JBir2SSA)
(* Common parts*)


include Var
include SsaT
include Exception
include Instr

let transform_from_bir = SsaJBir.transform_from_ir

let transform ?(bcv=false) cm code = 
  SsaJBir.transform_from_ir (JBir.transform ~bcv:bcv cm code)
