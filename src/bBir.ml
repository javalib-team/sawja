open JBasics
open Javalib
open JCode
open Bir

include Cmn

type binop = Bir.binop
type expr = Bir.expr
type virtual_call_kind = Bir.virtual_call_kind
type check = Bir.check

type instr =
  | AffectVar of var * expr (** x := e *)
  | AffectArray of expr * expr * expr (** e1\[e2\] := e3 *) 
  | AffectField of expr * JBasics.class_name * JBasics.field_signature * expr (** e1.<C:f> := e2 *)
  | AffectStaticField of JBasics.class_name * JBasics.field_signature * expr  (** <C:f> := e *)
  | Ifd of ([ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr) * int
  | New of var * JBasics.class_name * JBasics.value_type list * expr list (** x := new C<sig>(e1,...,en) *)
  | NewArray of var * JBasics.value_type * expr list  (** x := new C\[e1\]...\[en\] *)       
      (** value_type is the type of the array content *)
  | InvokeStatic of var option * JBasics.class_name *  JBasics.method_signature * expr list (** x :=  C.m<sig>(e1,...,en) or C.m<sig>(e1,...,en)  *)
  | InvokeVirtual of var option * expr * virtual_call_kind * JBasics.method_signature * expr list (** x := e.m<sig>(e1,...,en) or e.m<sig>(e1,...,en)  *)
  | InvokeNonVirtual of var option * expr * JBasics.class_name * JBasics.method_signature * expr list (** x := e.C.m<sig>(e1,...,en) or e.C.m<sig>(e1,...,en)  *)
  | MonitorEnter of expr
  | MonitorExit of expr
  | MayInit of JBasics.class_name
  | Check of check

type last_instr =
  | Goto of int
  | Throw of expr
  | Return of expr option

type exception_handler = {
  handler : int;
  catch_type : JBasics.class_name option
}
    
type block = {
  label : int;
  instrs : instr list;
  last : last_instr;
  handlers : exception_handler list;
}

type t = {
  params : var list;  (** method parameters *)
  code : block list
}

let bcvar = Bir.bcvar

exception Bad_Multiarray_dimension = Bir.Bad_Multiarray_dimension 
exception Bad_stack = Bir.Bad_stack
exception Subroutine = Bir.Subroutine
exception Content_constraint_on_Uninit = Bir.Content_constraint_on_Uninit
exception Type_constraint_on_Uninit = Bir.Type_constraint_on_Uninit
exception NonemptyStack_backward_jump = Bir.NonemptyStack_backward_jump
exception Uninit_is_not_expr = Bir.Uninit_is_not_expr

let to_instr = function
  | Bir.AffectVar (v,expr) -> AffectVar (v,expr)
  | Bir.AffectArray(e1,e2,e3) -> AffectArray(e1, e2, e3)
  | Bir.AffectField(e1,cn,fs,e2) -> AffectField(e1,cn,fs,e2) 
  | Bir.AffectStaticField(cn,fs,e) -> AffectStaticField(cn,fs,e)
  | Bir.Ifd ((cmp,e1,e2),i) -> Ifd ((cmp,e1,e2),i)
  | Bir.New(v,cn,vtl,el) -> New (v,cn,vtl,el)
  | Bir.NewArray(v,vt,el) -> NewArray(v,vt,el)
  | Bir.InvokeStatic(v,cn,ms,el) -> InvokeStatic(v,cn,ms,el)
  | Bir.InvokeVirtual(optv,expr, kind, ms, el) -> InvokeVirtual(optv, expr, kind, ms, el)
  | Bir.InvokeNonVirtual(optv, e, cn, ms, el) -> InvokeNonVirtual(optv, e, cn, ms, el)
  | Bir.MonitorEnter e -> MonitorEnter (e)
  | Bir.MonitorExit e ->  MonitorExit (e)
  | Bir.MayInit cn -> MayInit cn
  | Bir.Check c -> Check c
  | _ -> assert false

let to_last_instr = function
  | Bir.Goto i -> Goto i
  | Bir.Throw e -> Throw e
  | Bir.Return e -> Return e
  | _ -> assert false


let print_binop = function
  | ArrayLoad t -> Printf.sprintf "ArrayLoad %s" (print_typ t)
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

let print_field ?(long_fields=false) c f =
  if long_fields then
    Printf.sprintf "<%s:%s>" (JPrint.class_name c) (fs_name f)
  else (fs_name f)

let bracket b s =
  if b then s else Printf.sprintf "(%s)" s 

let print_expr = Bir.print_expr

let print_cmp = Bir.print_cmp

let print_instr = function
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr true e)
  | AffectField (e1,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_expr false e1) (print_field c f) (print_expr true e2)
  | AffectArray (e1,e2,e3) -> Printf.sprintf "%s[%s] := %s" (print_expr false e1) (print_expr true e2) (print_expr true e3)
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (print_list_sep "," (print_expr true) le) 
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_expr true e)) le) 
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s)" (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s)" (var_name_g x) (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | InvokeVirtual (r,e1,_,ms,le) -> 
      Printf.sprintf "%s%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr false e1) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | InvokeNonVirtual (r,e1,kd,ms,le) -> 
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr false e1) (JPrint.class_name kd) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | MonitorEnter e -> Printf.sprintf "monitorenter(%s)" (print_expr true e)
  | MonitorExit e -> Printf.sprintf "monitorexit(%s)" (print_expr true e)
  | MayInit c -> Printf.sprintf "mayinit %s" (JPrint.class_name c)
  | Check c ->
      begin
	match c with 
	    CheckNullPointer e -> Printf.sprintf "notnull %s" (print_expr true e)
	  | CheckArrayBound (a,i) -> Printf.sprintf "checkbound %s[%s]"  (print_expr true a) (print_expr true i)
	  | CheckArrayStore (a,v) -> Printf.sprintf "checkstore %s[] <- %s"  (print_expr true a) (print_expr true v)
	  | CheckNegativeArraySize e -> Printf.sprintf "checknegsize %s" (print_expr true e)
	  | CheckCast e -> Printf.sprintf "checkcast %s" (print_expr true e)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_expr true e)
      end

let print_last_instr = function
  | Goto i -> Printf.sprintf "goto %d" i
  | Throw e -> Printf.sprintf "throw %s" (print_expr false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr false e)

let rec print_block b =
  Printf.sprintf "%3d: %s%s%s\n" b.label
    (List.fold_left (fun s i -> s^(print_instr i)^"\n           ") "" b.instrs)
    (print_last_instr b.last)
    (List.fold_left 
       (fun s e -> 
	  Printf.sprintf "%s\n           catch %s goto %d" s
	    (match e.catch_type with
	       | None -> "_"
	       | Some cn -> JPrint.class_name cn)
	    e.handler) ""
       b.handlers)

let print_code = List.map print_block

let print m = print_code m.code

let compute_handlers handlers i =
  let handlers = List.filter (fun e -> e.e_start <= i && i < e.e_end) handlers in
  let handlers = List.map 
		   (fun e -> { handler = e.e_handler; 
			       catch_type = e.e_catch_type })
		   handlers in
    handlers

let rec add_jump next = function
  | [] -> ([],Goto next)
  | [Bir.Goto j] -> ([],Goto j)
  | [Bir.Throw e] -> ([],Throw e)
  | [Bir.Return e] -> ([],Return e)
  | Bir.Nop::q -> add_jump next q
  | i::q -> 
      let (instrs,last) = add_jump next q in
	((to_instr i)::instrs,last)


let make_block label instr next handlers =
  let (instrs,last) = add_jump next instr in
  { label = label;
    instrs = instrs;
    last = last;
    handlers = compute_handlers handlers label
  }

let build_blocks jump_target handlers code =
  let rec next = function
    | [] -> [],[]
    | (pc,instrs)::q ->
	if jump_target.(pc) then [],(pc,instrs)::q 
	else 
	  let (next,q) = next q in
	    (instrs@next,q) 
  in 
  let first = function
    | [] -> -1
    | (pc,_)::_ -> pc in
  let rec aux = function
    | [] -> []
    | (pc,instrs)::q ->
	  let (next,q) = next q in
	    (make_block pc (instrs@next) (first q) handlers)::(aux q) 
  in
    aux code

let bir2bbir bir : t = 
  List.iter
    (fun e -> 
	 bir.jump_target.(e.e_start) <- true; (* David: could not work well with strange programs *)
	 bir.jump_target.(e.e_end) <- true) (* David: could not work well with strange programs *)
      bir.exc_tbl;
  { params = bir.Bir.params ;
    code = build_blocks bir.Bir.jump_target bir.Bir.exc_tbl bir.Bir.code
  }

let transform ?(compress=false) j_m j_code =
  let code = Bir.transform_flat ~compress:compress j_m j_code in 
    bir2bbir code
      

  
