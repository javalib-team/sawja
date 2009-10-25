open JBasics
open Javalib
open JCode
open Bir

include Cmn

type binop =
  | ArrayLoad of JBasics.jvm_array_type 
  | Add of jvm_basic_type
  | Sub of jvm_basic_type 
  | Mult of jvm_basic_type
  | Div of jvm_basic_type
  | Rem of jvm_basic_type
  | IShl | IShr  | IAnd | IOr  | IXor | IUshr
  | LShl | LShr | LAnd | LOr | LXor | LUshr
  | CMP of comp
      
type expr =
  | Const of const
  | Var of value_type * var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * class_name * field_signature
  | StaticField of class_name * field_signature

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
	   | InstanceOf _ -> `Int)
  | Binop (ArrayLoad t,e,_) -> type_of_array_content t e
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
and type_of_array_content t e =
  match type_of_expr e with
    | TObject (TArray t) -> t 
    | _ -> (* we use the type found in the OpArrayLoad argument *)
	(match t with
	   | `Int | `Short | `Char | `ByteBool -> TBasic `Int
	   | `Long -> TBasic `Long
	   | `Float -> TBasic `Float
	   | `Double -> TBasic `Double
	   | `Object -> TObject (TClass java_lang_object))

type opexpr =  
  | Uninit of class_name * int
  | E of expr

type virtual_call_kind =
  | VirtualCall of object_type
  | InterfaceCall of class_name

type check = 
  | CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr * object_type
  | CheckArithmetic of expr

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
  params : (value_type * var) list;  (** method parameters *)
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

let to_binop = function
  | Bir.ArrayLoad t -> ArrayLoad t
  | Bir.Add t -> Add t
  | Bir.Sub t -> Sub t
  | Bir.Mult t -> Mult t
  | Bir.Div t -> Div t
  | Bir.Rem t -> Rem t
  | Bir.IShl -> IShl
  | Bir.IShr ->IShr
  | Bir.LShl ->LShl
  | Bir.LShr ->LShr
  | Bir.IAnd ->IAnd
  | Bir.IOr ->IOr
  | Bir.IXor ->IXor
  | Bir.IUshr ->IUshr
  | Bir.LAnd ->LAnd
  | Bir.LOr ->LOr
  | Bir.LXor ->LXor
  | Bir.LUshr ->LUshr
  | Bir.CMP c ->
      CMP (match c with 
	       Bir.DG -> DG
	     | Bir.DL -> DL
	     | Bir.FG -> FG
	     | Bir.FL -> FL 
	     | Bir.L -> L
      )

let rec to_expr = function
  | Bir.Var (t,x) -> Var (t,x) 
  | Bir.Field (e,c,f) -> Field (to_expr e,c,f)
  | Bir.StaticField (c,f) -> StaticField (c,f)
  | Bir.Const i -> Const i
  | Bir.Unop (op,e) -> Unop (op,to_expr e)
  | Bir.Binop (op,e1,e2) -> Binop (to_binop op,to_expr e1,to_expr e2)

let to_virtual_call_kind = function
  | Bir.VirtualCall t -> VirtualCall t
  | Bir.InterfaceCall c -> InterfaceCall c

let to_check = function
    Bir.CheckNullPointer e -> CheckNullPointer (to_expr e)
  | Bir.CheckArrayBound (a,i) -> CheckArrayBound (to_expr a,to_expr i)
  | Bir.CheckArrayStore (a,v) -> CheckArrayStore (to_expr a,to_expr v)
  | Bir.CheckNegativeArraySize e -> CheckNegativeArraySize (to_expr e)
  | Bir.CheckCast (e,t) -> CheckCast (to_expr e,t)
  | Bir.CheckArithmetic e -> CheckArithmetic (to_expr e)
  

let to_instr = function
  | Bir.AffectVar (v,expr) -> AffectVar (v,to_expr expr)
  | Bir.AffectArray(e1,e2,e3) -> AffectArray(to_expr e1,to_expr e2,to_expr e3)
  | Bir.AffectField(e1,cn,fs,e2) -> AffectField(to_expr e1,cn,fs,to_expr e2) 
  | Bir.AffectStaticField(cn,fs,e) -> AffectStaticField(cn,fs,to_expr e)
  | Bir.Ifd ((cmp,e1,e2),i) -> Ifd ((cmp,to_expr e1,to_expr e2),i)
  | Bir.New(v,cn,vtl,el) -> New (v,cn,vtl,List.map to_expr el)
  | Bir.NewArray(v,vt,el) -> NewArray(v,vt,List.map to_expr el)
  | Bir.InvokeStatic(v,cn,ms,el) -> InvokeStatic(v,cn,ms,List.map to_expr el)
  | Bir.InvokeVirtual(optv,expr, kind, ms, el) -> InvokeVirtual(optv,to_expr expr,to_virtual_call_kind kind, ms, List.map to_expr el)
  | Bir.InvokeNonVirtual(optv, e, cn, ms, el) -> InvokeNonVirtual(optv,to_expr e, cn, ms, List.map to_expr el)
  | Bir.MonitorEnter e -> MonitorEnter (to_expr e)
  | Bir.MonitorExit e ->  MonitorExit (to_expr e)
  | Bir.MayInit cn -> MayInit cn
  | Bir.Check c -> Check (to_check c)
  | _ -> assert false

let to_oexpr = function
  | None -> None
  | Some e -> Some (to_expr e)

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

let print_field ?(long_fields=false) c f =
  if long_fields then
    Printf.sprintf "<%s:%s>" (JPrint.class_name c) (fs_name f)
  else (fs_name f)

let bracket b s =
  if b then s else Printf.sprintf "(%s)" s 

let rec print_expr first_level = function
  | Var (t,x) -> Printf.sprintf "%s:%s" (var_name_g x) (print_typ t)
  | Field (e,c,f) -> Printf.sprintf "%s.%s" (print_expr false e) (print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Const i -> print_const i
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_expr false e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_expr true e)
  | Binop (ArrayLoad _,e1,e2) -> Printf.sprintf "%s[%s]" (print_expr false e1) (print_expr true e2) 
  | Binop (Add _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s+%s" (print_expr false e1) (print_expr false e2))
  | Binop (Sub _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s-%s" (print_expr false e1) (print_expr false e2))
  | Binop (Mult _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s*%s" (print_expr false e1) (print_expr false e2))
  | Binop (Div _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s/%s" (print_expr false e1) (print_expr false e2))
  | Binop (op,e1,e2) -> Printf.sprintf "%s(%s,%s)" (print_binop op) (print_expr true e1) (print_expr true e2) 

let print_cmp  (c,e1,e2) =
  match c with
    | `Eq -> Printf.sprintf "%s == %s" (print_expr false e1) (print_expr false e2)
    | `Ne -> Printf.sprintf "%s != %s" (print_expr false e1) (print_expr false e2)
    | `Lt -> Printf.sprintf "%s < %s" (print_expr false e1) (print_expr false e2)
    | `Ge -> Printf.sprintf "%s >= %s" (print_expr false e1) (print_expr false e2)
    | `Gt -> Printf.sprintf "%s > %s" (print_expr false e1) (print_expr false e2)
    | `Le -> Printf.sprintf "%s <= %s" (print_expr false e1) (print_expr false e2)

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
	  | CheckCast (e,t) -> Printf.sprintf "checkcast %s:%s" (print_expr true e) (JDumpBasics.object_value_signature t)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_expr true e)
      end

let print_last_instr = function
  | Goto i -> Printf.sprintf "goto %d" i
  | Throw e -> Printf.sprintf "throw %s" (print_expr false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr false e)

let is_check = function
  | Check _ -> true
  | _ -> false

let rec print_block ?(explicit_exception=true) b =
  Printf.sprintf "%3d: %s%s%s\n" b.label
    (List.fold_left
       (fun s i -> 
	  if (not explicit_exception)&&(is_check i) 
	  then s
	  else s^(print_instr i)^"\n           ")
       "" b.instrs)
    (print_last_instr b.last)
    (List.fold_left 
       (fun s e -> 
	  Printf.sprintf "%s\n           catch %s goto %d" s
	    (match e.catch_type with
	       | None -> "_"
	       | Some cn -> JPrint.class_name cn)
	    e.handler) ""
       b.handlers)

let print_code ?(explicit_exception=true) = List.map (print_block ~explicit_exception:explicit_exception)

let print ?(explicit_exception=true) m = print_code ~explicit_exception:explicit_exception m.code

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
  | [Bir.Throw e] -> ([],Throw (to_expr e))
  | [Bir.Return e] -> ([],Return (to_oexpr e))
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
      

  
