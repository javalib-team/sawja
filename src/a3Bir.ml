

open JBasics
open Javalib
open JCode


include Cmn

type binop =
  | ArrayLoad  of JBasics.jvm_array_type
  | Add of jvm_basic_type
  | Sub of jvm_basic_type 
  | Mult of jvm_basic_type
  | Div of jvm_basic_type
  | Rem of jvm_basic_type
  | IShl | IShr  | IAnd | IOr  | IXor | IUshr
  | LShl | LShr | LAnd | LOr | LXor | LUshr
  | CMP of comp

type basic_expr = 
  | Const of const
  | Var of value_type * var
      
type expr =
  | BasicExpr of basic_expr
  | Unop of unop * basic_expr
  | Binop of binop * basic_expr * basic_expr
  | Field of basic_expr * class_name * field_signature
  | StaticField of class_name * field_signature
	  
let rec type_of_basic_expr = function 
  | Var (t,_) -> t
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
and type_of_expr = function
  | BasicExpr e -> type_of_basic_expr e
  | Field (_,_,f) 
  | StaticField (_,f) -> fs_type f
  | Unop (u,_) -> 
      TBasic 
	(match u with
	   | Neg t -> Bir.basic_to_num t
	   | Conv c ->
	       (match c with
		  | I2L | F2L | D2L -> `Long
		  | I2F | L2F | D2F -> `Float
		  | I2D | L2D | F2D -> `Double
		  | L2I | F2I | D2I | I2B | I2C | I2S -> `Int)
	   | ArrayLength 
	   | InstanceOf _ -> `Int)
  | Binop (ArrayLoad t,e,_) -> type_of_array_content t  e
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
  match type_of_basic_expr e with
    | TObject (TArray t) -> t 
    | _ -> (* we use the type found in the OpArrayLoad argument *)
	(match t with
	   | `Int | `Short | `Char | `ByteBool -> TBasic `Int
	   | `Long -> TBasic `Long
	   | `Float -> TBasic `Float
	   | `Double -> TBasic `Double
	   | `Object -> TObject (TClass java_lang_object))

type virtual_call_kind =
  | VirtualCall of object_type
  | InterfaceCall of class_name

type check = 
  | CheckNullPointer of basic_expr
  | CheckArrayBound of basic_expr * basic_expr
  | CheckArrayStore of basic_expr * basic_expr
  | CheckNegativeArraySize of basic_expr
  | CheckCast of basic_expr * object_type
  | CheckArithmetic of basic_expr

type instr =
  | Nop
  | AffectVar of var * expr
  | AffectArray of basic_expr * basic_expr * basic_expr
  | AffectField of basic_expr * class_name * field_signature * basic_expr
  | AffectStaticField of class_name * field_signature * expr
  | Goto of int
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * basic_expr * basic_expr ) * int
  | Throw of basic_expr
  | Return of basic_expr option
  | New of var * class_name * value_type list * (basic_expr list)
      (* var :=  class (parameters) *)
  | NewArray of var * value_type * (basic_expr list)
      (* var :=  value_type[e1]...[e2] *) 
  | InvokeStatic of var option * class_name * method_signature * basic_expr list
  | InvokeVirtual of var option * basic_expr * virtual_call_kind * method_signature * basic_expr list
  | InvokeNonVirtual
      of var option * basic_expr * class_name * method_signature * basic_expr list
  | MonitorEnter of basic_expr
  | MonitorExit of basic_expr 
  | MayInit of class_name
  | Check of check 

let bcvar = Bir.bcvar

exception Bad_Multiarray_dimension = Bir.Bad_Multiarray_dimension 
exception Bad_stack = Bir.Bad_stack
exception Subroutine = Bir.Subroutine
exception Content_constraint_on_Uninit = Bir.Content_constraint_on_Uninit
exception Type_constraint_on_Uninit = Bir.Type_constraint_on_Uninit
exception NonemptyStack_backward_jump = Bir.NonemptyStack_backward_jump
exception Uninit_is_not_expr = Bir.Uninit_is_not_expr

let expr2var expr = 
  match expr with 
    | Bir.Var (t,v) -> Var (t,v)
    | Bir.Const i -> Const i
    | _ -> assert false

let bir2a3bir_binop = function
  | Bir.ArrayLoad t -> ArrayLoad t
  | Bir.Add t -> Add t
  | Bir.Sub t -> Sub t
  | Bir.Mult t -> Mult t
  | Bir.Div t -> Div t
  | Bir.Rem t -> Rem t
  | Bir.IShl -> IShl
  | Bir.IShr -> IShr
  | Bir.LShl -> LShl
  | Bir.LShr -> LShr
  | Bir.IAnd -> IAnd
  | Bir.IOr -> IOr
  | Bir.IXor -> IXor
  | Bir.IUshr -> IUshr
  | Bir.LAnd -> LAnd
  | Bir.LOr -> LOr
  | Bir.LXor -> LXor
  | Bir.LUshr -> LUshr
  | Bir.CMP c -> CMP c



let bir2a3bir_basic_expr e = match e with 
  | Bir.Const c -> Const c
  | Bir.Var (t,v) -> Var (t,v)
  | _ -> Printf.printf "%s\n" (Bir.print_expr false e) ; assert false

let rec bir2a3bir_expr e = match e with 
  | Bir.Unop (unop, expr) -> Unop(unop,bir2a3bir_basic_expr expr)
  | Bir.Binop(binop,expr1,expr2) ->  Binop(bir2a3bir_binop binop,bir2a3bir_basic_expr expr1,bir2a3bir_basic_expr expr2) 
  | Bir.Field(expr,cn,fs) -> Field (expr2var expr, cn, fs)
  | Bir.StaticField(cn,fs) -> StaticField(cn,fs)
  | _ -> assert false 

let kind2kind = function 
  | Bir.VirtualCall objt -> VirtualCall objt 
  | Bir.InterfaceCall cn -> InterfaceCall cn

let check2check = function 
  | Bir.CheckNullPointer e -> CheckNullPointer (bir2a3bir_basic_expr e)
  | Bir.CheckArrayBound (e1, e2) -> CheckArrayBound (bir2a3bir_basic_expr e1, bir2a3bir_basic_expr e2)
  | Bir.CheckArrayStore (e1,e2) -> CheckArrayStore (bir2a3bir_basic_expr e1,  bir2a3bir_basic_expr e2)
  | Bir.CheckNegativeArraySize e -> CheckNegativeArraySize (bir2a3bir_basic_expr e) 
  | Bir.CheckCast (e,t) -> CheckCast (bir2a3bir_basic_expr e,t)
  | Bir.CheckArithmetic e -> CheckArithmetic (bir2a3bir_basic_expr e)
      
  
let bir2a3bir_instr = function
    Bir.Nop -> Nop
  | Bir.AffectVar (v,expr) -> AffectVar (v,bir2a3bir_expr expr)
  | Bir.AffectArray(e1,e2,e3) -> AffectArray(expr2var e1, bir2a3bir_basic_expr e2, bir2a3bir_basic_expr e3)
  | Bir.AffectField(e1,cn,fs,e2) -> AffectField(expr2var e1,cn,fs,bir2a3bir_basic_expr e2) 
  | Bir.AffectStaticField(cn,fs,e) -> AffectStaticField(cn,fs,bir2a3bir_expr e)
  | Bir.Goto i -> Goto i
  | Bir.Ifd ((cmp,e1,e2),i) -> Ifd ((cmp, bir2a3bir_basic_expr e1,bir2a3bir_basic_expr e2),i)
  | Bir.Throw e -> Throw (bir2a3bir_basic_expr e)
  | Bir.Return (Some e) -> Return (Some (bir2a3bir_basic_expr e))
  | Bir.Return None -> Return None
  | Bir.New(v,cn,vtl,el) -> New (v,cn,vtl,List.map bir2a3bir_basic_expr el)
  | Bir.NewArray(v,vt,el) -> NewArray(v,vt,List.map bir2a3bir_basic_expr el)
  | Bir.InvokeStatic(v,cn,ms,el) -> InvokeStatic(v,cn,ms,List.map bir2a3bir_basic_expr el) 
  | Bir.InvokeVirtual(optv,expr, kind, ms, el) ->InvokeVirtual(optv, expr2var expr, kind2kind kind, ms, List.map bir2a3bir_basic_expr el)
  | Bir.InvokeNonVirtual(optv, e, cn, ms, el) -> InvokeNonVirtual(optv,expr2var  e, cn, ms, List.map bir2a3bir_basic_expr el) 
  | Bir.MonitorEnter e -> MonitorEnter (bir2a3bir_basic_expr e)
  | Bir.MonitorExit e ->  MonitorExit (bir2a3bir_basic_expr e)
  | Bir.MayInit cn -> MayInit cn
  | Bir.Check c -> Check (check2check c)
      
type t = {
  a3_params : (value_type * var) list; 
  a3_code : (int * instr list) list; 
  a3_exc_tbl : JCode.exception_handler list;
  a3_line_number_table : (int * int) list option;
}


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

let rec print_basic_expr = function 
  | Var (_,x) -> Bir.var_name_g x  
  | Const i -> print_const i

and print_expr first_level = function
  | BasicExpr e -> print_basic_expr e
  | Field (v,c,f) -> Printf.sprintf "%s.%s" (print_basic_expr v) (print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_basic_expr e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_basic_expr  e)
  | Binop (ArrayLoad _,e1,e2) -> Printf.sprintf "%s[%s]" (print_basic_expr  e1) (print_basic_expr e2) 
  | Binop (Add _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s+%s" (print_basic_expr  e1) (print_basic_expr  e2))
  | Binop (Sub _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s-%s" (print_basic_expr  e1) (print_basic_expr e2))
  | Binop (Mult _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s*%s" (print_basic_expr  e1) (print_basic_expr e2))
  | Binop (Div _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s/%s" (print_basic_expr  e1) (print_basic_expr  e2))
  | Binop (op,e1,e2) -> Printf.sprintf "%s(%s,%s)" (print_binop op) (print_basic_expr  e1) (print_basic_expr e2) 

let print_cmp  (c,e1,e2) =
  match c with
    | `Eq -> Printf.sprintf "%s == %s" (print_basic_expr e1) (print_basic_expr  e2)
    | `Ne -> Printf.sprintf "%s != %s" (print_basic_expr e1) (print_basic_expr e2)
    | `Lt -> Printf.sprintf "%s < %s" (print_basic_expr e1) (print_basic_expr  e2)
    | `Ge -> Printf.sprintf "%s >= %s" (print_basic_expr e1) (print_basic_expr e2)
    | `Gt -> Printf.sprintf "%s > %s" (print_basic_expr e1) (print_basic_expr  e2)
    | `Le -> Printf.sprintf "%s <= %s" (print_basic_expr  e1) (print_basic_expr  e2)


let print_instr = function
  | Nop -> "nop"
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr true  e)
  | AffectField (v,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_basic_expr v) (print_field c f) (print_basic_expr e2)
  | AffectArray (v,e2,e3) -> Printf.sprintf "%s[%s] := %s"  (print_basic_expr v) (print_basic_expr  e2) (print_basic_expr e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | Throw e -> Printf.sprintf "throw %s" (print_basic_expr  e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_basic_expr e)
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (Bir.print_list_sep "," (print_basic_expr) le) 
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (Bir.print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_basic_expr  e)) le) 
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s)" (JPrint.class_name c) (ms_name ms) (Bir.print_list_sep "," (print_basic_expr) le) 
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s)" (var_name_g x) (JPrint.class_name c) (ms_name ms) (Bir.print_list_sep "," (print_basic_expr) le) 
  | InvokeVirtual (r,x,_,ms,le) -> 
      Printf.sprintf "%s%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_basic_expr x) (ms_name ms) (Bir.print_list_sep "," print_basic_expr le) 
  | InvokeNonVirtual (r,x,kd,ms,le) -> 
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_basic_expr x) (JPrint.class_name kd) (ms_name ms) (Bir.print_list_sep "," print_basic_expr le) 
  | MonitorEnter e -> Printf.sprintf "monitorenter(%s)" (print_basic_expr e)
  | MonitorExit e -> Printf.sprintf "monitorexit(%s)" (print_basic_expr e)
  | MayInit c -> Printf.sprintf "mayinit %s" (JPrint.class_name c)
  | Check c ->
      begin
	match c with 
	    CheckNullPointer e -> Printf.sprintf "notnull %s" (print_basic_expr  e)
	  | CheckArrayBound (a,i) -> Printf.sprintf "checkbound %s[%s]"  (print_basic_expr  a) (print_basic_expr  i)
	  | CheckArrayStore (a,v) -> Printf.sprintf "checkstore %s[] <- %s"  (print_basic_expr  a) (print_basic_expr  v)
	  | CheckNegativeArraySize e -> Printf.sprintf "checknegsize %s" (print_basic_expr  e)
	  | CheckCast (e,t) -> Printf.sprintf "checkcast %s:%s" (print_basic_expr  e) (JDumpBasics.object_value_signature t)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_basic_expr e)
      end

let rec print_instrs (pc,instrs) =
  Printf.sprintf "%3d: %s\n" pc
    (Bir.print_list_sep "\n     " print_instr instrs)

let rec print_code_intra = function
  | [] -> []
  | (pc,instrs)::q -> ( Printf.sprintf "%3d: %s\n" pc (Bir.print_list_sep "\n     " print_instr instrs))::(print_code_intra q)


let print_fbir_intra m = 
  print_code_intra m.a3_code

let rec print_code = function
  | [] -> []
  | (pc,instrs)::q -> 
      let strl = (Bir.print_list_sep_list "     " print_instr instrs) in 
      let first = 
	match strl with 
	  | [] -> [(Printf.sprintf "%3d: " pc )]
	  | s::m -> (Printf.sprintf "%3d: %s" pc ) s :: m
      in
	first@(print_code q)

let print m = print_code m.a3_code

let bir2a3bir  bir = 
  { a3_params = bir.Bir.params ;
    a3_code = List.map (fun (i,instrl) -> (i, List.map bir2a3bir_instr instrl)) bir.Bir.code  ;
    a3_exc_tbl = bir.Bir.exc_tbl ;
    a3_line_number_table = bir.Bir.line_number_table ;
  }

(** Concrete method transformation. *) 
let transform ?(compress=false) j_m j_code =
  let code = Bir.transform_addr3 ~compress:compress j_m j_code in 
    bir2a3bir code
      
      
  
