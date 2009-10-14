open JBasics
open Javalib
open JCode
open Bir

include Cmn

type expr =
  | Const of const
  | Var of var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of var * class_name * field_signature
  | StaticField of class_name * field_signature
	  
type virtual_call_kind =
  | VirtualCall of object_type
  | InterfaceCall of class_name

type check = 
  | CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr
  | CheckArithmetic of expr

type instr =
  | Nop
  | AffectVar of var * expr
  | AffectArray of var * expr * expr
  | AffectField of var * class_name * field_signature * expr
  | AffectStaticField of class_name * field_signature * expr
  | Goto of int
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr ) * int
  | Throw of expr
  | Return of expr option
  | New of var * class_name * value_type list * (expr list)
      (* var :=  class (parameters) *)
  | NewArray of var * value_type * (expr list)
      (* var :=  value_type[e1]...[e2] *) 
  | InvokeStatic 
      of var option * class_name * method_signature * expr list
  | InvokeVirtual
      of var option * var * virtual_call_kind * method_signature * expr list
  | InvokeNonVirtual
      of var option * var * class_name * method_signature * expr list
  | MonitorEnter of expr
  | MonitorExit of expr 
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
    | Bir.Var v -> v
    | _ -> assert false

let rec bir2fbir_expr e = match e with 
  | Bir.Const c -> Const c
  | Bir.Var v -> Var v
  | Bir.Unop (unop, expr) -> Unop(unop, bir2fbir_expr expr)
  | Bir.Binop(binop,expr1,expr2) ->  Binop(binop,bir2fbir_expr expr1,bir2fbir_expr expr2) 
  | Bir.Field(expr,cn,fs) -> Field (expr2var expr, cn, fs)
  | Bir.StaticField(cn,fs) -> StaticField(cn,fs)

let kind2kind = function 
  | Bir.VirtualCall objt -> VirtualCall objt 
  | Bir.InterfaceCall cn -> InterfaceCall cn

let check2check = function 
  | Bir.CheckNullPointer e -> CheckNullPointer (bir2fbir_expr e)
  | Bir.CheckArrayBound (e1, e2) -> CheckArrayBound (bir2fbir_expr e1, bir2fbir_expr e2)
  | Bir.CheckArrayStore (e1,e2) -> CheckArrayStore (bir2fbir_expr e1,  bir2fbir_expr e2)
  | Bir.CheckNegativeArraySize e -> CheckNegativeArraySize (bir2fbir_expr e) 
  | Bir.CheckCast e -> CheckCast (bir2fbir_expr e)
  | Bir.CheckArithmetic e -> CheckArithmetic (bir2fbir_expr e)
      
  
let bir2fbir_instr = function
    Bir.Nop -> Nop
  | Bir.AffectVar (v,expr) -> AffectVar (v,bir2fbir_expr expr)
  | Bir.AffectArray(e1,e2,e3) -> AffectArray(expr2var e1, bir2fbir_expr e2, bir2fbir_expr e3)
  | Bir.AffectField(e1,cn,fs,e2) -> AffectField(expr2var e1,cn,fs,bir2fbir_expr e2) 
  | Bir.AffectStaticField(cn,fs,e) -> AffectStaticField(cn,fs,bir2fbir_expr e)
  | Bir.Goto i -> Goto i
  | Bir.Ifd ((cmp,e1,e2),i) -> Ifd ((cmp,bir2fbir_expr e1,bir2fbir_expr e2),i)
  | Bir.Throw e -> Throw (bir2fbir_expr e)
  | Bir.Return (Some e) -> Return (Some (bir2fbir_expr e))
  | Bir.Return None -> Return None
  | Bir.New(v,cn,vtl,el) -> New (v,cn,vtl,List.map bir2fbir_expr el)
  | Bir.NewArray(v,vt,el) -> NewArray(v,vt,List.map bir2fbir_expr el)
  | Bir.InvokeStatic(v,cn,ms,el) -> InvokeStatic(v,cn,ms,List.map bir2fbir_expr el)
  | Bir.InvokeVirtual(optv,expr, kind, ms, el) ->InvokeVirtual(optv, expr2var expr, kind2kind kind, ms, List.map bir2fbir_expr el)
  | Bir.InvokeNonVirtual(optv, e, cn, ms, el) -> InvokeNonVirtual(optv,expr2var  e, cn, ms, List.map bir2fbir_expr el)
  | Bir.MonitorEnter e -> MonitorEnter (bir2fbir_expr e)
  | Bir.MonitorExit e ->  MonitorExit (bir2fbir_expr e)
  | Bir.MayInit cn -> MayInit cn
  | Bir.Check c -> Check (check2check c)
      
type fbir = {
  f_params : var list; 
  f_code : (int * instr list) list; 
  f_exc_tbl : JCode.exception_handler list;
  f_line_number_table : (int * int) list option;
}



let print_field ?(long_fields=false) c f =
  if long_fields then
    Printf.sprintf "<%s:%s>" (JPrint.class_name c) (fs_name f)
  else (fs_name f)

let bracket b s =
  if b then s else Printf.sprintf "(%s)" s 

let rec print_expr first_level = function
  | Var x -> Bir.var_name_g x
  | Field (v,c,f) -> Printf.sprintf "%s.%s" (Bir.var_name_g v) (print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Const i -> print_const i
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_expr false e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_expr true e)
  | Binop (ArrayLoad,e1,e2) -> Printf.sprintf "%s[%s]" (print_expr false e1) (print_expr true e2) 
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
  | Nop -> "nop"
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr true e)
  | AffectField (v,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (var_name_g v) (print_field c f) (print_expr true e2)
  | AffectArray (v,e2,e3) -> Printf.sprintf "%s[%s] := %s"  (var_name_g v) (print_expr true e2) (print_expr true e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | Throw e -> Printf.sprintf "throw %s" (print_expr false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr false e)
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (print_list_sep "," (print_expr true) le) 
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_expr true e)) le) 
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s)" (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s)" (var_name_g x) (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | InvokeVirtual (r,x,_,ms,le) -> 
      Printf.sprintf "%s%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(var_name_g x) (ms_name ms) (print_list_sep "," (print_expr true) le) 
  | InvokeNonVirtual (r,x,kd,ms,le) -> 
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(var_name_g x) (JPrint.class_name kd) (ms_name ms) (print_list_sep "," (print_expr true) le) 
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

let rec print_instrs (pc,instrs) =
  Printf.sprintf "%3d: %s\n" pc
    (print_list_sep "\n     " print_instr instrs)

let rec print_code_intra = function
  | [] -> []
  | (pc,instrs)::q -> ( Printf.sprintf "%3d: %s\n" pc (print_list_sep "\n     " print_instr instrs))::(print_code_intra q)

let print_fbir_intra m = 
  print_code_intra m.f_code

let rec print_code = function
  | [] -> []
  | (pc,instrs)::q -> 
      let strl = (print_list_sep_list "     " print_instr instrs) in 
      let first = 
	match strl with 
	  | [] -> [(Printf.sprintf "%3d: " pc )]
	  | s::m -> (Printf.sprintf "%3d: %s" pc ) s :: m
      in
	first@(print_code q)

let print_fbir m = print_code m.f_code

let bir2fbir cm = 
  { f_params = cm.params ;
    f_code = List.map (fun (i,instrl) -> (i, List.map bir2fbir_instr instrl)) cm.code  ;
    f_exc_tbl = cm.exc_tbl ;
    f_line_number_table = cm.line_number_table ;
  }

(** Concrete method transformation. *) 
let cm_transform m =
  let bir_m = Bir.cm_transform_flat m in
    Javalib.map_concrete_method bir2fbir bir_m
      
(** [interface_or_class] transformation *) 
let iorc_transform iorc =  
  let bir_iorc = Bir.iorc_transform_flat iorc in
    Javalib.map_interface_or_class bir2fbir bir_iorc

(** transform the [interface_or_class] corresponding to the class_path string.
    ex: [cn_transform "dir/dir2/Test.class"] 
    cn_transform
 *) 
let cn_transform name =  
  let bir_iorc = Bir.cn_transform_flat name in
    Javalib.map_interface_or_class bir2fbir bir_iorc
      
  
