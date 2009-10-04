open JBasics
open Javalib
open JCode

include Cmn

(*********** TYPES *************)

type expr =
  | Const of const
  | Var of var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * class_name * field_signature
  | StaticField of class_name * field_signature
	  
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
  | CheckCast of expr
  | CheckArithmetic of expr

type instr =
  | Nop
  | AffectVar of var * expr
  | AffectArray of expr * expr * expr
  | AffectField of expr * class_name * field_signature * expr
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
      of var option * expr * virtual_call_kind * method_signature * expr list
  | InvokeNonVirtual
      of var option * expr * class_name * method_signature * expr list
  | MonitorEnter of expr
  | MonitorExit of expr
  | MayInit of class_name
  | Check of check 

type bir = {
  params : var list;
  code : (int * instr list) list;
  exc_tbl : exception_handler list;
  line_number_table : (int * int) list option;
}
(* For stack type inference only *)
type op_size = Op32 | Op64


(************* PRINT ************)      

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

let rec last = function
  | [] -> raise (JBasics.No_class_found "") (* non emty class identifier *)
  | [a] -> a
  | _::q -> last q

let print_field ?(long_fields=false) c f =
  if long_fields then
    Printf.sprintf "<%s:%s>" (JPrint.class_name c) (fs_name f)
  else (fs_name f)

let bracket b s =
  if b then s else Printf.sprintf "(%s)" s 

let rec print_expr first_level = function
  | Var x -> var_name_g x
  | Field (e,c,f) -> Printf.sprintf "%s.%s" (print_expr false e) (print_field c f)
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

let print_oexpr = function
  | Uninit (c,i) -> Printf.sprintf "Unit(%d,%s)" i (JPrint.class_name c)
  | E e -> print_expr true e

let print_stackmap = function
  | [] -> ""
  | x::q -> List.fold_left (fun s t -> Printf.sprintf "%s :: %s" (print_oexpr t) s) (print_oexpr x) q

let print_instr = function
  | Nop -> "nop"
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr true e)
  | AffectField (e1,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_expr false e1) (print_field c f) (print_expr true e2)
  | AffectArray (e1,e2,e3) -> Printf.sprintf "%s[%s] := %s" (print_expr false e1) (print_expr true e2) (print_expr true e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | Throw e -> Printf.sprintf "throw %s" (print_expr false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr false e)
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

let rec print_instrs (pc,instrs) =
  Printf.sprintf "%3d: %s\n" pc
    (print_list_sep "\n     " print_instr instrs)

let rec print_code_intra = function
  | [] -> []
  | (pc,instrs)::q -> ( Printf.sprintf "%3d: %s\n" pc (print_list_sep "\n     " print_instr instrs))::(print_code_intra q)

let print_bir_intra m = 
  print_code_intra m.code

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

let print_bir m = print_code m.code

(******* STACK MANIPULATION **************)

exception Bad_stack

(* Returns the top element of the stack *)
let top = function
  | [] -> raise Bad_stack
  | x::_ -> x

(* Pops one element off the stack *)
let pop = function
  | [] -> raise Bad_stack
  | _::q -> q

(* Pops n elements off the stack *)
let rec popn n s =
  if n=0 then s
  else pop (popn (n-1) s)

(* Pops 2, 3 elements off the stack *)
let pop2 s = popn 2 s
let pop3 s = popn 3 s


(* Returns a list of n top elements of the stack, in the reverse order *)
let rec param_acc n l acc =
  if n<=0 then acc
  else
    match l with
      | [] -> raise Bad_stack
      | x::q -> 
	  param_acc (n-1) q (x::acc)
let param n l = param_acc n l []

exception Uninit_is_not_expr

let topE = function
  | [] -> raise Bad_stack
  | (Uninit _)::_ -> raise Uninit_is_not_expr 
  | (E e)::_ -> e
let getE = function
  | Uninit _ -> raise Uninit_is_not_expr
  | E e -> e

let param n l = List.map getE (param n l)

(**************** STACK TYPE INFERENCE ****)

exception Subroutine

let convert_type = function
  | `Int | `Short| `Char
  | `Byte  | `Int2Bool  | `ByteBool
  | `Bool   | `Float   | `Object -> Op32
  | `Long  | `Double -> Op64
      
let convert_const = function
  | `String _   | `Class _   | `ANull 
  | `Byte _  | `Short _  | `Float _
  | `Int _ -> Op32
  | `Long _   | `Double _ -> Op64

let rec convert_field_type = function
  | TBasic t -> convert_type t
  | TObject t -> convert_object_type t
and convert_object_type = function
  | TClass _ -> Op32
  | TArray _ -> Op32

let type_next = function
  | OpNop -> (function s -> s)
  | OpConst x -> (function s -> (convert_const x)::s)
  | OpLoad (k,_) -> (function s -> (convert_type k)::s)
  | OpArrayLoad k -> (function s -> (convert_type k)::(pop2 s))
  | OpStore (_,_) -> (function s -> pop s)
  | OpArrayStore _ -> pop3
  | OpPop -> pop
  | OpPop2 -> (function s ->
		 match (top s) with
		   | Op32 -> pop2 s
		   | Op64 -> pop s)
  | OpDup -> (function s -> (top s)::s)
  | OpDupX1 -> (function s -> (top s)::(top (pop s))::(top s)::(pop2 s))
  | OpDupX2 -> (function s -> 
     (match (top (pop s)) with
	| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s)
	| Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s)))
  | OpDup2 -> (function s ->
      (match (top s) with
	 | Op32 -> (top s)::(top (pop s))::(top s)::(top (pop s))::(pop2 s)
	 | Op64 -> (top s)::s))
  | OpDup2X1 -> 
      (function s -> match (top s) with
	 | Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s)
	 | Op64 ->  (top s)::(top (pop s))::(top s)::(pop2 s))
  | OpDup2X2 -> 
      (function s -> match (top s) with
	 | Op32 ->
	     (match (top (pop2 s)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top (pop3 s))::(top s)::(top (pop s))::(pop (pop3 s))
		| Op64 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s))
	 | Op64 ->
	     (match (top (pop s)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s)
		| Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s)))
  | OpSwap -> (function s -> (top (pop s))::(top s)::(pop2 s))
  | OpAdd k 
  | OpSub k 
  | OpMult k 
  | OpDiv k 
  | OpRem k  -> (function s -> (convert_type k)::(pop2 s))
  | OpNeg k -> (function s -> (convert_type k)::(pop s))
  | OpIShl
  | OpIShr
  | OpIAnd
  | OpIOr 
  | OpIXor
  | OpIUShr -> (function s ->  Op32::(pop2 s))
  | OpLShr
  | OpLShl -> (function s ->  pop s)
  | OpLAnd
  | OpLOr 
  | OpLXor
  | OpLUShr -> (function s -> Op64::(pop2 s))
  | OpIInc (_,_) -> (function s -> s)
  | OpI2L -> (function s -> Op64::(pop s))
  | OpI2F -> (function s -> Op32::(pop s))
  | OpI2D -> (function s -> Op64::(pop s))
  | OpL2I -> (function s -> Op32::(pop s))
  | OpL2F -> (function s -> Op32::(pop s))
  | OpL2D -> (function s -> Op64::(pop s))
  | OpF2I -> (function s -> Op32::(pop s))
  | OpF2L -> (function s -> Op64::(pop s))
  | OpF2D -> (function s -> Op64::(pop s))
  | OpD2I -> (function s -> Op32::(pop s))
  | OpD2L -> (function s -> Op64::(pop s))
  | OpD2F -> (function s -> Op32::(pop s))
  | OpI2B -> (function s -> s)
  | OpI2C -> (function s -> s)
  | OpI2S -> (function s -> s)
  | OpCmp _ -> (function s -> Op32::(pop2 s))
  | OpIf (_, _) -> pop
  | OpIfCmp (_, _) -> pop2
  | OpGoto _ -> (function s -> s)
  | OpJsr _ -> raise Subroutine
  | OpRet _ -> raise Subroutine
  | OpTableSwitch _ -> pop
  | OpLookupSwitch _ -> pop
  | OpReturn _ -> (function _ -> [])
  | OpGetField (_, fs) ->  (function s -> (convert_field_type (fs_type fs))::(pop s))
  | OpGetStatic (_, fs) -> (function s -> (convert_field_type (fs_type fs))::s)
  | OpPutStatic _ -> pop
  | OpPutField _ -> pop2
  | OpInvoke (x, ms) -> (function s ->
      let s =
      (match x with
	 | `Static _ -> popn (List.length (ms_args ms)) s
	 | _ -> popn (List.length (ms_args ms)) (pop s) ) in
	(match ms_rtype ms with
	   | None -> s
	   | Some t -> (convert_field_type t)::s))

  | OpNew _ -> (function s -> Op32::s)
  | OpNewArray _ -> (function s -> Op32::(pop s))
  | OpArrayLength -> (function s -> Op32::(pop s))
  | OpThrow -> (function _ -> [])
  | OpCheckCast _ -> (function s -> s)
  | OpInstanceOf _-> (function s -> Op32::(pop s))
  | OpMonitorEnter -> pop
  | OpMonitorExit -> pop
  | OpAMultiNewArray (_,b) -> (function s -> Op32::(popn b s))
  | OpBreakpoint -> failwith "breakpoint"  
  | OpInvalid -> failwith "invalid"


(**************** GENERATION *************)

exception Bad_Multiarray_dimension

(* fetches the element value type *)
let rec remove_dim t n =
  (* only used for multidim arrays, hence not of dimension 0 *)
  if (n <= 0) then raise Bad_Multiarray_dimension ;
  let rec aux t n =
    match t with
      | TClass _ -> raise Bad_Multiarray_dimension (* bad dimension *)
      | TArray t -> 
	  if n=1 then t
	  else match t with
	    | TBasic _ -> raise Bad_Multiarray_dimension (* bad dimension *)
	    | TObject t -> aux t (n-1) 
  in aux t n


let is_in_expr test_var test_static test_field test_array =
  let rec aux expr =
    match expr with 
      | Const _ -> false
      | StaticField (c,f) -> test_static c f
      | Field (e,c,f) -> test_field c f || aux e
      | Var x -> test_var x
      | Unop(_,e) -> aux e
      | Binop(s,e1,e2) -> (test_array && s = ArrayLoad) || aux e1 || aux e2
  in aux

let replace_in_expr test_var test_static expr0 =
  let rec aux expr =
    match expr with 
      | Const _ -> expr
      | StaticField (c,f) -> if test_static c f then expr0 else expr
      | Field (e,c,f) -> Field (aux e,c,f)
      | Var x -> if test_var x then expr0 else expr
      | Unop (s,e) -> Unop (s,aux e)
      | Binop (s,e1,e2) -> Binop (s,aux e1,aux e2)
  in aux

let is_in_stack test_var test_static =
  let rec aux stack =
    match stack with 
      | [] -> false
      | (E e)::_ when is_in_expr test_var test_static (fun _ _ -> false) false e -> true
      | _::s -> aux s
  in aux

let replace_in_stack test_var test_static expr0 =
  let rec aux stack =
    match stack with 
      | [] -> []
      | e'::s -> begin
	  match e' with
	    | E e -> E (replace_in_expr test_var test_static expr0 e)::(aux s)
	    | Uninit _ -> e'::(aux s)
	end
  in aux

let is_var_in_stack x stack =
  is_in_stack (var_equal x) (fun _ _ -> false) stack

let is_static_in_stack c f stack =
  is_in_stack (fun _ -> false) (fun c0 f0 -> c=c0 && f=f0) stack

let is_field_in_expr c f expr =
  is_in_expr (fun _ -> false) (fun _ _ -> false) (fun c0 f0 -> c=c0 && f=f0) false expr

let is_array_in_expr =
  is_in_expr (fun _ -> false) (fun _ _ -> false)  (fun _ _ -> false) true

let is_var_in_expr_not_var x e = (* warning we use ocaml equality *)
  is_in_expr (var_equal x) (fun _ _ -> false)  (fun _ _ -> false) true e

let is_heap_sensible_element_in_expr expr =
  is_in_expr (fun _ -> false) (fun _ _ -> true) (fun _ _ -> true) true expr

let replace_var_in_expr x t =
  replace_in_expr (var_equal x) (fun _ _ -> false) (Var t)

let replace_var_in_stack x t stack =
  replace_in_stack (var_equal x) (fun _ _ -> false) (Var t) stack

let replace_static_in_stack c f t stack =
  replace_in_stack (fun _ -> false) (fun c0 f0 -> c=c0 && f=f0) (Var t) stack

let test_expr_in_instr f = function
  | AffectVar (_,Var _) 
  | Nop 
  | Return None
  | MayInit _
  | Goto _ -> false  
  | Throw e
  | AffectVar (_,e) 
  | Return (Some e)
  | MonitorEnter e
  | MonitorExit e 
  | Check (CheckNegativeArraySize e)
  | Check (CheckCast e)
  | Check (CheckArithmetic e)
  | Check (CheckNullPointer e)
  | AffectStaticField (_,_,e) -> f e
  | Ifd ((_,e1,e2), _)
  | Check (CheckArrayBound (e1,e2))
  | Check (CheckArrayStore (e1,e2))
  | AffectField (e1,_,_,e2) ->  f e1 || f e2
  | AffectArray (e1,e2,e3) -> f e1 || f e2 || f e3
  | NewArray (_,_,le)
  | InvokeStatic (_,_,_,le)
  | New (_,_,_,le) -> List.exists f le
  | InvokeVirtual (_,e,_,_,le)
  | InvokeNonVirtual (_,e,_,_,le) -> f e || List.exists f le 


	
let is_var_in_expr_instr_not_var x =
  test_expr_in_instr (is_var_in_expr_not_var x)

let is_var_in_expr_bir_not_var x bir =
  List.exists
    (fun (_,instrs) -> List.exists (is_var_in_expr_instr_not_var x) instrs)
    bir

let clean count1 count2 i test s instrs =
  let rec aux j c1 c2 = function
    | [] -> [], instrs, false, c1, c2
    | e::s -> 
	let (s,instrs,test_succeed,cc1,cc2) = aux (j+1) c1 c2 s in
	  match e with
	    | Uninit _ -> e::s, instrs, test_succeed, cc1, cc2
	    | E e ->
		if test e then begin
		  let x = TempVar (i,Some j) in
		    (E (Var x)::s, (AffectVar (x,e))::instrs, true, cc1,cc2+1)
		end else
		  E e::s, instrs, test_succeed, cc1,cc2
  in
  let (s,instrs,test_succeed,c1,c2) = aux 0 count1 count2 s in
    if test_succeed then (s,instrs,c1+1,c2)
    else (s,instrs,c1,c2)


let add_tempvars stats x = 
  match stats with 
      Some s ->  s.tempvars <- x :: s.tempvars 
    | None -> ()

let incr_stats stats a = 
  match stats with 
    | None -> ()
    | Some s ->
	match a with 
	  |  `Nb_jump_with_non_empty_stacks -> s.nb_jump_with_non_empty_stacks <- s.nb_jump_with_non_empty_stacks + 1
	  |  `Nb_back_jump_with_non_empty_stacks -> 
	       s.nb_back_jump_with_non_empty_stacks <- s.nb_back_jump_with_non_empty_stacks + 1
	  |  `Nb_store_is_var_in_stack -> s.nb_store_is_var_in_stack <- s.nb_store_is_var_in_stack + 1
	  |  `Nb_incr_is_var_in_stack -> s.nb_incr_is_var_in_stack <- s.nb_incr_is_var_in_stack + 1
	  |  `Nb_putfield_is_field_in_stack -> s.nb_putfield_is_field_in_stack <- s.nb_putfield_is_field_in_stack + 1
	  |  `Nb_arraystore_is_array_access_in_stack ->
	       s.nb_arraystore_is_array_access_in_stack <- s.nb_arraystore_is_array_access_in_stack + 1
	  |  `Nb_putstatic_is_static_in_stack -> s.nb_putstatic_is_static_in_stack <- s.nb_putstatic_is_static_in_stack + 1
	  |  `Nb_method_call_with_modifiable_in_stack -> 
	       s.nb_method_call_with_modifiable_in_stack <- s.nb_method_call_with_modifiable_in_stack + 1
	  |  `Nb_store -> s.nb_store <- s.nb_store + 1
	  |  `Nb_incr -> s.nb_incr <- s.nb_incr + 1
	  |  `Nb_putfield -> s.nb_putfield <- s.nb_putfield +1
	  |  `Nb_arraystore -> s.nb_arraystore <- s.nb_arraystore + 1
	  |  `Nb_putstatic ->  s.nb_putstatic <-  s.nb_putstatic + 1
	  |  `Nb_method_call -> s.nb_method_call <- s.nb_method_call + 1
	  |  `Nb_tempvar -> s.nb_tempvar <- s.nb_tempvar +1
	  |  `Nb_tempvar_branch -> s.nb_tempvar_branch <- s.nb_tempvar_branch +1 
	  |  `Nb_tempvar_removed -> s.nb_tempvar_removed <- s.nb_tempvar_removed + 1
	  |  `Nb_tempvar_method_effect -> s.nb_tempvar_method_effect <- s.nb_tempvar_method_effect
	  |  `Nb_tempvar_putfield -> s.nb_tempvar_putfield <- s.nb_tempvar_putfield +1
	  |  `Nb_tempvar_arraystore -> s.nb_tempvar_arraystore <- s.nb_tempvar_arraystore + 1
	  |  `Nb_tempvar_side_effect -> s.nb_tempvar_side_effect <- s.nb_tempvar_side_effect + 1
	  |  `Nb_tempvar_flat -> s.nb_tempvar_flat <- s.nb_tempvar_flat + 1
	  |  `Nb_classes -> s.nb_classes <-  s.nb_classes + 1
	  |  `Nb_methods -> s.nb_methods <-  s.nb_methods + 1
	  |  `Nb_subroutines ->  s.nb_subroutines <- s.nb_subroutines + 1
	  | _ -> failwith "flat"
	  
(* Maps each opcode to a function of a stack that modifies its 
 * according to opcode, and returns grimp corresponding instructions if any 
 * tos : type operand stack
 * i : current index of bytecode
 * next : progression along instruction bytecode index 
 * stats : option statistics to compute
 * mode : normal (O), flat (1), 3add (2)
 *)
let bc2bir_instr flat pp_var i tos s stats = function
  | OpNop -> s, [], stats
  | OpConst x -> E (Const x)::s, [], stats
  | OpLoad (_,n) -> E (Var (OriginalVar (i,n,pp_var i n)))::s, [], stats
  | OpArrayLoad _ -> 
      let a = topE (pop s) in
      let i = topE s in
	E (Binop(ArrayLoad,a,i))::(pop2 s), 
      [Check (CheckNullPointer a);Check (CheckArrayBound (a,i))],
      stats
  | OpStore (_,n) ->  
      incr_stats stats `Nb_store ;
      let y = OriginalVar(i,n,pp_var i n) in
	if is_var_in_stack y (pop s)
	then begin
	  incr_stats stats `Nb_store_is_var_in_stack ;
	  incr_stats stats `Nb_tempvar ;
	  let x = TempVar (i,None) in
	   (* was missing *)
	    add_tempvars stats x ;
	    replace_var_in_stack y x (pop s), 
	  [AffectVar(y,topE s); AffectVar(x,Var y)],
	  stats
	end else
	   (pop s,[AffectVar (y,topE s)], stats) 
  | OpIInc (a,b) ->
      incr_stats stats `Nb_incr ; 
      let a = OriginalVar (i,a,pp_var i a) in
	if is_var_in_stack a s
	then begin
	  incr_stats stats `Nb_incr_is_var_in_stack ;
	  incr_stats stats `Nb_tempvar;
	  let x = TempVar (i,None) in
	    (* was missing *)
	    add_tempvars stats x ;
	    replace_var_in_stack a x s, 
	  [AffectVar(x,Var a);
	   AffectVar (a,Binop(Add `Int2Bool,Var a,Const (`Int (Int32.of_int b))))],
	  stats
	end else s,[AffectVar(a,Binop(Add `Int2Bool,Var a,Const (`Int (Int32.of_int b))))],stats 
  | OpPutField (c, f) -> 
      incr_stats stats `Nb_putfield ;
      let r = topE (pop s) in
      let c1, c2 = 
	(match stats with 
	     Some s -> s.nb_putfield_is_field_in_stack, s.nb_tempvar_putfield 
	   | None -> 0, 0 ) in 
      let (s,instrs,count,count') = clean c1 c2 i (is_field_in_expr c f) (pop2 s) [Check (CheckNullPointer r); AffectField (r,c,f,topE s)]
      in
	(match stats with
	     Some s -> 	(s.nb_putfield_is_field_in_stack <- count ;
			 s.nb_tempvar_putfield <- count' )
	   | None -> ());
	(s,instrs,stats)
  | OpArrayStore _ -> 
      incr_stats stats `Nb_arraystore ;
      let v = topE s in
      let a = topE (pop2 s) in	
      let idx = topE (pop s) in
      let c1, c2 = 
	(match stats with 
	     Some s -> s.nb_arraystore_is_array_access_in_stack, s.nb_tempvar_arraystore
	   | None -> 0, 0 ) in
      let inss = [Check (CheckNullPointer a); 
		  Check (CheckArrayBound (a,idx)); 
		  Check (CheckArrayStore (a,v)); 
		  AffectArray (a, idx, v)]
      in let (s,instrs,count,count') = clean  c1 c2 i is_array_in_expr (pop3 s) inss	
      in 
	(match stats with
	     Some s -> 	(s.nb_arraystore_is_array_access_in_stack <- count ;
			 s.nb_tempvar_arraystore <- count' )
	   | None -> ());
	(s,instrs,stats)	  
  | OpPop -> pop s, [],stats
  | OpPop2 -> 
      (match (top tos) with
	 | Op32 -> pop2 s, [],stats
	 | Op64 -> pop s, [],stats)
  | OpDup -> (top s)::s,[],stats
  | OpDupX1 -> (top s)::(top (pop s))::(top s)::(pop2 s), [],stats
  | OpDupX2 -> 
	 (match (top (pop tos)) with
	    | Op32 -> 
		(top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s),[],stats
	    | Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s),[],stats)
  | OpDup2 -> 
      (match (top tos) with
	 | Op32 -> (top s)::(top (pop s))::(top s)::(top (pop s))::(pop2 s),[],stats
	 | Op64 -> (top s)::s,[],stats)
  | OpDup2X1 -> 
      (match (top tos) with
	 | Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s),[],stats
	 | Op64 ->  (top s)::(top (pop s))::(top s)::(pop2 s),[],stats)
  | OpDup2X2 -> 
      (match (top tos) with
	 | Op32 ->
	     (match (top (pop2 tos)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top (pop3 s))::(top s)::(top (pop s))::(pop (pop3 s)),[],stats
		| Op64 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s),[],stats)
	 | Op64 ->
	     (match (top (pop tos)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s),[],stats
		| Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s),[],stats))
  | OpSwap -> (top (pop s))::(top s)::(pop2 s),[],stats
  | OpAdd k -> E (Binop (Add k,topE (pop s),topE s))::(pop2 s), [],stats
  | OpSub k -> E (Binop (Sub k,topE (pop s),topE s))::(pop2 s), [],stats
  | OpMult k -> E (Binop (Mult k,topE (pop s),topE s))::(pop2 s), [],stats
  | OpDiv k -> 
      let q = topE s in
	E (Binop (Div k,topE (pop s),q))::(pop2 s), [Check (CheckArithmetic q)],stats
  | OpRem k -> 
      let q = topE s in
	E (Binop (Rem k,topE (pop s),q))::(pop2 s), [Check (CheckArithmetic q)],stats
  | OpNeg k -> E (Unop (Neg k,topE s))::(pop s), [],stats
  | OpIShl -> E (Binop (IShl,topE (pop s),topE s))::(pop2 s), [],stats
  | OpLShl -> E (Binop (LShl,topE (pop s),topE s))::(pop2 s), [],stats
  | OpIShr -> E (Binop (IShr,topE (pop s),topE s))::(pop2 s), [],stats
  | OpLShr -> E (Binop (LShr,topE (pop s),topE s))::(pop2 s), [],stats
  | OpIAnd -> E (Binop (IAnd,topE (pop s),topE s))::(pop2 s), [],stats
  | OpIOr -> E (Binop (IOr,topE (pop s),topE s))::(pop2 s), [],stats
  | OpIXor -> E (Binop (IXor,topE (pop s),topE s))::(pop2 s), [],stats
  | OpIUShr -> E (Binop (IUshr,topE (pop s),topE s))::(pop2 s), [],stats
  | OpLAnd -> E (Binop (LAnd,topE (pop s),topE s))::(pop2 s), [],stats
  | OpLOr -> E (Binop (LOr,topE (pop s),topE s))::(pop2 s), [],stats
  | OpLXor -> E (Binop (LXor,topE (pop s),topE s))::(pop2 s), [],stats
  | OpLUShr  -> E (Binop (LUshr,topE (pop s),topE s))::(pop2 s), [],stats
  | OpI2L -> E (Unop (Conv I2L,topE s))::(pop s), [],stats
  | OpI2F -> E (Unop (Conv I2F,topE s))::(pop s), [],stats
  | OpI2D -> E (Unop (Conv I2D,topE s))::(pop s), [],stats
  | OpL2I -> E (Unop (Conv L2I,topE s))::(pop s), [],stats
  | OpL2F -> E (Unop (Conv L2F,topE s))::(pop s), [],stats
  | OpL2D -> E (Unop (Conv L2D,topE s))::(pop s), [],stats
  | OpF2I -> E (Unop (Conv F2I,topE s))::(pop s), [],stats
  | OpF2L -> E (Unop (Conv F2L,topE s))::(pop s), [],stats
  | OpF2D -> E (Unop (Conv F2D,topE s))::(pop s), [],stats
  | OpD2I -> E (Unop (Conv D2I,topE s))::(pop s), [],stats
  | OpD2L -> E (Unop (Conv D2L,topE s))::(pop s), [],stats
  | OpD2F -> E (Unop (Conv D2F,topE s))::(pop s), [],stats
  | OpI2B -> E (Unop (Conv I2B,topE s))::(pop s), [],stats
  | OpI2C -> E (Unop (Conv I2C,topE s))::(pop s), [],stats
  | OpI2S -> E (Unop (Conv I2S,topE s))::(pop s), [],stats
  | OpCmp op -> 
      (      match op with 
	| `DG -> E (Binop (CMP DG,topE (pop s),topE s))::(pop2 s), [],stats
	| `DL -> E (Binop (CMP DL,topE (pop s),topE s))::(pop2 s), [],stats
	| `FG -> E (Binop (CMP FG,topE (pop s),topE s))::(pop2 s), [],stats
	| `FL -> E (Binop (CMP FL,topE (pop s),topE s))::(pop2 s), [],stats 
	| `L -> E (Binop (CMP L,topE (pop s),topE s))::(pop2 s), [],stats
      )
  | OpIf ( x , n) ->  
      let guard = 
	match x with
	  | `NonNull -> (`Ne,topE s, Const `ANull) 
	  | `Null ->  (`Eq,topE s, Const `ANull)
	  | `Eq | `Ge | `Gt | `Le | `Lt | `Ne as c   -> 
	      begin
		match topE s with
		  | Binop (CMP _,e1,e2) -> (c,e1,e2)
		  | e -> (c,e, Const (`Int (Int32.of_int 0)))
	      end
       in
      let target = i+n in
	pop s, [Ifd (guard,target)],stats
  | OpIfCmp (x, n) -> 
      let x = match x with
	  `AEq | `IEq -> `Eq
	| `ANe | `INe -> `Ne
	| `IGe -> `Ge
	| `IGt -> `Gt
	| `ILe -> `Le
	| `ILt -> `Lt in
      let guard = (x, topE (pop s), topE s) in
      let target = i+n in
	pop2 s, [Ifd (guard,target)],stats
  | OpGoto n -> s, [Goto (n+i)],stats
  | OpJsr _ -> raise Subroutine
  | OpRet _ -> raise Subroutine
  | OpTableSwitch (def,min,_,tbl) -> 
      pop s, 
      (Array.fold_right
		  (fun (guard,i) l -> (Ifd (guard,i))::l)
		  (Array.mapi 
		     (fun idx j -> 
			(`Eq,
			 topE s,
			 Const (`Int (Int32.add (Int32.of_int idx) min))),
			j+i) tbl)
		  [Goto (def+i)]),
      stats
  | OpLookupSwitch (def,pairs) -> 
      pop s, 
      (List.fold_right
		  (fun (c,j) l -> (Ifd ((`Eq,topE s,Const (`Int c)),j+i))::l)
		  pairs
		  [Goto (def+i)]),
      stats
  | OpReturn k -> [],
      (match k with
	 | `Void -> [Return None]
	 | _ -> [Return (Some (topE s))]),
      stats
  | OpGetField (c, f) -> 
      let r = topE s in
	if flat then begin
	  incr_stats stats `Nb_tempvar ;
	  incr_stats stats `Nb_tempvar_flat ;
	  let x = TempVar (i,None) in
	    E (Var x)::(pop s), 
	  [Check (CheckNullPointer r);AffectVar(x,Field (r,c,f))], stats 
	end
	else 
	  E (Field (r,c,f))::(pop s), [Check (CheckNullPointer r)],stats
  | OpGetStatic (c, f) -> E (StaticField (c, f))::s, [MayInit c],stats 
  | OpPutStatic (c, f) -> 
      incr_stats stats `Nb_putstatic ;
      if is_static_in_stack c f (pop s) then begin
	incr_stats stats `Nb_putstatic_is_static_in_stack ;
	incr_stats stats `Nb_tempvar ;
	let x = TempVar (i,None) in
	  add_tempvars stats x ;
	  replace_static_in_stack c f x (pop s), 
	[MayInit c;
	 AffectVar(x,StaticField(c,f));
	 AffectStaticField (c,f,topE s)],stats
      end else 
	pop s, [AffectStaticField (c, f,topE s)],stats
  | OpInvoke (x, ms) -> 
      begin
	incr_stats stats `Nb_method_call ;
	(match x with
	   | `Static c -> 
	       (match ms_rtype ms with
		  | None -> 
		      let c1, c2 = 
			(match stats with 
			     Some s -> s.nb_method_call_with_modifiable_in_stack, s.nb_tempvar_method_effect
			   | None -> 0, 0 ) in 
		      let (s,instrs,count,count') = clean  c1 c2 i is_heap_sensible_element_in_expr
			(popn (List.length (ms_args ms)) s) 
			[InvokeStatic (None,c,ms,param (List.length  (ms_args ms)) s)]
		      in
			(match stats with 
			     Some s -> (s.nb_method_call_with_modifiable_in_stack <- count ;
					s.nb_tempvar_method_effect <- count' )
			   | None -> ());
			(s,instrs,stats)
		  | Some _ ->
		      incr_stats stats `Nb_tempvar ;
		      let x = TempVar (i,None) in
			add_tempvars stats x ;
			let c1, c2 = 
			  (match stats with 
			       Some s -> s.nb_method_call_with_modifiable_in_stack, s.nb_tempvar_method_effect
			     | None -> 0, 0 )
			in let (s,instrs,count,count') = clean c1 c2 i is_heap_sensible_element_in_expr (E (Var x)::(popn (List.length (ms_args ms)) s)) 
			    [InvokeStatic (Some x,c,ms,param (List.length (ms_args ms)) s)]
			in
			  (match stats with 
			       Some s -> (s.nb_method_call_with_modifiable_in_stack <- count ;
					  s.nb_tempvar_method_effect <- count')
			     | None -> ());
			  (s,instrs,stats))
	   | x -> 
	       begin
		 let popn_s = popn (List.length (ms_args ms)) s in
		   (match top popn_s  with
		      | Uninit (c,j) ->
			  incr_stats stats `Nb_tempvar ;
			  let x = TempVar (i,None) in
			  let e' = E (Var x) in
			    add_tempvars stats x ;
			    let c1, c2 = 
			      (match stats with 
				   Some s -> s.nb_method_call_with_modifiable_in_stack, s.nb_tempvar_method_effect
				 | None -> 0, 0 )
			    in 
			    let (s,instrs,count,count') = clean c1 c2 i  is_heap_sensible_element_in_expr
			      (List.map 
				 (function e -> if e = Uninit (c,j) then e' else e)
				 (pop popn_s))
			      [New (x,c,ms_args ms,param (List.length (ms_args ms)) s)]
			    in 
			      (match stats with 
				   Some s -> (s.nb_method_call_with_modifiable_in_stack <- count ;
					      s.nb_tempvar_method_effect <- count' )
				 | None -> ());
			      (s,instrs,stats)
		      | E e0  ->
			  let nb_args = List.length (ms_args ms) in
			  let s_next = pop popn_s in
			  let this = topE popn_s in
			  let ins target = 
			    begin
			      match x with
				| `Virtual o ->
				    [InvokeVirtual (target,this,VirtualCall o,ms,param nb_args s)]
				| `Interface c ->
				    [InvokeVirtual (target,this,InterfaceCall c,ms,param nb_args s)]  
				| `Special c ->
				    [InvokeNonVirtual (target,this,c,ms,param nb_args s)]
				| `Static _ -> assert false (* already treated above *) 
			    end
			  in
			    (match ms_rtype ms with
			       | None -> 
				   let c1, c2 = 
				     (match stats with 
					  Some s -> s.nb_method_call_with_modifiable_in_stack, s.nb_tempvar_method_effect
					| None -> 0, 0 )
				   in
				   let (s,instrs,count,count') = 
				     clean c1 c2 i is_heap_sensible_element_in_expr s_next ([Check (CheckNullPointer e0)]@(ins None))
				   in
				     (match stats with 
					  Some s -> (s.nb_method_call_with_modifiable_in_stack <- count ;
						     s.nb_tempvar_method_effect <- count' )
					| None -> ());
				     (s,instrs,stats)				  
			       | Some _ -> 
				   incr_stats stats `Nb_tempvar ;
				   let y = TempVar (i,None) in 
				     add_tempvars stats y ;
				     let c1, c2 = 
				       (match stats with 
					    Some s -> s.nb_method_call_with_modifiable_in_stack, s.nb_tempvar_method_effect
					  | None -> 0, 0 )
				     in
				     let (s,instrs,count,count') =
				       clean c1 c2 i is_heap_sensible_element_in_expr (E (Var y)::s_next) ([Check (CheckNullPointer e0)]@(ins (Some y)))
				     in
				       (match stats with 
					    Some s -> (s.nb_method_call_with_modifiable_in_stack <- count ;
						       s.nb_tempvar_method_effect <- count' )
					  | None -> ());
				       (s,instrs,stats)
			    )) 
	       end)
      end
	
  | OpNew c -> (Uninit (c,i))::s, [MayInit c], stats
  | OpNewArray t -> 
      incr_stats stats `Nb_tempvar ;
      let x = TempVar (i,None) in
	add_tempvars stats x ;
	let dim = topE s in
	  E (Var x)::(pop s), [Check (CheckNegativeArraySize dim); NewArray (x,t,[dim])],stats
  | OpArrayLength -> 
      let a = topE s in
	E (Unop (ArrayLength,a))::(pop s), 
      [Check (CheckNullPointer a)],stats
  | OpThrow -> 
      let r = topE s in
	[], [Check (CheckNullPointer r); Throw r],stats
  | OpCheckCast _ -> s, [Check (CheckCast (topE s))],stats
  | OpInstanceOf c -> E (Unop (InstanceOf c,topE s))::(pop s), [],stats
  | OpMonitorEnter -> 
      let r = topE s in
	pop s, [Check (CheckNullPointer r); MonitorEnter r],stats
  | OpMonitorExit -> 
      let r = topE s in
	pop s, [Check (CheckNullPointer r); MonitorExit r],stats
  | OpAMultiNewArray (cn,dim) -> 
       incr_stats stats `Nb_tempvar ; 
      let x = TempVar (i,None) in
	add_tempvars stats x ;
	let params = param dim s in
	  E (Var x)::(popn dim s), 
      (List.map (fun e -> Check (CheckNegativeArraySize e)) params)
      @[NewArray (x,remove_dim cn dim,params)],stats
  | OpBreakpoint -> failwith "breakpoint"  
  | OpInvalid -> failwith "invalid"
      
exception End_of_method
let next c i =
  try
    let k = ref (i+1) in
      while c.(!k)=OpInvalid do incr k done;
      !k
  with _ -> raise End_of_method

let normal_next code i = 
  match code.c_code.(i) with
  | OpIf (_, n) 
  | OpIfCmp (_, n) -> [next code.c_code i;i+n]
  | OpGoto n -> [i+n]
  | OpJsr _
  | OpRet _ -> raise Subroutine
  | OpTableSwitch (default, _, _, table) ->
      List.map (( + ) i) (default :: Array.to_list table)
  | OpLookupSwitch (default, npairs) ->
      List.map (( + ) i) (default :: List.map snd npairs)
  | OpReturn _ -> []
  | OpThrow -> []
  | OpBreakpoint -> failwith "breakpoint"  
  | OpInvalid -> failwith "invalid"
  | _ -> [next code.c_code i]

let is_jump_instr = function
  | OpIfCmp _ 
  | OpTableSwitch _
  | OpLookupSwitch _ 
  | OpIf _ 
  | OpGoto _ -> true
  | _ -> false


module MapPc = Map.Make(struct type t=int let compare = compare end)

let newStackJump pc n =
  let rec aux i =
    if i>=n then [] else E (Var (TempVar (pc,Some i)))::(aux (i+1))
  in aux 0

let is_branchvar_in_stack succs =
  let test = function
    | BranchVar (i,_) -> List.mem i succs
    | _ -> false in
  is_in_stack test (fun _ _ -> false)


let para_assign pc succs stack =
  if is_branchvar_in_stack succs stack then
    let rec aux i = function
      [] -> [], []
    | e::q -> begin
	match e with
	  | Uninit _ -> aux (i+1) q
	  | E e -> 
	      let (l1,l2) = aux (i+1) q in
	      let x = BranchVar2 (pc,i) in
	      let l = List.map (fun j -> AffectVar (BranchVar (j,i),Var x)) succs in
		AffectVar (x,e) :: l1, l @ l2
      end
    in 
    let (l1,l2) = aux 0 stack in
      l1@l2
  else
    let rec aux i = function
	[] -> []
      | e::q -> begin
	  match e with
	    | Uninit _ -> aux (i+1) q
	    | E e -> 
		let l = List.map (fun j -> AffectVar (BranchVar (j,i),e)) succs in
		  l @ (aux (i+1) q)
	end
    in aux 0 stack

let jump_stack count1 count2 pc' stack =
  let rec aux i c1 c2 = function
      [] -> ([],c1,c2)
    | e::q -> begin
	match e with 
	  | Uninit _ -> 
	      let a,b,c = (aux (i+1) c1 c2 q) in 
		(e :: a, b, c)
	  | E _ -> 
	      let a,b,c = (aux (i+1) c1 c2 q) in 
	      ( E (Var (BranchVar (pc',i))) :: a, b+1 , c+1)
      end
  in aux 0 count1 count2 stack

(* simplify the assignts chain, if this is allowed according to out_stack *)
let simplify_assign flat stats bir out_stack = 
  match bir with
    | (pc,[AffectVar(j,Var(k))])::(pc',instrs)::q ->
	begin (* remove useless assignt *)
	match List.rev instrs with
	  | New(i,c,types,params)::l when  (var_equal i k) ->
	      if (is_in_stack (var_equal j) (fun _ _ -> false) out_stack ) then 
		(* check the variable removal can be done (no further used in the block) *)
		bir,stats
	      else
		(incr_stats stats `Nb_tempvar_removed ;
		 (pc,[])::(pc',List.rev (New (j,c,types,params)::l))::q , stats)
	  | (NewArray(i,c,params))::l when  (var_equal i k) ->
	      if (is_in_stack (var_equal j) (fun _ _ -> false) out_stack ) then 
		bir,stats
	      else 
		 (incr_stats stats `Nb_tempvar_removed ;
		 (pc,[])::(pc',List.rev (NewArray (j,c,params)::l))::q , stats)
	  | (InvokeStatic (Some x,c,ms,le))::l when var_equal x k ->
	      if flat then bir,stats
	      else
		if (is_in_stack (var_equal j) (fun _ _ -> false) out_stack ) then 
		  bir,stats
		else 
		  (incr_stats stats `Nb_tempvar_removed ;
		   (pc,[])::(pc',List.rev (InvokeStatic (Some j,c,ms,le)::l))::q , stats)
	  | (InvokeVirtual (Some x,e1,kd,ms,le))::l when var_equal x k ->
	      if flat then bir, stats
	      else 
		if (is_in_stack (var_equal j) (fun _ _ -> false) out_stack ) then 
		  bir,stats
		else 
		  (incr_stats stats `Nb_tempvar_removed ;
		   (pc,[])::(pc',List.rev (InvokeVirtual (Some j,e1,kd,ms,le)::l))::q , stats)
	  | (InvokeNonVirtual (Some x,e1,kd,ms,le))::l when var_equal x k ->
	      if flat then bir,stats
	      else 
		if ( is_in_stack (var_equal j) (fun _ _ -> false) out_stack ) then 
		  bir,stats
		else 
		  (incr_stats stats `Nb_tempvar_removed ;
		   (pc,[])::(pc',List.rev (InvokeNonVirtual (Some j,e1,kd,ms,le)::l))::q ,stats)
	  | _ -> bir,stats
	end
    | _ -> bir,stats

exception NonemptyStack_backward_jump
exception Type_constraint_on_Uninit
exception Content_constraint_on_Uninit

let bc2ir flat pp_var jump_target code stats0 =
  let rec loop as_ts_jump ins ts_in as_in pc stats =
    let succs = normal_next code pc in
    let (ts_in,as_in) =
      if jump_target.(pc) then
	try MapPc.find pc as_ts_jump
	with Not_found -> 
	  (* no predecessor of pc have been visited before *)
	  if List.exists (fun e -> pc = e.e_handler) code.c_exc_tbl then
	    (* this is a handler point *)
	    ([Op32],[E (Var (TempVar (pc,Some 0)))])
	  else
	    (* this is a back jump target *)
	    ([],[])
      else (ts_in,as_in)
    in 
    let ts_out = type_next code.c_code.(pc) ts_in in
    let (as_out,instrs,stats) = bc2bir_instr flat pp_var pc ts_in as_in stats code.c_code.(pc)  in
      
      (* fail on backward branchings on a non-empty stack *)
      if List.length as_out>0 then  
	if (List.exists (fun j -> j<pc) succs) then raise NonemptyStack_backward_jump;
    
    let jump_succs = List.filter (fun i -> jump_target.(i)) succs in
    let branch_assigns = para_assign pc jump_succs as_out in
    let ins,stats =
      if is_jump_instr code.c_code.(pc) then
	(pc,branch_assigns@instrs)::ins, stats
      else if jump_target.(pc) then
	(pc,instrs@branch_assigns)::ins, stats
      else
	simplify_assign flat stats ((pc,instrs@branch_assigns)::ins) as_out
    in
    let as_ts_jump =       
      List.fold_left 
	(fun as_jump pc' ->
	   try 
	     let (ts_jmp,as_jmp) = MapPc.find pc' as_jump in
	       (* check constraint on expr uninit and jumping forward on a non empty stack 
		  all defined predecessor advice must match what is reached *)
	       if (ts_jmp <> ts_out) then raise Type_constraint_on_Uninit ;
	       let (jmp_s, _,_ ) =  jump_stack 0 0 pc' as_out in
		 if (as_jmp <>  jmp_s) then raise Content_constraint_on_Uninit ;
		 as_jump
	   with Not_found -> 
	     (* when first advice for pc', no constraint to check. add the advice in the map *)
	     let (count1, count2) = 
	       (match stats with 
		  | Some s -> s.nb_tempvar, s.nb_tempvar_branch
		  | None -> 0,0) in
	     let (st,c1,c2) = jump_stack count1 count2 pc' as_out
	     in
	       (match stats with 
		    Some s -> (s.nb_tempvar <- c1 ; s.nb_tempvar_branch <- c2)
		  | None -> ());
	       MapPc.add pc' (ts_out,st) as_jump)
	as_ts_jump
	jump_succs in
      try
	loop as_ts_jump ins ts_out as_out (next code.c_code pc) stats
      with End_of_method -> ins
  in 
    (match stats0 with 
	 Some s ->
	   (s.nb_tempvar <- 0;
	    s.nb_tempvar_branch <- 0;
	    s.nb_tempvar_putfield <- 0;
	    s.nb_tempvar_method_effect <- 0;
	    s.nb_tempvar_arraystore <- 0;
	    s.nb_tempvar_removed <- 0;
	    s.nb_tempvar_side_effect <- 0;
	    s.nb_tempvar_flat <- 0;
	    s.tempvars <- [];)
       | None -> ());    
    let res = loop MapPc.empty [] [] [] 0 stats0 in
      List.iter 
      (fun x -> if is_var_in_expr_bir_not_var x res then 
	 incr_stats stats0 `Nb_tempvar_side_effect)
      (match stats0 with 
	   Some s ->  s.tempvars
	 | None -> []);
    if code.c_max_locals>0 then begin
      let get_avg x = (float_of_int (100 * x))/.(float_of_int code.c_max_locals) in
	(match stats0 with 
	   | Some s -> 
	       begin 
		 let avg = get_avg s.nb_tempvar in
		 let avg_branch = get_avg s.nb_tempvar_branch in
		 let avg_putfield = get_avg s.nb_tempvar_putfield in
		 let avg_arraystore = get_avg s.nb_tempvar_arraystore in
		 let avg_method_effect = get_avg s.nb_tempvar_method_effect in
		 let avg_side_effect = get_avg s.nb_tempvar_side_effect in
		 let avg_flat = get_avg s.nb_tempvar_flat in
		 let avg_simp = get_avg s.nb_tempvar_removed in
		   s.average_tempvar <- avg :: s.average_tempvar;
		   s.average_tempvar_branch <- avg_branch :: s.average_tempvar_branch;
		   s.average_tempvar_putfield <- avg_putfield :: s.average_tempvar_putfield;
		   s.average_tempvar_arraystore <- avg_arraystore :: s.average_tempvar_arraystore;
		   s.average_tempvar_method_effect <- avg_method_effect :: s.average_tempvar_method_effect;
		   s.average_tempvar_after_simplification <- avg_simp :: s.average_tempvar_after_simplification;
		   s.average_tempvar_side_effect <- avg_side_effect :: s.average_tempvar_side_effect;
		   s.average_tempvar_flat <- avg_flat :: s.average_tempvar_flat
	       end
	   | None -> ())
    end;
    (res, stats0)
      
let varname = Cmn.varname

let search_name_localvar static code i x = 
  if x=0 && (not static) then "this"
  else match JCode.get_local_variable_info x i code with
    | None ->  Printf.sprintf "%s%d" varname x 
    | Some (s,_) -> s

let compute_jump_target code =
  let jump_target = Array.make (Array.length code.c_code) false in
    List.iter (fun e -> jump_target.(e.e_handler) <- true) code.c_exc_tbl;
    Array.iteri
      (fun i instr ->
	 match instr with
	   | OpIf (_, n) 
	   | OpIfCmp (_, n) 
	   | OpGoto n -> jump_target.(i+n) <- true;
	   | OpJsr _
	   | OpRet _ -> raise Subroutine
	   | OpTableSwitch (default, _, _, table) ->
	       List.iter (fun n -> jump_target.(i+n) <- true) (default :: Array.to_list table)
	   | OpLookupSwitch (default, npairs) ->
	       List.iter (fun n -> jump_target.(i+n) <- true) (default :: List.map snd npairs)
	   | _ -> ())
      code.c_code;
    jump_target

let gen_params pp_var cm =
  if cm.cm_static then
    ExtList.List.mapi
      (fun i _ -> OriginalVar (0,i,pp_var 0 i))
      (ms_args cm.cm_signature)
  else 
    (OriginalVar (0,0,pp_var 0 0))::
      (ExtList.List.mapi
	 (fun i _ -> OriginalVar (0,i+1,pp_var 0 (i+1)))
	 (ms_args cm.cm_signature))

let transform_intra_stats flat ?(stats=false) ?(stats_opt=None) m =
  let signature = m.cm_signature in
  let cm_sig = m.cm_class_method_signature in 
  let cm_stat = m.cm_static in 
  let cm_final = m.cm_final in 
  let cm_synch = m.cm_synchronized in
  let cm_strict = m.cm_strict in 
  let cm_access = m.cm_access in 
  let cm_gsig = m.cm_generic_signature in 
  let cm_bridge = m.cm_bridge in 
  let cm_varargs = m.cm_varargs in 
  let cm_synt = m.cm_synthetic in 
  let oth = m.cm_other_flags in 
  let cm_exn = m.cm_exceptions in 
  let cm_att = m.cm_attributes in 
  let (implem,stats) = 
    match m.cm_implementation with
      | Native -> (Native,None)
      | Java code -> 
	begin 
	  let code = (Lazy.force code) in
	  let pp_var = search_name_localvar m.cm_static code in
	  let jump_target = compute_jump_target code in
	  let (res,stats) =
	    if not stats then
	      (bc2ir flat pp_var jump_target code None) 
	    else 
	      (bc2ir flat pp_var jump_target code stats_opt) 
	  in 
	    Java { params = gen_params pp_var m;
		   code = List.rev res;
		   exc_tbl = code.c_exc_tbl;
		   line_number_table = code.c_line_number_table }, stats
	end
  in
  let method_rec = {
	cm_signature = signature ; cm_class_method_signature = cm_sig ;	cm_static = cm_stat ;
   	cm_final = cm_final ; cm_synchronized = cm_synch ; cm_strict = cm_strict ;
   	cm_access = cm_access ; cm_generic_signature = cm_gsig ; cm_bridge = cm_bridge ;
   	cm_varargs = cm_varargs ; cm_synthetic = cm_synt ; cm_other_flags = oth ;
   	cm_exceptions = cm_exn ; cm_attributes = cm_att ; cm_implementation = implem ; 
  }
  in
    (method_rec, stats)

let transform_intra = transform_intra_stats ~stats:false

let stats0 =  
   { nb_tempvar = 0; nb_tempvar_branch = 0; nb_tempvar_putfield = 0;
     nb_tempvar_method_effect = 0; nb_tempvar_arraystore = 0; 
     nb_tempvar_removed = 0; nb_tempvar_side_effect = 0;
     nb_tempvar_flat = 0;
     nb_jump_with_non_empty_stacks = 0 ; nb_back_jump_with_non_empty_stacks = 0 ;
     nb_store_is_var_in_stack= 0 ; nb_incr_is_var_in_stack= 0 ;
     nb_putfield_is_field_in_stack= 0 ; nb_arraystore_is_array_access_in_stack = 0 ;
     nb_putstatic_is_static_in_stack= 0 ; nb_method_call_with_modifiable_in_stack= 0 ;
     nb_store= 0 ; nb_incr= 0 ; nb_putfield= 0 ;
     nb_arraystore= 0 ; nb_putstatic= 0 ; nb_method_call= 0 ;
     tempvars = [];	average_tempvar= [] ; average_tempvar_side_effect = [] ;
     average_tempvar_flat = [] ;
     average_tempvar_after_simplification= [] ; average_tempvar_branch= [] ;
     average_tempvar_method_effect= [] ; average_tempvar_putfield= [] ;
     average_tempvar_arraystore= [] ;
     nb_classes = 0 ; nb_methods = 0 ; nb_subroutines = 0 ;
   }
    
let reset_stats0 = 
  begin
  stats0.nb_tempvar <- 0; stats0.nb_tempvar_branch <- 0; stats0.nb_tempvar_putfield <- 0;
  stats0.nb_tempvar_method_effect <- 0; stats0.nb_tempvar_arraystore <- 0; 
  stats0.nb_tempvar_removed <- 0; stats0.nb_tempvar_side_effect <- 0;
  stats0.nb_tempvar_flat <- 0;
  stats0.nb_jump_with_non_empty_stacks <- 0 ; stats0.nb_back_jump_with_non_empty_stacks <- 0 ;
  stats0.nb_store_is_var_in_stack<- 0 ; stats0.nb_incr_is_var_in_stack<- 0 ;
  stats0.nb_putfield_is_field_in_stack<- 0 ; stats0.nb_arraystore_is_array_access_in_stack <- 0 ;
  stats0.nb_putstatic_is_static_in_stack<- 0 ; stats0.nb_method_call_with_modifiable_in_stack<- 0 ;
  stats0.nb_store<- 0 ; stats0.nb_incr<- 0 ; stats0.nb_putfield<- 0 ;
  stats0.nb_arraystore<- 0 ; stats0.nb_putstatic<- 0 ; stats0.nb_method_call<- 0 ;
  stats0.tempvars <- []; stats0.average_tempvar<- [] ; stats0.average_tempvar_side_effect <- [] ;
  stats0.average_tempvar_flat <- [] ;
  stats0.average_tempvar_after_simplification<- [] ; stats0.average_tempvar_branch<- [] ;
  stats0.average_tempvar_method_effect<- [] ; stats0.average_tempvar_putfield<- [] ;
  stats0.average_tempvar_arraystore<- [] ;
  stats0.nb_classes <- 0 ; stats0.nb_methods <- 0 ; stats0.nb_subroutines <- 0 
  end

 let cm_transform_stats flat ?(stats=false) = 
   let _ = reset_stats0 in
   function a -> fst (transform_intra_stats flat ~stats:stats ~stats_opt:(Some stats0) a)
   
 let cm_transform = cm_transform_stats false ~stats:false

 let jmethod_accu flat cstats  m  statsmmap =
   let  (mmap,stat) = statsmmap in 
   match m with
     | ConcreteMethod cm ->
	 begin
	   incr_stats stat `Nb_methods ;
	   try
	     let (ir,stats) = transform_intra_stats flat ~stats:cstats ~stats_opt:stat cm in
	       begin
		 match ir.cm_implementation with
		   | Java _ ->  MethodMap.add (get_method_signature m) (ConcreteMethod ir) mmap , stats
		   | _ -> mmap, stats
	       end
	   with
	     | Subroutine ->
		 incr_stats stat `Nb_subroutines ;
		 mmap, None
	 end
     | AbstractMethod am ->
	 MethodMap.add (get_method_signature m) (AbstractMethod am) mmap , stat

 

 let iorc_transform_intra_stats flat cstats ~stats ci = 
   match ci with 
     | JInterface(i) ->
	 incr_stats stats `Nb_classes ;
	 begin
	   match i.i_initializer with
	     | None -> JInterface(
		 { i_name = i.i_name ;  i_version = i.i_version ;
		   i_access = i.i_access ; i_interfaces = i.i_interfaces ;
		   i_generic_signature = i.i_generic_signature ; i_consts = i.i_consts  ;
		   i_sourcefile = i.i_sourcefile ; i_deprecated = i.i_deprecated ;
		   i_source_debug_extention = i.i_source_debug_extention ; i_inner_classes = i.i_inner_classes ;
		   i_other_attributes = i.i_other_attributes ; i_annotation = i.i_annotation ;
		   i_other_flags = i.i_other_flags ; i_fields = i.i_fields ;
		   i_methods = i.i_methods ;  i_initializer = None
		   }) , stats
	       | Some cm -> 
		   let (ir,stats) = transform_intra_stats flat ~stats:cstats cm ~stats_opt:stats
		   in
		     JInterface( 
		       { i_name = i.i_name ; i_version = i.i_version ; i_access = i.i_access ;
			 i_interfaces = i.i_interfaces ; i_generic_signature = i.i_generic_signature ;
			 i_consts = i.i_consts  ; i_sourcefile = i.i_sourcefile ; i_deprecated = i.i_deprecated ;
			 i_source_debug_extention = i.i_source_debug_extention ;
			 i_inner_classes = i.i_inner_classes ; i_other_attributes = i.i_other_attributes ;
			 i_annotation = i.i_annotation ; i_other_flags = i.i_other_flags ;
			 i_fields = i.i_fields ; i_methods = i.i_methods ;  i_initializer = Some ir ;
		       }), stats
	 end
     |  JClass (cl) -> 
	  begin
	    incr_stats stats `Nb_classes ;
	    let a, b = MethodMap.fold (fun _ -> jmethod_accu flat cstats )  cl.c_methods (MethodMap.empty,stats) 
	    in  JClass ( {
			   c_name = cl.c_name ; c_version = cl. c_version ; c_access = cl.c_access ; c_final = cl.c_final ;
			   c_abstract = cl.c_abstract ; c_super_class = cl.c_super_class ; 
			   c_generic_signature = cl.c_generic_signature ; c_fields = cl.c_fields ; c_interfaces = cl.c_interfaces ;
			   c_consts = cl.c_consts ; c_sourcefile = cl.c_sourcefile; c_deprecated = cl.c_deprecated ;
			   c_enclosing_method = cl.c_enclosing_method ; c_source_debug_extention = cl.c_source_debug_extention ;
			   c_inner_classes = cl.c_inner_classes ; c_synthetic = cl.c_synthetic ; c_enum = cl.c_enum ;
			   c_other_flags = cl.c_other_flags ; c_other_attributes = cl.c_other_attributes ; c_methods = a ;
			 }) , b
	  end


 let iorc_transform_intra flat = iorc_transform_intra_stats flat false ~stats:None



let iorc_transform_stats flat ?(cstats=false) ci =
  reset_stats0 ; 
  fst (iorc_transform_intra_stats flat cstats ~stats:(Some stats0) ci)

let iorc_transform = iorc_transform_stats false ~cstats:false


let is_dir d =
  try
    (Unix.stat d).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let is_file f =
  try
    (Unix.stat f).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false


 
let cn_transform_stats flat ?(cstats=false) classfile =
  if is_file classfile && Filename.check_suffix classfile ".class" then 
    begin
      let cp = class_path (Filename.dirname classfile) in
      let file = Filename.chop_suffix (Filename.basename classfile) ".class" in
      let stats = iorc_transform_stats flat ~cstats:cstats (get_class cp (make_cn file)) in
	close_class_path cp;
	stats
    end 
  else raise (JBasics.No_class_found classfile)


let cn_transform = cn_transform_stats false ~cstats:false 


