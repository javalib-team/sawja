open JBasics
open Javalib
open JCode

include Cmn

(*********** TYPES *************)

type binop =
  | ArrayLoad
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
      (* value_type is the type of the array content *)
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

type t = {
  params : (value_type * var) list;
  code : (int * instr list) list;
  exc_tbl : exception_handler list;
  line_number_table : (int * int) list option;
  jump_target : bool array
}

(* For stack type inference only *)
type op_size = Op32 | Op64

(************* PRINT ************)      

let print_binop = function
  | ArrayLoad -> Printf.sprintf "ArrayLoad"
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
  | Var (t,x) -> Printf.sprintf "%s:%s" (var_name_g x) (print_typ t)
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

let print m = print_code m.code

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

let compute_handlers code i =
  let handlers = code.c_exc_tbl in
  let handlers = List.filter (fun e -> e.e_start <= i && i < e.e_end) handlers in
  let handlers = List.map (fun e -> e.e_handler) handlers in
    handlers

let succs code i = 
  (normal_next code i)@(compute_handlers code i)

let mapi f g =
  let rec aux i = function
      [] -> []
    | x::q -> (f i x)::(aux (g i x) q) 
  in  aux 0

module BCV = struct

type typ =
  | Top
  | Bot
  | Top32
  | Top64
  | Int
  | Float
  | Double
  | Long
  | Object
  | Array of value_type

type t = typ list * typ Ptmap.t

let conv = function
  | TObject (TArray v) -> Array v
  | TObject (TClass _) -> Object 
  | TBasic jbt -> 
      (match jbt with
	 | `Int
	 | `Short
	 | `Char
	 | `Byte
	 | `Bool -> Int
	 | `Float -> Float
	 | `Long -> Long
	 | `Double -> Double)

let rec print_typ = function
  | Top -> "Top"
  | Bot -> "Bot"
  | Top32 -> "Top32"
  | Top64 -> "Top64"
  | Int -> "I"
  | Float -> "F"
  | Double -> "D"
  | Long -> "L"
  | Object -> "O"
  | Array v -> "["^(print_typ (conv v))

let print (s,l) =
  Printf.sprintf "{%s} %s"
    (print_list_sep ";" print_typ s)
    (List.fold_right
       (fun (i,t) -> Printf.sprintf "%d:%s %s" i (print_typ t))
       (Ptmap.elements l) "")

exception BadLoadType
let to_value_type = function
  | Top32 
  | Top64
  | Top -> raise BadLoadType
  | Int -> TBasic `Int
  | Float -> TBasic `Float
  | Double -> TBasic `Double
  | Long -> TBasic `Long
  | Bot 
  | Object -> TObject (TClass java_lang_object)
  | Array v -> TObject (TArray v)


exception GetNotFound
let get l n = 
  try Ptmap.find n l 
  with Not_found -> assert false

let get l n =
  let res = get l n in
  let _ = to_value_type res in
    res

let upd l n t = 
  Ptmap.add n t l 

exception ArrayContent
let array_content i = function
  | Array v -> conv v
  | x -> Printf.printf "\n\nbad array_content of %s at %d\n\n\n" (print_typ x) i; raise ArrayContent

let basic = function
  | `Int2Bool -> Int
  | `Long -> Long
  | `Double -> Double
  | `Float -> Float

let size = function
  | Top -> assert false
  | Int
  | Float
  | Object
  | Array _
  | Top32 -> Op32
  | Double
  | Long
  | Top64 -> Op64
  | Bot -> assert false

let is32_basic = function
  | `Int
  | `Short
  | `Char
  | `Byte
  | `Bool
  | `Float -> true
  | `Long -> false
  | `Double -> false

let rec leq_value_type v1 v2 = 
  match v1, v2 with
  | TObject v1, TObject v2 -> leq_object_type v1 v2
  | TBasic t1, TBasic t2 -> t1=t2
  | _, _ -> false
and leq_object_type v1 v2 = 
  match v1, v2 with
  | TClass c1, TClass c2 -> cn_equal c1 c2 || cn_equal c2 java_lang_object
  | TArray v1, TArray v2 -> leq_value_type v1 v2
  | TArray _, TClass c -> cn_equal c java_lang_object
  | _, _ -> false

let (<=) x y =
  x==y ||
    x=y ||
      y=Top ||
      x=Bot ||
      (match y with
	 | Top64 -> x=Double || x=Long || x=Top64
	 | Object -> (match x with Array _ -> true | _ -> false)
	 | Top32 -> (match x with 
			 Array _ | Object
		       | Int | Float | Top32-> true
		       | _ -> false)
	 | Array y -> (match x with Array x -> leq_value_type x y | _ -> false)
	 | _ -> false)

(* v1 and v2 not ordered *)
let rec lub_value_type v1 v2 = 
  match v1, v2 with
    | TObject v1, TObject v2 -> Some (TObject (lub_object_type v1 v2))
    | _, _ -> None
and lub_object_type x y =
  match x, y with
  | TArray v1, TArray v2 -> 
      (match lub_value_type v1 v2 with
	 | None -> TClass java_lang_object
	 | Some v -> TArray v)
  | _, _ -> TClass java_lang_object

let lub x y =
  if x <= y then y
  else if  y <= x then x
  else match x with
    | Int
    | Float
    | Object -> (match y with Top64 | Double | Long -> Top | _ -> Top32)
    | Double 
    | Long -> (match y with Double | Long -> Top64 | _ -> Top)
    | Array x -> 
	(match y with
	     Array y -> 
	       (match lub_value_type x y with
		  | None -> Object
		  | Some v -> Array v)
	   | Int | Float -> Top32
	   | _ -> Top)
    | _ -> Top
	
let next i = function
  | OpNop -> (function (s,l) -> (s,l))
  | OpConst x -> (fun (s,l) -> 
		    let c = match x with
		      | `ANull -> Bot
		      | `String _
		      | `Class _ -> Object
		      | `Byte _ 
		      | `Short _
		      | `Int _ -> Int
		      | `Long _ -> Long
		      | `Float _ -> Float
		      | `Double _ -> Double in
		      (c::s,l))
  | OpLoad (_,n) -> (fun (s,l) -> (get l n)::s,l)
  | OpArrayLoad _ -> (fun (s,l) -> array_content i (top (pop s))::(pop2 s),l)
  | OpStore (_,n) -> (fun (s,l) -> pop s, upd l n (top s))
  | OpArrayStore _ -> (fun (s,l) -> pop3 s, l)
  | OpPop -> (fun (s,l) -> pop s, l)
  | OpPop2 -> (fun (s,l) ->
		 (match size (top s) with
		    | Op32 -> pop2 s
		    | Op64 -> pop s), l)
  | OpDup -> (fun (s,l) -> (top s)::s, l)
  | OpDupX1 -> (fun (s,l) -> (top s)::(top (pop s))::(top s)::(pop2 s), l)
  | OpDupX2 -> (fun (s,l) -> 
     (match size (top (pop s)) with
	| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s), l
	| Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s), l))
  | OpDup2 -> (fun (s,l) ->
      (match size (top s) with
	 | Op32 -> (top s)::(top (pop s))::(top s)::(top (pop s))::(pop2 s), l
	 | Op64 -> (top s)::s, l))
  | OpDup2X1 -> 
      (fun (s,l) -> match size (top s) with
	 | Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s), l
	 | Op64 ->  (top s)::(top (pop s))::(top s)::(pop2 s), l)
  | OpDup2X2 -> 
      (fun (s,l) -> match size (top s) with
	 | Op32 ->
	     (match size (top (pop2 s)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top (pop3 s))::(top s)::(top (pop s))::(pop (pop3 s)), l
		| Op64 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s), l)
	 | Op64 ->
	     (match size (top (pop s)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s), l
		| Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s), l))
  | OpSwap -> (fun (s,l) -> (top (pop s))::(top s)::(pop2 s), l)
  | OpAdd k 
  | OpSub k 
  | OpMult k 
  | OpDiv k 
  | OpRem k  -> (fun (s,l) -> (basic k)::(pop2 s), l)
  | OpNeg k -> (fun (s,l) -> (basic k)::(pop s), l)
  | OpIShl
  | OpIShr
  | OpIAnd
  | OpIOr 
  | OpIXor
  | OpIUShr -> (fun (s,l) ->  Int::(pop2 s), l)
  | OpLShr
  | OpLShl -> (fun (s,l) ->  pop s, l)
  | OpLAnd
  | OpLOr 
  | OpLXor
  | OpLUShr -> (fun (s,l) -> Long::(pop2 s), l)
  | OpIInc (_,_) -> (fun (s,l) -> s,l) 
  | OpI2L -> (fun (s,l) -> Long::(pop s), l)  
  | OpI2F -> (fun (s,l) -> Float::(pop s), l)
  | OpI2D -> (fun (s,l) -> Double::(pop s), l)
  | OpL2I -> (fun (s,l) -> Int::(pop s), l)
  | OpL2F -> (fun (s,l) -> Float::(pop s), l)
  | OpL2D -> (fun (s,l) -> Double::(pop s), l)
  | OpF2I -> (fun (s,l) -> Int::(pop s), l)
  | OpF2L -> (fun (s,l) -> Long::(pop s), l)
  | OpF2D -> (fun (s,l) -> Double::(pop s), l)
  | OpD2I -> (fun (s,l) -> Int::(pop s), l)
  | OpD2L -> (fun (s,l) -> Long::(pop s), l)
  | OpD2F -> (fun (s,l) -> Float::(pop s), l)
  | OpI2B -> (fun (s,l) -> Int::(pop s), l)
  | OpI2C -> (fun (s,l) -> Int::(pop s), l)
  | OpI2S -> (fun (s,l) -> Int::(pop s), l)
  | OpCmp _ -> (fun (s,l) -> Int::(pop2 s),l)
  | OpIf (_, _) -> (fun (s,l) -> pop s, l)
  | OpIfCmp (_, _) -> (fun (s,l) -> pop2 s, l)
  | OpGoto _ -> (fun (s,l) -> s,l)
  | OpJsr _ -> raise Subroutine
  | OpRet _ -> raise Subroutine
  | OpTableSwitch _ -> (fun (s,l) -> pop s, l)
  | OpLookupSwitch _ -> (fun (s,l) -> pop s, l)
  | OpReturn _ -> (function (s,l) -> (s,l))
  | OpGetField (_, fs) ->  (fun (s,l) -> (conv (fs_type fs))::(pop s), l)
  | OpGetStatic (_, fs) -> (fun (s,l) -> (conv (fs_type fs))::s, l)
  | OpPutStatic _ -> (fun (s,l) -> pop s, l)
  | OpPutField _ -> (fun (s,l) -> pop2 s, l)
  | OpInvoke (x, ms) -> (fun (s,l) ->
      let s =
      (match x with
	 | `Static _ -> popn (List.length (ms_args ms)) s
	 | _ -> popn (List.length (ms_args ms)) (pop s) ) in
	(match ms_rtype ms with
	   | None -> s, l
	   | Some t -> (conv t)::s, l))
  | OpNew _ -> (fun (s,l) -> Object::s, l)
  | OpNewArray t -> (fun (s,l) -> Array t::(pop s), l)
  | OpArrayLength -> (fun (s,l) -> Int::(pop s),l)
  | OpThrow -> (fun (s,l) -> (s,l))
  | OpCheckCast t -> (fun (s,l) -> (conv (TObject t))::(pop s),l)
  | OpInstanceOf _-> (fun (s,l) -> Int::(pop s),l)
  | OpMonitorEnter -> (fun (s,l) -> pop s,l)
  | OpMonitorExit -> (fun (s,l) -> pop s,l)
  | OpAMultiNewArray (t,b) -> (fun (s,l) -> (conv (TObject t))::(popn b s), l)
  | OpBreakpoint -> failwith "breakpoint"  
  | OpInvalid -> failwith "invalid"

let init cm =
  let rec aux i = function
      [] -> Ptmap.empty
    | v::q -> 
	if convert_field_type v = Op32 then
	  Ptmap.add i (conv v) (aux (i+1) q)
	else
	  Ptmap.add i (conv v) (aux (i+2) q)
  in
    if cm.cm_static then [], aux 0 (ms_args cm.cm_signature)
    else [], Ptmap.add 0 Object (aux 1 (ms_args cm.cm_signature)) 

let lub (s1,l1) (s2,l2) =
  (List.map2 lub s1 s2,Ptmap.merge lub l1 l2)

let run cm code =
  let rec array_fold f b t i =
    if i>=0 then f i t.(i) (array_fold f b t (i-1)) else b in
  let array_fold f b t = array_fold f b t ((Array.length t)-1) in
  let ws = array_fold 
	     (fun i op ws -> if op=OpInvalid then ws else Ptset.add i ws)
	     Ptset.empty code.c_code in
  let types : t option array = Array.make (Array.length code.c_code) None in
  let upd sl' ws i =
    match types.(i) with
      | None -> types.(i) <- Some sl'; Ptset.add i ws
      | Some sl -> 
	  let sl' = lub sl sl' in
	    if sl=sl' then ws
	    else (types.(i) <- Some sl'; Ptset.add i ws)
  in
  let rec loop ws =
    if Ptset.is_empty ws then ()
    else 
      let i = Ptset.min_elt ws in
      let ws = Ptset.remove i ws in
      let sl = match types.(i) with Some sl -> sl | None -> assert false in
      let sl' = next i code.c_code.(i) sl in
      let ws = List.fold_left (upd sl') ws (normal_next code i) in
      let sl' = ([Object],snd sl') in
      let ws = List.fold_left (upd sl') ws (compute_handlers code i) in
	loop ws
  in
    assert ((Array.length types)>0);
    types.(0) <- Some (init cm);
    (try loop ws 
     with
	 GetNotFound -> Printf.printf "GET_NOT_FOUND !\n"; assert false
       | BadLoadType -> Printf.printf "BAD_LOAD_TYPE !\n"
       | ArrayContent -> assert false);
    (fun i -> 
       match code.c_code.(i) with
	 | OpLoad (_,n) -> 
	     (match types.(i) with
		| Some (_, l) -> to_value_type (get l n)
		| _ -> assert false)
	 | _ -> assert false)

let run_dummy code i =
  match code.c_code.(i) with
    | OpLoad (t,_) -> 
	(match t with
	   | `Long -> TBasic `Long
	   | `Float -> TBasic `Float
	   | `Double -> TBasic `Double
	   | `Int2Bool -> TBasic `Int
	   | `Object -> TObject (TClass java_lang_object))
    | _ -> assert false

let run dummy cm code =
  if dummy then run_dummy code
  else run cm code
	
let print_instr i ins =
  JDump.opcode
    (match ins with
       | OpIf (t, n) -> OpIf (t,n+i)
       | OpIfCmp (t, n) -> OpIfCmp (t,n+i)
       | OpGoto n -> OpGoto (i+n)
       | OpTableSwitch (default, low, high, table) ->
	   OpTableSwitch (default+i, low, high,Array.map ((+)i) table)
       | OpLookupSwitch (default, npairs) ->
	   OpLookupSwitch (default+i,List.map (fun (x,y) -> (x,y+i)) npairs)
       | _ -> ins)

(*
let print_result cm types code =
  let code = Lazy.force code in
  Printf.printf "%s%s\n" (if cm.cm_static then "static " else "") (JPrint.method_signature cm.cm_signature);
  Array.iteri
    (fun i op ->
       if op<>OpInvalid then
	 Printf.printf "    %s\n%3d: %s\n"
	   (match types.(i) with None -> "NONE" | Some sl -> print sl)
	   i (print_instr i op))
    code.c_code;
  List.iter 
    (fun e -> Printf.printf "    %s\n" (JPrint.exception_handler e))
    code.c_exc_tbl

let debug verbose cm code =
(*  if ms_name cm.cm_signature = "marshalIn" then *)
  try
    let types = run cm code in 
      if verbose then print_result cm types code
  with
      Subroutine -> ()
*)

end

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
  | Binop (ArrayLoad,e,_) -> type_of_array_content e
  | Binop (b,_,_) -> 
      TBasic
      (match b with
	 | ArrayLoad -> assert false
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
and type_of_array_content e =
  (match type_of_expr e with
     | TObject (TArray t) -> t (* can this happen ? *)
     | _ -> Printf.printf "%s\n" (print_typ (type_of_expr e)) ; assert false)



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
      | Var (_,x) -> test_var x
      | Unop(_,e) -> aux e
      | Binop(s,e1,e2) -> (test_array && s=ArrayLoad) || aux e1 || aux e2
  in aux

let var_in_expr test_var =
  let rec aux expr =
    match expr with 
      | Const _ -> None
      | StaticField _ -> None
      | Field (e,_,_) -> aux e
      | Var (t,x) -> if test_var x then Some t else None
      | Unop(_,e) -> aux e
      | Binop(_,e1,e2) -> begin
	  match aux e1 with
	    | None -> aux e2
	    | Some t -> Some t
	end
  in aux

let replace_in_expr test_var test_static var =
  let rec aux expr =
    match expr with 
      | Const _ -> expr
      | StaticField (c,f) -> 
	  if test_static c f
	  then Var (fs_type f,var)
	  else expr
      | Field (e,c,f) -> Field (aux e,c,f)
      | Var (t,x) -> if test_var x then Var (t,var) else expr
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

let var_in_stack test_var =
  let rec aux stack =
    match stack with 
      | [] -> None
      | (E e)::s -> begin
	  match var_in_expr test_var  e with
	    | Some t -> Some t
	    | None -> aux s
	end
      | _::s -> aux s
  in aux

let replace_in_stack test_var test_static var =
  let rec aux stack =
    match stack with 
      | [] -> []
      | e'::s -> begin
	  match e' with
	    | E e -> E (replace_in_expr test_var test_static var e)::(aux s)
	    | Uninit _ -> e'::(aux s)
	end
  in aux

let is_var_in_stack x stack =
  var_in_stack (var_equal x) stack

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

let replace_var_in_expr x y =
  replace_in_expr (var_equal x) (fun _ _ -> false) y

let replace_var_in_stack x y stack =
  replace_in_stack (var_equal x) (fun _ _ -> false) y stack

let replace_static_in_stack c f y stack =
  replace_in_stack (fun _ -> false) (fun c0 f0 -> c=c0 && f=f0) y stack


let temp_in_expr acc expr =
  let rec aux acc expr =
    match expr with 
      | Const _ 
      | StaticField _ -> acc
      | Field (e,_,_) -> aux acc e
      | Var (_,x) ->
	  (match x with
	       TempVar i -> Ptset.add i acc
	     | _ -> acc)
      | Unop (_,e) -> aux acc e
      | Binop (_,e1,e2) -> aux (aux acc e1) e2
  in aux acc expr

let temp_in_opexpr acc = function
  | Uninit _ -> acc
  | E e -> temp_in_expr acc e

let temp_in_stack s =
  List.fold_left temp_in_opexpr Ptset.empty s

let fresh_in_stack s =
  let set = temp_in_stack s in
  if Ptset.is_empty set then 0 
  else (Ptset.max_elt set)  +1

let clean test s instrs =
  let rec aux fresh = function
    | [] -> [], instrs, fresh
    | e::s -> 
	let (s,instrs,fresh) = aux fresh s in
	  match e with
	    | Uninit _ -> e::s, instrs , fresh
	    | E e ->
		if test e then
		  let x = TempVar fresh in
		  let t = type_of_expr e in
		    E (Var (t,x))::s, (AffectVar (x,e))::instrs, fresh +1
		else 
		  E e::s, instrs, fresh
  in
  let (s,instrs,_) = aux (fresh_in_stack s) s in
    (s,instrs)

	  
let to_addr3_binop mode binop s instrs =
  match mode with 
    | Addr3 -> 	let x = TempVar (fresh_in_stack s)
      in begin
	let e = Binop (binop,topE (pop s),topE s) in
	  E (Var (type_of_expr e,x))::(pop2 s), instrs@[AffectVar(x,e)]
      end
    | _ -> E (Binop (binop,topE (pop s),topE s))::(pop2 s), instrs

let to_addr3_unop mode unop s instrs = 
  match mode with 
    | Addr3 -> let x = TempVar (fresh_in_stack s) in
	begin
	  let e = Unop (unop,topE s) in
	    E (Var (type_of_expr e,x))::(pop s), instrs@[AffectVar(x,e)]
	end
    | _ ->E (Unop (unop,topE s))::(pop s), instrs 

let make_tempvar s next_store =
  match next_store with
    | None -> TempVar (fresh_in_stack s)
    | Some x -> begin
	match is_var_in_stack x s with
	  | Some _ -> TempVar (fresh_in_stack s)
	  | None -> x
      end


(* Maps each opcode to a function of a stack that modifies its 
 * according to opcode, and returns grimp corresponding instructions if any 
 * tos : type operand stack
 * i : current index of bytecode
 * next : progression along instruction bytecode index 
 * mode : normal , flat , 3add 
 *)
let bc2bir_instr mode pp_var i bctype tos s next_store = function
  | OpNop -> s, []
  | OpConst x -> E (Const x)::s, []
  | OpLoad (_,n) ->
      E (Var (bctype i,OriginalVar (n,pp_var i n)))::s, []
  | OpArrayLoad _ -> 
      let a = topE (pop s) in
      let idx = topE s in 
	begin
	match mode with 
	  | Addr3 ->
	      let x = make_tempvar (pop2 s) next_store in 
		E (Var (type_of_array_content a,x))::(pop2 s), 
		[Check (CheckNullPointer a);Check (CheckArrayBound (a,idx));AffectVar (x,(Binop(ArrayLoad,a,idx)))]
	  | _ ->
	      E (Binop(ArrayLoad,a,idx))::(pop2 s), 
	      [Check (CheckNullPointer a);Check (CheckArrayBound (a,idx))]
	end
  | OpStore (_,n) ->
      let y = OriginalVar(n,pp_var i n) in
	begin
	  match topE s with
	    | Var (_,y') when var_equal y y' ->  (pop s,[]) 
	    | _ -> 
		begin
		  match is_var_in_stack y (pop s) with
		    | Some t ->
			let x = make_tempvar s None in
			  replace_var_in_stack y x (pop s), 
			  [AffectVar(x,Var (t,y)); AffectVar(y,topE s)]
		    | None ->
			(pop s,[AffectVar (y,topE s)]) 
		end
	end
  | OpIInc (a,b) ->
      let a = OriginalVar (a,pp_var i a) in
	begin
	  match is_var_in_stack a s with
	    | Some t ->
		let x = make_tempvar s None in
		  replace_var_in_stack a x s, 
		  [AffectVar(x,Var (t,a));
		   AffectVar (a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Const (`Int (Int32.of_int b))))]
	    | _ -> s,[AffectVar(a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Const (`Int (Int32.of_int b))))]
	end
  | OpPutField (c, f) -> 
      let r = topE (pop s) in
	clean (is_field_in_expr c f) (pop2 s) [Check (CheckNullPointer r); AffectField (r,c,f,topE s)]
  | OpArrayStore _ -> 
      let v = topE s in
      let a = topE (pop2 s) in	
      let idx = topE (pop s) in
      let inss = [Check (CheckNullPointer a); 
		  Check (CheckArrayBound (a,idx)); 
		  Check (CheckArrayStore (a,v)); 
		  AffectArray (a, idx, v)]
      in clean is_array_in_expr (pop3 s) inss	
  | OpPop -> pop s, []
  | OpPop2 -> 
      (match (top tos) with
	 | Op32 -> pop2 s, []
	 | Op64 -> pop s, [])
  | OpDup -> (top s)::s,[]
  | OpDupX1 -> (top s)::(top (pop s))::(top s)::(pop2 s), []
  | OpDupX2 -> 
	 (match (top (pop tos)) with
	    | Op32 -> 
		(top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s),[]
	    | Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s),[])
  | OpDup2 -> 
      (match (top tos) with
	 | Op32 -> (top s)::(top (pop s))::(top s)::(top (pop s))::(pop2 s),[]
	 | Op64 -> (top s)::s,[])
  | OpDup2X1 -> 
      (match (top tos) with
	 | Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s),[]
	 | Op64 ->  (top s)::(top (pop s))::(top s)::(pop2 s),[])
  | OpDup2X2 -> 
      (match (top tos) with
	 | Op32 ->
	     (match (top (pop2 tos)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top (pop3 s))::(top s)::(top (pop s))::(pop (pop3 s)),[]
		| Op64 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(top (pop s))::(pop3 s),[])
	 | Op64 ->
	     (match (top (pop tos)) with
		| Op32 -> (top s)::(top (pop s))::(top (pop2 s))::(top s)::(pop3 s),[]
		| Op64 -> (top s)::(top (pop s))::(top s)::(pop2 s),[]))
  | OpSwap -> (top (pop s))::(top s)::(pop2 s),[]
  | OpAdd k -> to_addr3_binop mode (Add k) s [] 
  | OpSub k -> to_addr3_binop mode (Sub k) s [] 
  | OpMult k -> to_addr3_binop mode (Mult k) s [] 
  | OpDiv k -> let q = topE s in to_addr3_binop mode (Div k) s [Check (CheckArithmetic q)] 
  | OpRem k -> let q = topE s in to_addr3_binop mode (Rem k) s [Check (CheckArithmetic q)]  
  | OpNeg k -> to_addr3_unop mode (Neg k) s []  
  | OpIShl ->  to_addr3_binop mode IShl s []  
  | OpIShr ->  to_addr3_binop mode IShr s []  
  | OpLShl ->  to_addr3_binop mode LShl s []  
  | OpLShr -> to_addr3_binop mode LShr s []  
  | OpIAnd -> to_addr3_binop mode IAnd s []  
  | OpIOr -> to_addr3_binop mode IOr s []  
  | OpIXor -> to_addr3_binop mode IXor s []  
  | OpIUShr -> to_addr3_binop mode IUshr s []  
  | OpLAnd -> to_addr3_binop mode LAnd s []  
  | OpLOr -> to_addr3_binop mode LOr s []  
  | OpLXor -> to_addr3_binop mode LXor s []  
  | OpLUShr  -> to_addr3_binop mode LUshr s []  
  | OpI2L -> to_addr3_unop mode (Conv I2L) s []  
  | OpI2F -> to_addr3_unop mode (Conv I2F) s []  
  | OpI2D ->to_addr3_unop mode (Conv I2D) s []  
  | OpL2I ->to_addr3_unop mode (Conv L2I) s []  
  | OpL2F ->to_addr3_unop mode (Conv L2F) s []  
  | OpL2D ->to_addr3_unop mode (Conv L2D) s []  
  | OpF2I ->to_addr3_unop mode (Conv F2I) s []  
  | OpF2L ->to_addr3_unop mode (Conv F2L) s []  
  | OpF2D ->to_addr3_unop mode (Conv F2D) s []  
  | OpD2I ->to_addr3_unop mode (Conv D2I) s []  
  | OpD2L ->to_addr3_unop mode (Conv D2L) s []  
  | OpD2F ->to_addr3_unop mode (Conv D2F) s []  
  | OpI2B ->to_addr3_unop mode (Conv I2B) s []  
  | OpI2C ->to_addr3_unop mode (Conv I2C) s []  
  | OpI2S ->to_addr3_unop mode (Conv I2S) s []  
  | OpCmp op -> 
      (match op with 
	 | `DG -> to_addr3_binop mode (CMP DG) s [] 
	 | `DL -> to_addr3_binop mode (CMP DL) s [] 
	 | `FG -> to_addr3_binop mode (CMP FG) s [] 
	 | `FL ->to_addr3_binop mode (CMP FL) s [] 
	 | `L ->to_addr3_binop mode (CMP L) s [] 
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
	pop s, [Ifd (guard,target)]
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
	pop2 s, [Ifd (guard,target)]
  | OpGoto n -> s, [Goto (n+i)]
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
		  [Goto (def+i)])
  | OpLookupSwitch (def,pairs) -> 
      pop s, 
      (List.fold_right
		  (fun (c,j) l -> (Ifd ((`Eq,topE s,Const (`Int c)),j+i))::l)
		  pairs
		  [Goto (def+i)])
  | OpReturn k -> [],
      (match k with
	 | `Void -> [Return None]
	 | _ -> [Return (Some (topE s))])
  | OpGetField (c, f) -> 
      let r = topE s in
	begin
	  match mode with 
	    | Normal -> E (Field (r,c,f))::(pop s), [Check (CheckNullPointer r)]
	    | _ ->
		let x = make_tempvar s next_store in
		  E (Var (fs_type f,x))::(pop s), 
		[Check (CheckNullPointer r);AffectVar(x,Field (r,c,f))]
	end
  | OpGetStatic (c, f) -> E (StaticField (c, f))::s, [MayInit c]
  | OpPutStatic (c, f) -> 
      if is_static_in_stack c f (pop s) then begin
	let x = make_tempvar s None in
	  replace_static_in_stack c f x (pop s), 
	[MayInit c;
	 AffectVar(x,StaticField(c,f));
	 AffectStaticField (c,f,topE s)]
      end else 
	pop s, [AffectStaticField (c, f,topE s)]
  | OpInvoke (x, ms) -> 
      begin
	(match x with
	   | `Static c -> 
	       (match ms_rtype ms with
		  | None -> 
		      clean is_heap_sensible_element_in_expr
			(popn (List.length (ms_args ms)) s) 
			[InvokeStatic (None,c,ms,param (List.length  (ms_args ms)) s)]
		  | Some t ->
		      let x = make_tempvar s next_store in
		      clean is_heap_sensible_element_in_expr
			(E (Var (t,x))::(popn (List.length (ms_args ms)) s)) 
			[InvokeStatic (Some x,c,ms,param (List.length (ms_args ms)) s)])
	   | x -> 
	       begin
		 let popn_s = popn (List.length (ms_args ms)) s in
		   (match top popn_s  with
		      | Uninit (c,j) ->
			  let x = make_tempvar s next_store in
			  let e' = E (Var (TObject (TClass java_lang_object),x)) in
			    clean is_heap_sensible_element_in_expr 
			      (List.map 
				 (function e -> if e = Uninit (c,j) then e' else e)
				 (pop popn_s))
			      [New (x,c,ms_args ms,param (List.length (ms_args ms)) s)]
		      | E e0  ->
			  let nb_args = List.length (ms_args ms) in
			  let s_next = pop popn_s in
			  let this = topE popn_s in
			  let ins target = 
			    begin match x with
				| `Virtual o -> [InvokeVirtual (target,this,VirtualCall o,ms,param nb_args s)]
				| `Interface c -> [InvokeVirtual (target,this,InterfaceCall c,ms,param nb_args s)]  
				| `Special c -> [InvokeNonVirtual (target,this,c,ms,param nb_args s)]
				| `Static _ -> assert false (* already treated above *) 
			    end
			  in
			    (match ms_rtype ms with
			       | None -> 
				   clean is_heap_sensible_element_in_expr s_next ([Check (CheckNullPointer e0)]@(ins None))
			       | Some t -> 
				   let y = make_tempvar s next_store in 
				     clean is_heap_sensible_element_in_expr (E (Var (t,y))::s_next) ([Check (CheckNullPointer e0)]@(ins (Some y)))
			    )) 
	       end)
	end
  | OpNew c -> (Uninit (c,i))::s, [MayInit c]
  | OpNewArray t -> 
      let x = make_tempvar s next_store in
      let dim = topE s in
	E (Var (TObject (TArray t),x))::(pop s), [Check (CheckNegativeArraySize dim); NewArray (x,t,[dim])]
  | OpArrayLength -> 
      let a = topE s in begin
	  match mode with 
	    | Addr3 -> 
		let x = make_tempvar s next_store in
		  E (Var (TBasic `Int,x))::(pop s), [Check (CheckNullPointer a);AffectVar(x,Unop (ArrayLength,a))]
	    | _ -> E (Unop (ArrayLength,a))::(pop s),[Check (CheckNullPointer a)]
	end
  | OpThrow -> 
      let r = topE s in [], [Check (CheckNullPointer r); Throw r]
  | OpCheckCast _ -> s, [Check (CheckCast (topE s))]
  | OpInstanceOf c -> to_addr3_unop mode (InstanceOf c) s [] 
  | OpMonitorEnter -> 
      let r = topE s in
	pop s, [Check (CheckNullPointer r); MonitorEnter r]
  | OpMonitorExit -> 
      let r = topE s in
	pop s, [Check (CheckNullPointer r); MonitorExit r]
  | OpAMultiNewArray (cn,dim) -> 
      let x = make_tempvar s next_store in
      let params = param dim s in
	E (Var (TObject cn,x))::(popn dim s), 
	(List.map (fun e -> Check (CheckNegativeArraySize e)) params)
	@[NewArray (x,remove_dim cn dim,params)]
  | OpBreakpoint -> failwith "breakpoint"  
  | OpInvalid -> failwith "invalid"
      
let is_jump_instr = function
  | OpIfCmp _ 
  | OpTableSwitch _
  | OpLookupSwitch _ 
  | OpIf _ 
  | OpGoto _ -> true
  | _ -> false

module MapPc = Map.Make(struct type t=int let compare = compare end)

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
	      let l = List.map (fun j -> AffectVar (BranchVar (j,i),Var (type_of_expr e,x))) succs in
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

let jump_stack pc' stack =
  let rec aux i = function
      [] -> []
    | e::q -> begin
	match e with 
	  | Uninit _ -> 
	      e :: (aux (i+1) q)
	  | E e -> 
	      E (Var (type_of_expr e,BranchVar (pc',i))) :: (aux (i+1) q)
      end
  in aux 0 stack
       
let fold_ir f a ir = 
  List.fold_left
    (fun s (pc,instrs) ->
       List.fold_left 
	 (fun s ins -> f s pc ins)
	 s instrs
    ) a ir

type tempvar_stats = {
  stat_nb_total : int;
  stat_nb_branchvar : int;
  stat_nb_branchvar2 : int;
}
    

module SetInt2 = Set.Make(struct type t = int*int let compare = compare end)

let make_tempvar_stats ir = 
  let (nb_tempvar_branch,_) = 
    fold_ir
      (fun (n,s) _ ->
	 function 
	   | (AffectVar (BranchVar (i,j),_)) when (not (SetInt2.mem (i,j) s))-> (n+1,SetInt2.add (i,j) s)
	   | _ -> (n,s))
      (0,SetInt2.empty) ir in
  let (nb_tempvar_branch2,_) = 
    fold_ir
      (fun (n,s) _ ->
	 function 
	   | (AffectVar (BranchVar2 (i,j),_)) when (not (SetInt2.mem (i,j) s))-> (n+1,SetInt2.add (i,j) s)
	   | _ -> (n,s))
      (0,SetInt2.empty) ir in
  let (nb_tempvar_not_branch,_) = 
    fold_ir
      (fun (n,s) _ ->
	 function 
	   | (AffectVar (TempVar i,_)) when (not (Ptset.mem i s))-> (n+1,Ptset.add i s)
	   | _ -> (n,s))
      (0,Ptset.empty) ir in
    {
      stat_nb_total = nb_tempvar_not_branch + nb_tempvar_branch + nb_tempvar_branch2;
      stat_nb_branchvar = nb_tempvar_branch;
      stat_nb_branchvar2 = nb_tempvar_branch2;
    }
    
exception NonemptyStack_backward_jump
exception Type_constraint_on_Uninit
exception Content_constraint_on_Uninit


let value_compare e1 e2 = 
  match e1, e2 with 
    | Var(_,x), Var(_,y) -> var_equal x y
    | _ -> e1 = e2

let value_compare e1 e2 = 
  match e1, e2 with 
    | Uninit _, Uninit _ -> e1 = e2
    | E e1, E e2 -> value_compare e1 e2
    | _ -> false

let value_compare_stack s1 s2 =
  List.for_all2 value_compare s1 s2


let bc2ir flat pp_var jump_target bctype code =
  let rec loop as_ts_jump ins ts_in as_in pc =
    
    (* Simplifying redundant assignt on the fly : see one instr ahead *)
    let next_store =
      let next_pc = try next code.c_code pc with End_of_method -> pc in
	match code.c_code.(next_pc) with
	  | OpStore (_,n) -> if jump_target.(next_pc) then None else Some (OriginalVar (n,pp_var next_pc n))
	  | _ -> None
    in
    let succs = normal_next code pc in
    let (ts_in,as_in) =
      if jump_target.(pc) then
	try MapPc.find pc as_ts_jump
	with Not_found -> 
	  (* no predecessor of pc have been visited before *)
	  if List.exists (fun e -> pc = e.e_handler) code.c_exc_tbl then
	    (* this is a handler point *)
	    ([Op32],[E (Var (TObject (TClass java_lang_object),catch_var))])
	  else
	    (* this is a back jump target *)
	    ([],[])
      else (ts_in,as_in)
    in 
    let ts_out = type_next code.c_code.(pc) ts_in in
    let (as_out,instrs) = bc2bir_instr flat pp_var pc bctype ts_in as_in next_store code.c_code.(pc)  in
      
      (* fail on backward branchings on a non-empty stack *)
      if List.length as_out>0 then  
	if (List.exists (fun j -> j<pc) succs) then raise NonemptyStack_backward_jump;
    
    let jump_succs = List.filter (fun i -> jump_target.(i)) succs in
    let branch_assigns = para_assign pc jump_succs as_out in
    let ins =
      if is_jump_instr code.c_code.(pc) then
	(pc,branch_assigns@instrs)::ins
      else
	(pc,instrs@branch_assigns)::ins
    in
    let as_ts_jump =       
      List.fold_left 
	(fun as_jump pc' ->
	   try 
	     let (ts_jmp,as_jmp) = MapPc.find pc' as_jump in
	       (* check constraint on expr uninit and jumping forward on a non empty stack 
		  all defined predecessor advice must match what is reached *)
	       if (ts_jmp <> ts_out) then raise Type_constraint_on_Uninit ;
	       let jmp_s =  jump_stack pc' as_out in
		 if (not (value_compare_stack as_jmp jmp_s)) then 
		   ( Printf.printf "\n %s\n" (string_of_int pc') ;
		     Printf.printf "%s \n" (print_stackmap as_jmp) ;
		     Printf.printf "%s \n" (print_stackmap jmp_s) ;
		     assert false ) (*raise Content_constraint_on_Uninit*)
		 else as_jump
	   with Not_found -> 
	     (* when first advice for pc', no constraint to check. add the advice in the map *)
	     let st = jump_stack pc' as_out in
	       MapPc.add pc' (ts_out,st) as_jump)
	as_ts_jump
	jump_succs in
      try
	loop as_ts_jump ins ts_out as_out (next code.c_code pc) 
      with End_of_method ->  ins
  in 
    loop MapPc.empty [] [] [] 0 
      
let varname = Cmn.varname

let search_name_localvar static code i x = 
  if x=0 && (not static) then Some "this"
  else match JCode.get_local_variable_info x i code with
    | None ->  None
    | Some (s,_) -> Some s

let bcvar i = OriginalVar (i,None)

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
    mapi
      (fun i t -> t, OriginalVar (i,pp_var 0 i))
      (fun i t -> match convert_field_type t with Op32 -> i+1 | Op64 -> i+2)
      (ms_args cm.cm_signature)
  else 
    (TObject (TClass java_lang_object ), OriginalVar (0,pp_var 0 0))::
      (mapi
	 (fun i t -> t, OriginalVar (i+1,pp_var 0 (i+1)))
	 (fun i t -> match convert_field_type t with Op32 -> i+1 | Op64 -> i+2)
	 (ms_args cm.cm_signature))

let compress_ir ir jump_target =
  let rec aux0 pc0 = function
    | [] -> [pc0,[Nop]]
    | (pc,instrs)::q when jump_target.(pc) -> (pc0,[Nop])::(pc,instrs)::(aux q)
    | (_,[])::q -> aux0 pc0 q
    | (_,instrs)::q -> (pc0,instrs)::(aux q)
  and aux = function
    | [] -> []
    | (pc,[])::q -> aux0 pc q
    | (pc,instrs)::q -> (pc,instrs)::(aux q)
  in aux ir

let jcode2bir mode compress cm jcode =
  let code = jcode in
    (*    Array.iteri
	  (fun i op ->
	  match op with
	  OpLoad (_,x) 
	  | OpStore (_,x) -> 
	  Printf.printf "%s at line %d, var name is %s\n" (JDump.opcode op) i 
	  (match JCode.get_local_variable_info x i code with | None -> "?" | Some (s,_) -> s)
	  | _ -> ()
	  ) code.c_code; *)
  let pp_var = search_name_localvar cm.cm_static code in
  let jump_target = compute_jump_target code in
  let bctype = BCV.run false cm jcode in
  let res = bc2ir mode pp_var jump_target bctype code in 
    { params = gen_params pp_var cm;
      code = if compress then compress_ir (List.rev res) jump_target else (List.rev res);
      exc_tbl = code.c_exc_tbl;
      line_number_table = code.c_line_number_table;
      jump_target = jump_target }
      
let transform ?(compress=false) = jcode2bir Normal compress 
let transform_flat ?(compress=false) = jcode2bir Flat compress 
let transform_addr3 ?(compress=false) = jcode2bir Addr3 compress 

