(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

open Javalib_pack
open Javalib
open JBasics
open JCode 

include Cmn

(*********** TYPES *************)

type binop =
  | ArrayLoad of JBasics.value_type
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl | IShr  | IAnd | IOr  | IXor | IUshr
  | LShl | LShr | LAnd | LOr | LXor | LUshr
  | CMP of comp

type expr =
  | Const of const
  | Var of JBasics.value_type * var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * JBasics.class_name * field_signature
  | StaticField of JBasics.class_name * field_signature

type opexpr =
  | Uninit of JBasics.class_name * int
  | E of expr


type virtual_call_kind =
  | VirtualCall of object_type
  | InterfaceCall of JBasics.class_name

type check =
  | CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr * object_type
  | CheckArithmetic of expr

type instr =
  | Nop
  | AffectVar of var * expr
  | AffectArray of expr * expr * expr
  | AffectField of expr * JBasics.class_name * field_signature * expr
  | AffectStaticField of JBasics.class_name * field_signature * expr
  | Goto of int
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr ) * int
  | Throw of expr
  | Return of expr option
  | New of var * JBasics.class_name * JBasics.value_type list * (expr list)
  | NewArray of var * JBasics.value_type * (expr list)
  | InvokeStatic
      of var option * JBasics.class_name * method_signature * expr list
  | InvokeVirtual
      of var option * expr * virtual_call_kind * method_signature * expr list
  | InvokeNonVirtual
      of var option * expr * JBasics.class_name * method_signature * expr list
  | MonitorEnter of expr
  | MonitorExit of expr
  | MayInit of JBasics.class_name
  | Check of check

type t = {
  vars : var array;  (** All variables that appear in the method. [vars.(i)] is the variable of index [i]. *)
  params : (JBasics.value_type * var) list;
  code : instr array;
  exc_tbl : exception_handler list;
  line_number_table : (int * int) list option;
  pc_bc2ir : int Ptmap.t;
  pc_ir2bc : int array; 
  jump_target : bool array
}

(* For stack type inference only *)
type op_size = Op32 | Op64

(************* PRINT ************)

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

let rec print_expr ?(show_type=true) first_level = function
  | Var (t,x) -> 
      if show_type then Printf.sprintf "%s:%s" (var_name_g x) (print_typ t)
      else (var_name_g x)
  | Field (e,c,f) -> Printf.sprintf "%s.%s" (print_expr ~show_type:show_type false e) (print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Const i -> print_const i
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_expr ~show_type:show_type false e)
  | Unop (Cast t,e) -> Printf.sprintf "(%s)%s" (print_unop (Cast t)) (print_expr ~show_type:show_type true e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_expr ~show_type:show_type true e)
  | Binop (ArrayLoad t,e1,e2) -> 
      if show_type then Printf.sprintf "%s[%s]:%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type true e2) (print_typ t)
      else Printf.sprintf "%s[%s]" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type true e2)
  | Binop (Add _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s+%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (Sub _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s-%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (Mult _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s*%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (Div _,e1,e2) -> bracket first_level
      (Printf.sprintf "%s/%s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2))
  | Binop (op,e1,e2) -> Printf.sprintf "%s(%s,%s)" (print_binop op) (print_expr ~show_type:show_type true e1) (print_expr ~show_type:show_type true e2)

let print_cmp ?(show_type=true) (c,e1,e2) =
  match c with
    | `Eq -> Printf.sprintf "%s == %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Ne -> Printf.sprintf "%s != %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Lt -> Printf.sprintf "%s < %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Ge -> Printf.sprintf "%s >= %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Gt -> Printf.sprintf "%s > %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)
    | `Le -> Printf.sprintf "%s <= %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type false e2)

let print_oexpr ?(show_type=true) = function
  | Uninit (c,i) -> Printf.sprintf "Unit(%d,%s)" i (JPrint.class_name c)
  | E e -> print_expr ~show_type:show_type true e

let print_stackmap ?(show_type=true) = function
  | [] -> ""
  | x::q -> List.fold_left (fun s t -> Printf.sprintf "%s :: %s" (print_oexpr ~show_type:show_type t) s) (print_oexpr ~show_type:show_type x) q

let print_instr ?(show_type=true) = function
  | Nop -> "nop"
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr ~show_type:show_type true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr ~show_type:show_type true e)
  | AffectField (e1,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_expr ~show_type:show_type false e1) (print_field c f) (print_expr ~show_type:show_type true e2)
  | AffectArray (e1,e2,e3) -> Printf.sprintf "%s[%s] := %s" (print_expr ~show_type:show_type false e1) (print_expr ~show_type:show_type true e2) (print_expr ~show_type:show_type true e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | Throw e -> Printf.sprintf "throw %s" (print_expr ~show_type:show_type false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr ~show_type:show_type false e)
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_expr ~show_type:show_type true e)) le)
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s)" (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s)" (var_name_g x) (JPrint.class_name c) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | InvokeVirtual (r,e1,_,ms,le) ->
      Printf.sprintf "%s%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr ~show_type:show_type false e1) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | InvokeNonVirtual (r,e1,kd,ms,le) ->
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr ~show_type:show_type false e1) (JPrint.class_name kd) (ms_name ms) (print_list_sep "," (print_expr ~show_type:show_type true) le)
  | MonitorEnter e -> Printf.sprintf "monitorenter(%s)" (print_expr ~show_type:show_type true e)
  | MonitorExit e -> Printf.sprintf "monitorexit(%s)" (print_expr ~show_type:show_type true e)
  | MayInit c -> Printf.sprintf "mayinit %s" (JPrint.class_name c)
  | Check c ->
      begin
	match c with
	    CheckNullPointer e -> Printf.sprintf "notnull %s" (print_expr ~show_type:show_type true e)
	  | CheckArrayBound (a,i) -> Printf.sprintf "checkbound %s[%s]"  (print_expr ~show_type:show_type true a) (print_expr ~show_type:show_type true i)
	  | CheckArrayStore (a,v) -> Printf.sprintf "checkstore %s[] <- %s"  (print_expr ~show_type:show_type true a) (print_expr ~show_type:show_type true v)
	  | CheckNegativeArraySize e -> Printf.sprintf "checknegsize %s" (print_expr ~show_type:show_type true e)
	  | CheckCast (e,t) -> Printf.sprintf "checkcast %s:%s" (print_expr ~show_type:show_type true e) (JDumpBasics.object_value_signature t)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_expr ~show_type:show_type true e)
      end

let print_handler exc = 
  Printf.sprintf "      [%d-%d] -> %d (%s %s)" exc.e_start exc.e_end exc.e_handler
    (match exc.e_catch_type with
       | None -> "_"
       | Some cl -> JPrint.class_name cl)
    (var_name_g exc.e_catch_var)

let rec print_code code i acc =
  if i<0 then acc
  else print_code code (i-1) (Printf.sprintf "%3d: %s" i (print_instr code.(i))::acc)


let print m =
  let size = Array.length (m.code) in
    print_code m.code (size-1) []

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

(* Extracts, when possible, the expression from a stack element *)
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
  | `Int  | `Short   | `Char
  | `Byte | `Int2Bool| `ByteBool
  | `Bool | `Float   | `Object -> Op32
  | `Long | `Double -> Op64

let convert_const = function
  | `String _  | `Class _   | `ANull
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
  let handlers = List.filter (fun e -> e.JCode.e_start <= i && i < e.JCode.e_end) handlers in
  let handlers = List.map (fun e -> e.JCode.e_handler) handlers in
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
  | Array of JBasics.value_type

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


exception GetNotFound (* this exn is never raised *)
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
  | `Long 
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
  | OpIf(_, _) -> (fun (s,l) -> pop s, l)
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

let print_instr i ins =
  JPrint.jopcode ~jvm:true
    (match ins with
       | OpIf (t, n) -> OpIf (t,n+i)
       | OpIfCmp (t, n) -> OpIfCmp (t,n+i)
       | OpGoto n -> OpGoto (i+n)
       | OpTableSwitch (default, low, high, table) ->
	   OpTableSwitch (default+i, low, high,Array.map ((+)i) table)
       | OpLookupSwitch (default, npairs) ->
	   OpLookupSwitch (default+i,List.map (fun (x,y) -> (x,y+i)) npairs)
       | _ -> ins)

let print_result cm types code =
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

let run verbose cm code =
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
      match types.(i) with
	  Some sl -> 
	    let sl' = next i code.c_code.(i) sl in
	    let ws = List.fold_left (upd sl') ws (normal_next code i) in
	    let sl' = ([Object],snd sl') in
	    let ws = List.fold_left (upd sl') ws (compute_handlers code i) in
	      loop ws
	| None -> loop ws
  in
    assert ((Array.length types)>0);
    types.(0) <- Some (init cm);
    (try loop ws
     with
	 GetNotFound -> Printf.printf "GET_NOT_FOUND !\n"; assert false
       | BadLoadType -> Printf.printf "BAD_LOAD_TYPE !\n"
       | ArrayContent -> assert false);
    if verbose then print_result cm types code;
    (fun i ->
       match code.c_code.(i) with
	 | OpLoad (_,n) ->
	     (match types.(i) with
		| Some (_, l) -> to_value_type (get l n)
		| _ -> assert false)
	 | _ -> assert false),
    (fun _ i -> 
       match code.c_code.(i) with
	 | OpArrayLoad _ ->
	     (match types.(i) with
		| Some (_::(Array t)::_, _) -> t
		| _ -> assert false)
	 | _ -> assert false)


let array_type_to_value_type = function
  | `Long -> TBasic `Long
  | `Float -> TBasic `Float
  | `Double -> TBasic `Double
  | `Int2Bool -> TBasic `Int
  | `Object -> TObject (TClass java_lang_object)

let run_dummy code =
  (fun i ->
     match code.c_code.(i) with
       | OpLoad (t,_) -> 
	   (match t with
	      | `Long -> TBasic `Long
	      | `Float -> TBasic `Float
	      | `Double -> TBasic `Double
	      | `Int2Bool -> TBasic `Int
	      | `Object -> TObject (TClass java_lang_object))
       | _ -> assert false),
  (fun t _ -> (match t with
		 | `Long -> TBasic `Long
		 | `Float -> TBasic `Float
		 | `Double -> TBasic `Double
		 | `Int -> TBasic `Int
		 | `Short -> TBasic `Short
		 | `Char ->  TBasic `Char
		 | `ByteBool -> TBasic `Byte
		 | `Int2Bool -> TBasic `Int
		 | `Object -> TObject (TClass java_lang_object)))
	 
let run bcv ?(verbose=false) cm code =
  if bcv then run verbose cm code
  else (run_dummy code)
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
  | Unop (Cast t,_) -> TObject t
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
	   | InstanceOf _ -> `Int
	   | _ -> assert false)
  | Binop (ArrayLoad t,_,_) -> t
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

let is_arrayload = function
  | ArrayLoad _ -> true
  | _ -> false

let is_in_expr test_var test_static test_field test_array =
  let rec aux expr =
    match expr with
      | Const _ -> false
      | StaticField (c,f) -> test_static c f
      | Field (e,c,f) -> test_field c f || aux e
      | Var (_,x) -> test_var x
      | Unop(_,e) -> aux e
      | Binop(s,e1,e2) -> (test_array && is_arrayload s) || aux e1 || aux e2
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
  var_in_stack ((=) x) stack

let is_static_in_stack c f stack =
  is_in_stack (fun _ -> false) (fun c0 f0 -> c=c0 && f=f0) stack

let is_field_in_expr c f expr =
  is_in_expr (fun _ -> false) (fun _ _ -> false) (fun c0 f0 -> c=c0 && f=f0) false expr

let is_array_in_expr =
  is_in_expr (fun _ -> false) (fun _ _ -> false)  (fun _ _ -> false) true

let is_var_in_expr_not_var x e = (* warning we use ocaml equality *)
  is_in_expr ((=) x) (fun _ _ -> false)  (fun _ _ -> false) true e

let is_heap_sensible_element_in_expr expr =
  is_in_expr (fun _ -> false) (fun _ _ -> true) (fun _ _ -> true) true expr

let replace_var_in_expr x y =
  replace_in_expr ((=) x) (fun _ _ -> false) y

let replace_var_in_stack x y stack =
  replace_in_stack ((=) x) (fun _ _ -> false) y stack

let replace_static_in_stack c f y stack =
  replace_in_stack (fun _ -> false) (fun c0 f0 -> c=c0 && f=f0) y stack


let temp_in_expr acc expr =
  let rec aux acc expr =
    match expr with
      | Const _
      | StaticField _ -> acc
      | Field (e,_,_) -> aux acc e
      | Var (_,(_,x)) ->
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

let fresh_in_stack ssa fresh_counter s =
  if ssa then 
    let x = !fresh_counter in
      incr fresh_counter; x
  else
    let set = temp_in_stack s in
      if Ptset.is_empty set then 0
      else (Ptset.max_elt set)  +1

let clean dico ssa fresh_counter test s instrs =
  if ssa then
    let rec aux = function
      | [] -> [], instrs
      | e::s ->
	  let (s,instrs) = aux s in
	    match e with
	      | Uninit _ -> e::s, instrs 
	      | E e ->
		  if test e then
		    let x = make_var dico (TempVar !fresh_counter) in
		    let _ = incr fresh_counter in
		    let t = type_of_expr e in
		      E (Var (t,x))::s, (AffectVar (x,e))::instrs
		  else
		    E e::s, instrs
    in
      aux s 
  else
    let rec aux fresh = function
      | [] -> [], instrs, fresh
      | e::s ->
	  let (s,instrs,fresh) = aux fresh s in
	    match e with
	      | Uninit _ -> e::s, instrs , fresh
	      | E e ->
		  if test e then
		    let x = make_var dico (TempVar fresh) in
		    let t = type_of_expr e in
		      E (Var (t,x))::s, (AffectVar (x,e))::instrs, fresh +1
		  else
		    E e::s, instrs, fresh
    in
    let (s,instrs,_) = aux (fresh_in_stack ssa fresh_counter s) s in
      (s,instrs)

let to_addr3_binop dico mode binop ssa fresh_counter s =
  match mode with
    | Addr3 -> 	let x = make_var dico (TempVar (fresh_in_stack ssa fresh_counter s))
      in 
	(match binop with 
	   | Div _ 
	   | Rem _ ->
	       begin
		 let q = topE s in 
		 let e = Binop (binop,topE (pop s),topE s) in
		   E (Var (type_of_expr e,x))::(pop2 s), [Check (CheckArithmetic q) ; AffectVar(x,e)]
	       end
	   | _ ->
	       begin
		 let e = Binop (binop,topE (pop s),topE s) in
		   E (Var (type_of_expr e,x))::(pop2 s), [AffectVar(x,e)]
	       end)
    | _ -> E (Binop (binop,topE (pop s), topE s))::(pop2 s), []
	
let to_addr3_unop dico mode unop ssa fresh_counter s instrs =
  match mode with
    | Addr3 -> let x = make_var dico (TempVar (fresh_in_stack ssa fresh_counter s)) in
	begin
	  let e = Unop (unop,topE s) in
	    E (Var (type_of_expr e,x))::(pop s), instrs@[AffectVar(x,e)]
	end
    | _ ->E (Unop (unop,topE s))::(pop s), instrs

let make_tempvar dico ssa fresh_counter s next_store =
  match next_store with
    | None -> make_var dico (TempVar (fresh_in_stack ssa fresh_counter s))
    | Some x -> begin
	match is_var_in_stack x s with
	  | Some _ -> make_var dico (TempVar (fresh_in_stack ssa fresh_counter s))
	  | None -> x
      end

and type_of_array_content t e =
  match type_of_expr e with
    | TObject (TArray t) -> t (* can this happen ? *)
    | _ -> (* we use the type found in the OpArrayLoad argument *)
	(match t with
	   | `Int | `Short | `Char | `ByteBool -> TBasic `Int
	   | `Long -> TBasic `Long
	   | `Float -> TBasic `Float
	   | `Double -> TBasic `Double
	   | `Object -> TObject (TClass java_lang_object))

(* Maps each opcode to a function of a stack that modifies its
 * according to opcode, and returns corresponding instructions if any
 * TODO : comment arguments...
 *)
let bc2bir_instr dico mode pp_var ssa fresh_counter i load_type arrayload_type tos s next_store = function
  | OpNop -> s, []
  | OpConst c -> 
      begin
	match c with
	  | `Class _ | `String _ -> 
	      let x = make_tempvar dico ssa fresh_counter s next_store in
		E (Var (TObject (TClass java_lang_object),x))::s,
		[AffectVar(x,Const c)]
	  | _ -> E (Const c)::s, []
      end
  | OpLoad (_,n) ->
      E (Var (load_type i,make_var dico (OriginalVar (n,pp_var i n))))::s, []
  | OpArrayLoad t ->
      let a = topE (pop s) in
      let idx = topE s in
      let t = arrayload_type t i in
	begin
	match mode with
	  | Addr3 ->
	      let x = make_tempvar dico ssa fresh_counter (pop2 s) next_store in
		E (Var (t,x))::(pop2 s),
		[Check (CheckNullPointer a);Check (CheckArrayBound (a,idx));AffectVar (x,(Binop(ArrayLoad t,a,idx)))]
	  | _ ->
	      E (Binop(ArrayLoad t,a,idx))::(pop2 s),
	      [Check (CheckNullPointer a);Check (CheckArrayBound (a,idx))]
	end
  | OpStore (_,n) ->
      let y = make_var dico (OriginalVar(n,pp_var i n)) in
	begin
	  match topE s with
	    | Var (_,y') when index y = index y' ->  (pop s,[])
	    | _ ->
		begin
		  match is_var_in_stack y (pop s) with
		    | Some t ->
			let x = make_tempvar dico ssa fresh_counter s None in
			  replace_var_in_stack y x (pop s),
			  [AffectVar(x,Var (t,y)); AffectVar(y,topE s)]
		    | None ->
			(pop s,[AffectVar (y,topE s)])
		end
	end
  | OpIInc (a,b) ->
      let a = make_var dico (OriginalVar (a,pp_var i a)) in
	begin
	  match is_var_in_stack a s with
	    | Some t ->
		let x = make_tempvar dico ssa fresh_counter s None in
		  replace_var_in_stack a x s,
		  [AffectVar(x,Var (t,a));
		   AffectVar (a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Const (`Int (Int32.of_int b))))]
	    | _ -> s,[AffectVar(a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Const (`Int (Int32.of_int b))))]
	end
  | OpPutField (c, f) ->
      let r = topE (pop s) in
	clean dico ssa fresh_counter (is_field_in_expr c f) (pop2 s) [Check (CheckNullPointer r); AffectField (r,c,f,topE s)]
  | OpArrayStore _ ->
      let v = topE s in
      let a = topE (pop2 s) in
      let idx = topE (pop s) in
      let inss = [Check (CheckNullPointer a);
		  Check (CheckArrayBound (a,idx));
		  Check (CheckArrayStore (a,v));
		  AffectArray (a, idx, v)]
      in clean dico ssa fresh_counter is_array_in_expr (pop3 s) inss
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
  | OpAdd k -> to_addr3_binop dico mode (Add k) ssa fresh_counter s
  | OpSub k -> to_addr3_binop dico mode (Sub k) ssa fresh_counter s 
  | OpMult k -> to_addr3_binop dico mode (Mult k) ssa fresh_counter s 
  | OpDiv k ->  to_addr3_binop dico mode (Div k) ssa fresh_counter s
  | OpRem k -> to_addr3_binop dico mode (Rem k) ssa fresh_counter s 
  | OpNeg k -> to_addr3_unop dico mode (Neg k) ssa fresh_counter s []  
  | OpIShl ->  to_addr3_binop dico mode IShl ssa fresh_counter s 
  | OpIShr ->  to_addr3_binop dico mode IShr ssa fresh_counter s
  | OpLShl ->  to_addr3_binop dico mode LShl ssa fresh_counter s
  | OpLShr -> to_addr3_binop dico mode LShr ssa fresh_counter s 
  | OpIAnd -> to_addr3_binop dico mode IAnd ssa fresh_counter s 
  | OpIOr -> to_addr3_binop dico mode IOr ssa fresh_counter s
  | OpIXor -> to_addr3_binop dico mode IXor ssa fresh_counter s 
  | OpIUShr -> to_addr3_binop dico mode IUshr ssa fresh_counter s
  | OpLAnd -> to_addr3_binop dico mode LAnd ssa fresh_counter s
  | OpLOr -> to_addr3_binop dico mode LOr ssa fresh_counter s 
  | OpLXor -> to_addr3_binop dico mode LXor ssa fresh_counter s 
  | OpLUShr  -> to_addr3_binop dico mode LUshr ssa fresh_counter s 
  | OpI2L -> to_addr3_unop dico mode (Conv I2L) ssa fresh_counter s []  
  | OpI2F -> to_addr3_unop dico mode (Conv I2F) ssa fresh_counter s []  
  | OpI2D ->to_addr3_unop dico mode (Conv I2D) ssa fresh_counter s []  
  | OpL2I ->to_addr3_unop dico mode (Conv L2I) ssa fresh_counter s []  
  | OpL2F ->to_addr3_unop dico mode (Conv L2F) ssa fresh_counter s []  
  | OpL2D ->to_addr3_unop dico mode (Conv L2D) ssa fresh_counter s []  
  | OpF2I ->to_addr3_unop dico mode (Conv F2I) ssa fresh_counter s []  
  | OpF2L ->to_addr3_unop dico mode (Conv F2L) ssa fresh_counter s []  
  | OpF2D ->to_addr3_unop dico mode (Conv F2D) ssa fresh_counter s []  
  | OpD2I ->to_addr3_unop dico mode (Conv D2I) ssa fresh_counter s []  
  | OpD2L ->to_addr3_unop dico mode (Conv D2L) ssa fresh_counter s []  
  | OpD2F ->to_addr3_unop dico mode (Conv D2F) ssa fresh_counter s []  
  | OpI2B ->to_addr3_unop dico mode (Conv I2B) ssa fresh_counter s []  
  | OpI2C ->to_addr3_unop dico mode (Conv I2C) ssa fresh_counter s []  
  | OpI2S ->to_addr3_unop dico mode (Conv I2S) ssa fresh_counter s []  
  | OpCmp op -> 
      (match op with 
	 | `DG -> to_addr3_binop dico mode (CMP DG) ssa fresh_counter s 
	 | `DL -> to_addr3_binop dico mode (CMP DL) ssa fresh_counter s 
	 | `FG -> to_addr3_binop dico mode (CMP FG) ssa fresh_counter s 
	 | `FL ->to_addr3_binop dico mode (CMP FL) ssa fresh_counter s 
	 | `L ->to_addr3_binop dico mode (CMP L) ssa fresh_counter s 
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
		let x = make_tempvar dico ssa fresh_counter s next_store in
		  E (Var (fs_type f,x))::(pop s),
		[Check (CheckNullPointer r);AffectVar(x,Field (r,c,f))]
	end
  | OpGetStatic (c, f) ->
      begin
	  match mode with 
	    | Addr3 -> 
		let x = make_tempvar dico ssa fresh_counter s next_store in
		  E (Var (fs_type f,x))::s, [MayInit c ; AffectVar(x,StaticField (c,f))]
	    | _ -> E (StaticField (c, f))::s, [MayInit c]
	    
	end
  | OpPutStatic (c, f) ->
      if is_static_in_stack c f (pop s) then begin
	let x = make_tempvar dico ssa fresh_counter s None in
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
		      let params = param (List.length  (ms_args ms)) s in
		      clean dico ssa fresh_counter is_heap_sensible_element_in_expr
			(popn (List.length (ms_args ms)) s)
			[MayInit c;InvokeStatic (None,c,ms,params)]
		  | Some t ->
		      let x = make_tempvar dico ssa fresh_counter s next_store in
		      clean dico ssa fresh_counter is_heap_sensible_element_in_expr
			(E (Var (t,x))::(popn (List.length (ms_args ms)) s))
			[MayInit c; InvokeStatic (Some x,c,ms,param (List.length (ms_args ms)) s)])
	   | x ->
	       begin
		 let popn_s = popn (List.length (ms_args ms)) s in
		   (match top popn_s  with
		      | Uninit (c,j) ->
			  let x = make_tempvar dico ssa fresh_counter s next_store in
			  let e' = E (Var (TObject (TClass java_lang_object),x)) in
			    clean dico ssa fresh_counter is_heap_sensible_element_in_expr
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
				   clean dico ssa fresh_counter is_heap_sensible_element_in_expr s_next ([Check (CheckNullPointer e0)]@(ins None))
			       | Some t ->
				   let y = make_tempvar dico ssa fresh_counter s next_store in
				     clean dico ssa fresh_counter is_heap_sensible_element_in_expr (E (Var (t,y))::s_next) ([Check (CheckNullPointer e0)]@(ins (Some y)))
			    ))
	       end)
	end
  | OpNew c -> (Uninit (c,i))::s, [MayInit c]
  | OpNewArray t ->
      let x = make_tempvar dico ssa fresh_counter s next_store in
      let dim = topE s in
	E (Var (TObject (TArray t),x))::(pop s), [Check (CheckNegativeArraySize dim); NewArray (x,t,[dim])]
  | OpArrayLength ->
      let a = topE s in begin
	  match mode with
	    | Addr3 ->
		let x = make_tempvar dico ssa fresh_counter s next_store in
		  E (Var (TBasic `Int,x))::(pop s), [Check (CheckNullPointer a);AffectVar(x,Unop (ArrayLength,a))]
	    | _ -> E (Unop (ArrayLength,a))::(pop s),[Check (CheckNullPointer a)]
	end
  | OpThrow ->
      let r = topE s in [], [Check (CheckNullPointer r); Throw r]
  | OpCheckCast t -> to_addr3_unop dico mode (Cast t) ssa fresh_counter s [Check (CheckCast (topE s,t))]
  | OpInstanceOf c -> to_addr3_unop dico mode (InstanceOf c) ssa fresh_counter s []
  | OpMonitorEnter ->
      let r = topE s in
	pop s, [Check (CheckNullPointer r); MonitorEnter r]
  | OpMonitorExit ->
      let r = topE s in
	pop s, [Check (CheckNullPointer r); MonitorExit r]
  | OpAMultiNewArray (cn,dim) ->
      let x = make_tempvar dico ssa fresh_counter s next_store in
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
    | (_,BranchVar (i,_)) -> List.mem i succs
    | _ -> false in
  is_in_stack test (fun _ _ -> false)


let para_assign dico pc succs stack =
  if is_branchvar_in_stack succs stack then
    let rec aux i = function
      [] -> [], []
    | e::q -> begin
	match e with
	  | Uninit _ -> aux (i+1) q
	  | E e ->
	      let (l1,l2) = aux (i+1) q in
	      let x = make_var dico (BranchVar2 (pc,i)) in
	      let l = List.map (fun j -> AffectVar (make_var dico (BranchVar (j,i)),Var (type_of_expr e,x))) succs in
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
		let l = List.map (fun j -> AffectVar (make_var dico (BranchVar (j,i)),e)) succs in
		  l @ (aux (i+1) q)
	end
    in aux 0 stack

let jump_stack dico pc' stack =
  let rec aux i = function
      [] -> []
    | e::q -> begin
	match e with
	  | Uninit _ ->
	      e :: (aux (i+1) q)
	  | E e ->
	      E (Var (type_of_expr e,make_var dico (BranchVar (pc',i)))) :: (aux (i+1) q)
      end
  in aux 0 stack




exception NonemptyStack_backward_jump
exception Type_constraint_on_Uninit
exception Content_constraint_on_Uninit


let value_compare e1 e2 =
  match e1, e2 with
    | Var(_,x), Var(_,y) -> x = y
    | _ -> e1 = e2

let value_compare e1 e2 =
  match e1, e2 with
    | Uninit _, Uninit _ -> e1 = e2
    | E e1, E e2 -> value_compare e1 e2
    | _ -> false

let value_compare_stack s1 s2 =
  List.for_all2 value_compare s1 s2

let bc2ir dico flat ssa pp_var jump_target load_type arrayload_type code =
  let rec loop as_ts_jump ins ts_in as_in pc fresh_counter =

    (* Simplifying redundant assignt on the fly : see one instr ahead *)
    let next_store =
      let next_pc = try next code.c_code pc with End_of_method -> pc in
	match code.c_code.(next_pc) with
	  | OpStore (_,n) -> 
	      if jump_target.(next_pc) 
	      then None
	      else Some (make_var dico (OriginalVar (n,pp_var next_pc n)))
	  | _ -> None
    in
    let succs = normal_next code pc in
    let (ts_in,as_in) =
      if jump_target.(pc) then
	try MapPc.find pc as_ts_jump
	with Not_found ->
	  (* no predecessor of pc have been visited before *)
	  if List.exists (fun e -> pc = e.JCode.e_handler) code.c_exc_tbl then
	    (* this is a handler point *)
	    ([Op32],[E (Var (TObject (TClass java_lang_object),make_var dico (CatchVar pc)))])
	  else
	    (* this is a back jump target *)
	    ([],[])
      else (ts_in,as_in)
    in
    let ts_out = type_next code.c_code.(pc) ts_in in
    let (as_out,instrs) = bc2bir_instr dico flat pp_var ssa fresh_counter pc load_type arrayload_type ts_in as_in next_store code.c_code.(pc)  in

      (* fail on backward branchings on a non-empty stack *)
      if List.length as_out>0 then
	if (List.exists (fun j -> j<pc) succs) then raise NonemptyStack_backward_jump;

    let jump_succs = List.filter (fun i -> jump_target.(i)) succs in
    let branch_assigns = para_assign dico pc jump_succs as_out in
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
	       let jmp_s =  jump_stack dico pc' as_out in
		 if (not (value_compare_stack as_jmp jmp_s)) then
		   ( Printf.printf "\n %s\n" (string_of_int pc') ;
		     Printf.printf "%s \n" (print_stackmap as_jmp) ;
		     Printf.printf "%s \n" (print_stackmap jmp_s) ;
		     raise Content_constraint_on_Uninit )
		 else as_jump
	   with Not_found ->
	     (* when first advice for pc', no constraint to check. add the advice in the map *)
	     let st = jump_stack dico pc' as_out in
	       MapPc.add pc' (ts_out,st) as_jump)
	as_ts_jump
	jump_succs in
      try
	loop as_ts_jump ins ts_out as_out (next code.c_code pc) fresh_counter
      with End_of_method ->  ins
  in
    loop MapPc.empty [] [] [] 0 (ref 0)

let varname = Cmn.varname

let search_name_localvar static code i x =
  if x=0 && (not static) then Some "this"
  else match JCode.get_local_variable_info x i code with
    | None ->  None
    | Some (s,_) -> Some s

let bcvar i = OriginalVar (i,None)

let compute_jump_target code =
  let jump_target = Array.make (Array.length code.c_code) false in
    List.iter (fun e -> jump_target.(e.JCode.e_handler) <- true) code.c_exc_tbl;
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

let gen_params dico pp_var cm =
  if cm.cm_static then
    mapi
      (fun i t -> t, make_var dico (OriginalVar (i,pp_var 0 i)))
      (fun i t -> match convert_field_type t with Op32 -> i+1 | Op64 -> i+2)
      (ms_args cm.cm_signature)
  else
    (TObject (TClass java_lang_object ), make_var dico (OriginalVar (0,pp_var 0 0)))::
      (mapi
	 (fun i t -> t, make_var dico (OriginalVar (i+1,pp_var 0 (i+1))))
	 (fun i t -> match convert_field_type t with Op32 -> i+1 | Op64 -> i+2)
	 (ms_args cm.cm_signature))

let compress_ir handlers ir jump_target =
  let h_ends = List.fold_right (fun e s -> Ptset.add e.JCode.e_end (Ptset.add e.JCode.e_start s)) handlers Ptset.empty in
  let rec aux0 pc0 = function
    | [] -> [pc0,[Nop]]
    | (pc,_)::_ as l when jump_target.(pc) || Ptset.mem pc h_ends -> (pc0,[Nop])::(aux l)
    | (_,[])::q -> aux0 pc0 q
    | (_,instrs)::q -> (pc0,instrs)::(aux q)
  and aux = function
    | [] -> []
    | (pc,[])::q -> aux0 pc q
    | (pc,instrs)::q -> (pc,instrs)::(aux q)
  in aux ir

let make_exception_handler dico e =
  {
    e_start = e.JCode.e_start;
    e_end = e.JCode.e_end;
    e_handler = e.JCode.e_handler;
    e_catch_type = e.JCode.e_catch_type;
    e_catch_var = make_var dico (CatchVar e.JCode.e_handler);
  }

(* Code flattening *)
let rec last = function
    [] -> assert false
  | [(x,_)] -> x
  | _::q -> last q

let flatten_code code exc_tbl =
  (* starting from i, and given the code [code], computes a triple *)
  let rec aux i map = function
      [] -> [], [], map
    | (pc,instrs)::q ->
	let (instrs',pc_list,map) = aux (i+List.length instrs) map q in
	  instrs@instrs', 
            (* flatten_code the code but does not modify the instruction in it *)
	  (List.map (fun _ -> pc) instrs)@pc_list,
	    (* the list of initial pcs of instrs *)
	  Ptmap.add pc i map
	    (* map from initial pcs to the future pc they are mapped to *)
  in 
  let (instrs,pc_list,map) = aux 0 Ptmap.empty code in
  let rec find i = (* find the flattened pc associated to i *)
    try Ptmap.find i map
    with Not_found -> assert (i=1+(last code)); List.length instrs
  in
  let instrs =
    Array.of_list
      (List.map (function 
		   | Goto pc -> Goto (Ptmap.find pc map)
		   | Ifd (g, pc) -> Ifd (g, Ptmap.find pc map)
		   | ins -> ins) instrs) (* Change to new pcs *)
  and exc_tbl =
    List.map
      (fun e -> 
         { e_start = find e.e_start;
	   e_end = find e.e_end; (* It may be outside the range ?  *)
	   e_handler = find e.e_handler;
	   e_catch_type = e.e_catch_type;
	   e_catch_var = e.e_catch_var
         }) exc_tbl
  in
    (instrs, Array.of_list pc_list, map, exc_tbl)


let jcode2bir mode bcv ssa cm jcode =
  let code = jcode in
    match JsrInline.inline code with
      | Some code ->
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
	  let (load_type,arrayload_type) = BCV.run bcv cm code in
	  let dico = make_dictionary () in
	  let res = bc2ir dico mode ssa pp_var jump_target load_type arrayload_type code in
	  let ir_code = compress_ir code.c_exc_tbl (List.rev res) jump_target in
	  let ir_exc_tbl = List.map (make_exception_handler dico) code.c_exc_tbl in
	  let (ir_code,ir2bc,bc2ir,ir_exc_tbl) =  flatten_code ir_code ir_exc_tbl in
	    { params = gen_params dico pp_var cm;
	      vars = make_array_var dico;
	      code = ir_code;
	      pc_ir2bc = ir2bc;
	      pc_bc2ir = bc2ir;
	      exc_tbl = ir_exc_tbl;
	      line_number_table = code.c_line_number_table;
	      jump_target = jump_target }
      | None -> raise Subroutine

let transform ?(bcv=false) ?(ir_ssa=false) = jcode2bir Normal bcv ir_ssa
let transform_flat ?(bcv=false) ?(ir_ssa=false) = jcode2bir Flat bcv ir_ssa
let transform_addr3 ?(bcv=false) ?(ir_ssa=false) = jcode2bir Addr3 bcv ir_ssa


(* Agregation of boolean tests  *)
module AgregatBool = struct
  type cond = [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr 

  type decision_tree =
    | LeafTrue
    | LeafFalse
    | Node of 
	cond (* b *)
	* decision_tree (* b true *)
	* decision_tree (* b false *)
	  
  let dec = "  "

  let print_tree t =
    let rec aux b = function
      | LeafTrue -> Printf.sprintf "%strue\n" b
      | LeafFalse -> Printf.sprintf "%sfalse\n" b
      | Node (x,t1,t2) -> 
	  Printf.sprintf "%sNode %s\n%s%s" b
	    (print_cmp x)
	    (aux (b^dec) t1)
	    (aux (b^dec) t2) in
      aux "" t

  let neg_cond (c,e1,e2) =
    match c with
      | `Eq -> (`Ne,e1,e2)
      | `Ne -> (`Eq,e1,e2)
      | `Lt -> (`Ge,e1,e2)
      | `Ge -> (`Lt,e1,e2)
      | `Gt -> (`Le,e1,e2)
      | `Le -> (`Gt,e1,e2)

  exception Not_a_decision_tree

  let rec find_block i = function
      [] -> raise Not_a_decision_tree
    | (j,block)::rest -> if i=j then (j,block)::rest else find_block i rest

  let is_branch_var = function
    | (_,BranchVar _) -> true
    | _ -> false

  let rec compute_decision_tree_aux = function
    | (i,(Ifd (c,j))::block)::rest ->
	let (target_left,left) = compute_decision_tree_aux ((i,block)::rest) in
	let (target_right,right) = compute_decision_tree_aux (find_block j rest) in
	  if target_left=target_right 
	  then (target_right,Node (neg_cond c,left,right))
	  else raise Not_a_decision_tree
    | (_,[Goto j])::rest -> compute_decision_tree_aux (find_block j rest)
    | (_,[AffectVar (x,Const (`Int i1));Goto l])::_
	when is_branch_var x ->
	if i1 = Int32.one then ((x,l),LeafTrue)
	else if i1 = Int32.zero then ((x,l),LeafFalse)
	else raise Not_a_decision_tree
    | _ -> raise Not_a_decision_tree

  let rec compute_decision_tree acc = function
    | (_,(Ifd (_,_))::_)::_ as l ->
	let (target,left) = compute_decision_tree_aux l in
	  (List.rev acc,target,left)
    | (i,ins::block)::rest -> compute_decision_tree (ins::acc) ((i,block)::rest)
    | _ -> raise Not_a_decision_tree
	
  let compute_decision_tree = compute_decision_tree []
				
(*
  (* argument must be of the form
     Or (c1,(Or(c2,....
     with each c of the form And (C ..,And (C ..., .... *)
  let rec cons_and c0 = function
    | Or (d,g) -> Or (And (C c0,d),cons_and c0 g)
    | x -> And (C c0,x)

  (* argument must be of the form
     And (d1,(And(d2,....
     with each d of the form Or (C ..,Or (C ..., .... *)
  let rec app_or l1 l2 =
    match l1 with
      | Or (d,g) -> Or (d,app_or g l2)
      | x -> Or (x,l2)    
	  
  let guard_of_decision_tree = function
    | Node (c,left,right) -> begin
	let rec aux c0 = function
	  | LeafFalse -> None
	  | LeafTrue -> Some (C c0)
	  | Node (c,left,right) -> begin
	      match aux (neg_cond c) right, aux c left with
		| None, None -> None
		| Some g_right, None -> Some (cons_and c0 g_right)
		| None, Some g_left -> Some (cons_and c0 g_left)
		| Some g_right, Some g_left -> Some (app_or (cons_and c0 g_right) g_left)
	    end in
	  match aux (neg_cond c) right, aux c left with
	    | None, None -> raise Not_a_decision_tree
	    | Some g_right, None -> g_right
	    | None, Some g_left -> g_left
	    | Some g_right, Some g_left -> app_or g_right g_left
      end
    | _ -> raise Not_a_decision_tree
	
  let rec cons_and c0 = function
    | [] -> []
    | d::g -> (c0::d)::(cons_and c0 g)
	
  let guard_of_decision_tree t = 
    let rec aux = function
      | LeafFalse -> []
      | LeafTrue -> [[]]
      | Node (c,left,right) -> 
	  let l = aux left in
	  let r = aux right in
	    if l = [] then cons_and (neg_cond c) r
	    else if r = [] then cons_and c l
	    else (cons_and c l)@(cons_and (neg_cond c) r) in
    let rec to_conj = function
      | [] -> raise Not_a_decision_tree
      | [c] -> C c
      | c::q -> And (C c,to_conj q) in
    let rec to_dij = function
      | [] -> raise Not_a_decision_tree
      | [conj] -> to_conj conj
      | conj::q -> Or (to_conj conj, to_dij q) in
    let l = List.filter (fun d -> d<>[]) (aux t) in
      (*    List.iter
	    (fun conj ->
	    Printf.printf "[ ";
	    List.iter (fun c -> Printf.printf "%s : " (print_cmp pp_var c)) conj;
	 Printf.printf " ]\n"
	    ) l; *)
      to_dij l

  let rec simplify_bool = function
      [] -> []
    | (i,block,handlers)::blocks as l ->
	try begin
	  let (begin_block,(x,label),tree) = compute_decision_tree l in
	    (*	  Printf.printf "Found a decision tree from %d to %d in %s\n%s\n"
		  i label m.cm_signature.ms_name
		  (print_tree pp_var tree); *)
	    match find_block label blocks with
	      | [] -> raise Not_a_decision_tree
	      | (_,block,handlers)::blocks ->
		  let g = guard_of_decision_tree tree in
		    simplify_bool 
		      (replace_var_in_blocks x (Guard g) ((i,begin_block@block,handlers)::blocks))
	end with Not_a_decision_tree -> (i,block,handlers)::(simplify_bool blocks)
	  *)

end

