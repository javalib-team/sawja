(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
 * Copyright (c)2016 David Pichardie (ENS Rennes)
 * Copyright (c)2016 Laurent Guillo (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

open Javalib_pack
open Javalib
open JBasics
open JCode 

include Cmn

type expr =
  | Const of const
  | Var of JBasics.value_type * var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * JBasics.class_name * field_signature
  | StaticField of JBasics.class_name * field_signature

type formula =
  | Atom of [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr
  | BoolVar of expr
  | And of formula * formula
  | Or of formula * formula

type opexpr =
  | Uninit of JBasics.class_name * int
  | E of expr

type check =
  | CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr * object_type
  | CheckArithmetic of expr
  | CheckLink of jopcode

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
  | Formula of class_method_signature * formula
      

let type_of_const i = 
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


let type_of_expr = function
  | Var (t,_) -> t
  | Field (_,_,f)
  | StaticField (_,f) -> fs_type f
  | Const i -> type_of_const i
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

let instr_jump_to = function
  | Ifd (_, n)
  | Goto n -> Some n
  | _ -> None


(************* PRINT ************)

let rec print_expr' ?(show_type=true) first_level = function
  | Var (t,x) -> 
      if show_type then Printf.sprintf "%s:%s" (var_name_g x) (print_typ t)
      else (var_name_g x)
  | Field (e,c,f) -> Printf.sprintf "%s.%s" (print_expr' ~show_type:show_type false e) (JUtil.print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Const i -> print_const i
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_expr' ~show_type:show_type false e)
  | Unop (Cast t,e) -> Printf.sprintf "(%s) %s" (print_typ (TObject t)) (print_expr' ~show_type:show_type true e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_expr' ~show_type:show_type true e)
  | Binop (ArrayLoad t,e1,e2) -> 
      if show_type then Printf.sprintf "%s[%s]:%s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type true e2) (print_typ t)
      else Printf.sprintf "%s[%s]" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type true e2)
  | Binop (Add _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s+%s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2))
  | Binop (Sub _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s-%s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2))
  | Binop (Mult _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s*%s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2))
  | Binop (Div _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s/%s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2))
  | Binop (op,e1,e2) -> Printf.sprintf "%s(%s,%s)" (print_binop op) (print_expr' ~show_type:show_type true e1) (print_expr' ~show_type:show_type true e2)

let print_cmp ?(show_type=true) (c,e1,e2) =
  match c with
    | `Eq -> Printf.sprintf "%s == %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2)
    | `Ne -> Printf.sprintf "%s != %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2)
    | `Lt -> Printf.sprintf "%s < %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2)
    | `Ge -> Printf.sprintf "%s >= %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2)
    | `Gt -> Printf.sprintf "%s > %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2)
    | `Le -> Printf.sprintf "%s <= %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type false e2)

let rec print_formula ?(show_type=true) = function
  | Atom (cmp,e1,e2) -> print_cmp ~show_type:show_type (cmp,e1,e2)
  | BoolVar v -> print_expr' ~show_type: show_type false v
  | And (f1,f2) -> Printf.sprintf "(%s) && (%s)" 
      (print_formula ~show_type:show_type f1) (print_formula ~show_type:show_type f2)
  | Or (f1,f2) -> Printf.sprintf "(%s) || (%s)" 
      (print_formula ~show_type:show_type f1) (print_formula ~show_type:show_type f2)

let print_oexpr ?(show_type=true) = function
  | Uninit (c,i) -> Printf.sprintf "Unit(%d,%s)" i (JPrint.class_name c)
  | E e -> print_expr' ~show_type:show_type true e

let print_stackmap ?(show_type=true) = function
  | [] -> ""
  | x::q -> List.fold_left (fun s t -> Printf.sprintf "%s :: %s" (print_oexpr ~show_type:show_type t) s) (print_oexpr ~show_type:show_type x) q

let print_instr ?(show_type=true) = function
  | Nop -> "nop"
  | AffectVar (x,e) -> Printf.sprintf "%s := %s" (var_name_g x) (print_expr' ~show_type:show_type true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_expr' ~show_type:show_type true e)
  | AffectField (e1,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_expr' ~show_type:show_type false e1) (JUtil.print_field c f) (print_expr' ~show_type:show_type true e2)
  | AffectArray (e1,e2,e3) -> Printf.sprintf "%s[%s] := %s" (print_expr' ~show_type:show_type false e1) (print_expr' ~show_type:show_type true e2) (print_expr' ~show_type:show_type true e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp g) el
  | Throw e -> Printf.sprintf "throw %s" (print_expr' ~show_type:show_type false e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_expr' ~show_type:show_type false e)
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (JUtil.print_list_sep "," (print_expr' ~show_type:show_type true) le)
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (JUtil.print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_expr' ~show_type:show_type true e)) le)
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s)" (JPrint.class_name c) (ms_name ms) (JUtil.print_list_sep "," (print_expr' ~show_type:show_type true) le)
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s)" (var_name_g x) (JPrint.class_name c) (ms_name ms) (JUtil.print_list_sep "," (print_expr' ~show_type:show_type true) le)
  | InvokeVirtual (r,e1,_,ms,le) ->
      Printf.sprintf "%s%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr' ~show_type:show_type false e1) (ms_name ms) (JUtil.print_list_sep "," (print_expr' ~show_type:show_type true) le)
  | InvokeNonVirtual (r,e1,kd,ms,le) ->
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_expr' ~show_type:show_type false e1) (JPrint.class_name kd) (ms_name ms) (JUtil.print_list_sep "," (print_expr' ~show_type:show_type true) le)
  | MonitorEnter e -> Printf.sprintf "monitorenter(%s)" (print_expr' ~show_type:show_type true e)
  | MonitorExit e -> Printf.sprintf "monitorexit(%s)" (print_expr' ~show_type:show_type true e)
  | MayInit c -> Printf.sprintf "mayinit %s" (JPrint.class_name c)
  | Check c ->
      begin
	match c with
	    CheckNullPointer e -> Printf.sprintf "notnull %s" (print_expr' ~show_type:show_type true e)
	  | CheckArrayBound (a,i) -> Printf.sprintf "checkbound %s[%s]"  (print_expr' ~show_type:show_type true a) (print_expr' ~show_type:show_type true i)
	  | CheckArrayStore (a,v) -> Printf.sprintf "checkstore %s[] <- %s"  (print_expr' ~show_type:show_type true a) (print_expr' ~show_type:show_type true v)
	  | CheckNegativeArraySize e -> Printf.sprintf "checknegsize %s" (print_expr' ~show_type:show_type true e)
	  | CheckCast (e,t) -> Printf.sprintf "checkcast %s:%s" (print_expr' ~show_type:show_type true e) (JDumpBasics.object_value_signature t)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_expr' ~show_type:show_type true e)
	  | CheckLink op -> Printf.sprintf "checklink (%s)" (JPrint.jopcode op)
      end
  | Formula (cmd,f) -> 
      let (cn, ms) = cms_split cmd in 
        Printf.sprintf "FORMULA: %s.%s(%s)" (cn_name cn) (ms_name ms)
          (print_formula ~show_type:show_type f)

let print_expr ?(show_type=true) = print_expr' ~show_type:show_type true

	
type bir = {
  bir_vars : var array;  (** All variables that appear in the method. [vars.(i)] is the variable of index [i]. Caution, when we are in bir SSA, it contains both the bir variables (with lower index) and birSSA variables. *)
  bir_params : (JBasics.value_type * var) list;
  bir_code : instr array;
  bir_exc_tbl : exception_handler list;
  bir_line_number_table : (int * int) list option;
  (*  bir_pc_bc2ir : int Ptmap.t;*)
  bir_pc_ir2bc : int array;

  (* only for ssa *)
  bir_preds : (int array) array;
  bir_phi_nodes : (phi_node list) array;
  bir_mem_ssa : memory_ssa_info
}

let empty = 
  {
    bir_vars = [||];
    bir_params = [];
    bir_code = [||];
    bir_exc_tbl = [];
    bir_line_number_table = None;
    (*bir_pc_bc2ir = Ptmap.empty;*)
    bir_pc_ir2bc = [||];
    bir_preds = [||];
    bir_phi_nodes = [||];
    bir_mem_ssa = 
      {
        mem_ssa_in= (fun a -> a);
        mem_ssa_out= (fun a -> a);
        mem_ssa_phi= (fun a -> a, [|a|])
      }
  }


let rec ssa_print_code phi_simpl m i acc =
  if i<0 then acc
  else 
    begin
      let new_acc = 
        (app_phi_nodes phi_simpl i m.bir_preds.(i) m.bir_phi_nodes.(i)
           (Printf.sprintf "%3d: %s" 
              i 
              (print_instr m.bir_code.(i))::acc))
      in
        ssa_print_code phi_simpl m (i-1) new_acc
    end


let ssa_print ?(phi_simpl=true) m =
  let size = Array.length m.bir_code in
    ssa_print_code phi_simpl m (size-1) []

let rec print_code code i acc =
  if i<0 then acc
  else print_code code (i-1) (Printf.sprintf "%3d: %s" i (print_instr code.(i))::acc)

let bir_print m =
  let size = Array.length (m.bir_code) in
    print_code m.bir_code (size-1) []	

let bir_jump_target m =
  let jump_target = Array.make (Array.length m.bir_code) false in
    List.iter (fun e -> jump_target.(e.e_handler) <- true) m.bir_exc_tbl;
    Array.iter
      (fun instr ->
	 match instr_jump_to instr with
	     Some n -> jump_target.(n) <- true;
	   | None -> ())
      m.bir_code;
    jump_target	  

let bir_get_source_line_number pc_ir m =
  match m.bir_line_number_table with
    | None -> None
    | Some lnt ->
        JCode.get_source_line_number' m.bir_pc_ir2bc.(pc_ir) lnt

let bir_exception_edges m = 
    generic_exception_edges m.bir_code m.bir_exc_tbl 

		   

(*********** TYPES *************)

(* For stack type inference only *)
type op_size = Op32 | Op64



(******* STACK MANIPULATION **************)

exception Bad_stack
  (** [Bad_stack] is raised in case the stack does not fit the length/content
      constraint of the bytecode instruction being transformed. *)

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
  (* [Uninit_is_not_expr] is raised in case an uninitialised reference is used
      as a traditional expression (variable assignment, field reading etc).*)
  (* David: can't happen if the program is bytecode verified *)

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


(* For an opcode and the previous type inference stack, return the updated
* stack.*)
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

(*Computes successors of instruction i. They can be several successors in case
* of conditionnal instruction.*)
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

(*[mapi f g lst] maps [lst] to a new list using [f] as transformation function.
* [f] takes an int and an element of the list. The integer has been previously
* computed using the g function on the previous element. This integer can be
* used to count a lenght depending of every element of the list. *)
let mapi f g =
  let rec aux i = function
      [] -> []
    | x::q -> (f i x)::(aux (g i x) q)
  in  aux 0
	
(*ByteCode Verifier*)
module BCV = struct

  type typ =
    | Top
    | Top32
    | Top64
    | Int
    | Float
    | Double
    | Long
    | Object
    | Array of JBasics.value_type
    | Null

(* The first element is the stack, the second one is the local var map. *)
  type t = typ list * typ Ptmap.t

  let conv_array_type t = 
    match t with
      | `Int  | `Short   | `Char
      | `Int2Bool | `ByteBool -> Int
      | `Float -> Float
      | `Object -> Object
      | `Long -> Long
      | `Double -> Double

  let type_of_array_type t = 
    match t with
      | `Int  | `Short   | `Char
      | `Int2Bool | `ByteBool -> TBasic `Int
      | `Float -> TBasic `Float
      | `Object -> TObject (TClass java_lang_object)
      | `Long -> TBasic `Long
      | `Double -> TBasic `Double



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
    | Null -> "Null"
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
      (JUtil.print_list_sep ";" print_typ s)
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
    | Null
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
  let array_content i t = function
    | Array v -> conv v
    | Null -> conv_array_type t
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
    | Null
    | Array _
    | Top32 -> Op32
    | Double
    | Long
    | Top64 -> Op64

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
	x=Null ||
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
			| `ANull -> Null
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
    | OpArrayLoad t -> (fun (s,l) -> array_content i t (top (pop s))::(pop2 s),l)
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
      (fun i -> (*return true if pp [i] has type info.*)
         match types.(i) with
           | Some _ -> true
           | None -> false
      ),
      (fun i ->
	 match code.c_code.(i) with
	   | OpLoad (_,n) ->
	       (match types.(i) with
		  | Some (_, l) -> to_value_type (get l n)
		  | _ -> assert false)
	   | _ -> assert false),
      (fun _ i -> 
	 match code.c_code.(i) with
	   | OpArrayLoad t ->
	       (match types.(i) with
		  | Some (_::(Array t)::_, _) -> t
                  | Some (_::Null ::_, _) -> type_of_array_type t 
		  | _ -> assert false)
	   | _ -> assert false)


  let array_type_to_value_type = function
    | `Long -> TBasic `Long
    | `Float -> TBasic `Float
    | `Double -> TBasic `Double
    | `Int2Bool -> TBasic `Int
    | `Object -> TObject (TClass java_lang_object)

  let run_dummy code =
    (fun _ -> true),
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
		   | `ByteBool
		   | `Short
		   | `Int -> TBasic `Int
		   | `Char ->  TBasic `Char
		   | `Int2Bool -> TBasic `Int
		   | `Object -> TObject (TClass java_lang_object)))
    
  let run bcv ?(verbose=false) cm code =
    if bcv then run verbose cm code
    else (run_dummy code)
end



(**************** GENERATION *************)


exception Bad_Multiarray_dimension
  (** [Bad_Multiarray_dimension] is raise when attempting to transforming a
      multi array of dimension zero. *)

(* fetches the element value type *)
let remove_dim t n =
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


(*Add in acc the Tempvar found in expr.*)
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

(* Return the set of Tempvar found in the stack. *)
let temp_in_stack s =
  List.fold_left temp_in_opexpr Ptset.empty s

(*Return an integer indicating the next available Tempvar identifier.*)
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

type mode = Normal | Addr3 


let to_addr3_binop dico mode binop ssa fresh_counter s =
  match mode with
    | Addr3 -> 	let x = make_var dico (TempVar (fresh_in_stack ssa fresh_counter s))
      in 
	(match binop with 
	   | Div jvm_t 
	   | Rem jvm_t when jvm_t = `Int2Bool || jvm_t = `Long ->
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
    | _ -> 
	(match binop with 
	   | Div jvm_t 
	   | Rem jvm_t when jvm_t = `Int2Bool || jvm_t = `Long ->  
	       begin
		 let q = topE s in 
		   E (Binop (binop,topE (pop s), topE s))::(pop2 s), [Check (CheckArithmetic q)]
	       end
	   | _ ->
	       begin
		 E (Binop (binop,topE (pop s), topE s))::(pop2 s), []
	       end)
	  
let to_addr3_const dico mode ssa fresh_counter c s instrs next_store next_is_junc_point_or_a_goto =
  match mode with
    | Addr3 -> 
	begin
	  match next_store with
	    | None when not next_is_junc_point_or_a_goto -> 
		let x = make_var dico (TempVar (fresh_in_stack ssa fresh_counter s)) in
		  begin
		    let e = Const c in
		      E (Var (type_of_expr e,x))::s, instrs@[AffectVar(x,e)]
		  end
	    | _ -> E (Const c)::s, instrs
	end
    | _ -> E (Const c)::s, instrs

let to_addr3_ifd dico mode ssa fresh_counter s guard target =
  match guard with
    | (cmp,e1,Const c) -> 
	begin
	  match mode with
	    | Addr3 -> let x = make_var dico (TempVar (fresh_in_stack ssa fresh_counter s)) in
		begin
		  let e = Const c in
		  let v = Var (type_of_expr e,x) in
		    [AffectVar(x,e); Ifd ((cmp,e1,v),target)]
		end
	    | _ ->  [Ifd (guard,target)]
	end
    | _ -> [Ifd (guard,target)]


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

(* 
 * Maps each opcode to a (expr list * instr list). An [expr] is a value
 * (constant or variable) or a basic operation that return a value without side
 * effect (an addition for exemple). [instr] is an operation which can take
 * [expr] and which change the state of the system (a variable affectation for
 * exemple).
 * 
 * [dico] : A dictionary to each variable of the program. we can directly add
 * new variables in it (mutable structure).
 * [mode] : precises if we are in 3 address or normal mod.
 * [pp_var i x] : A function which return the name if available (string
 * option) of the var at index [x] for the program point [i].
 * [ch_link] : true if linkage operation is done.
 * [ssa] : gives a SSA representation.
 * [fresh_counter] : Only used with SSA representation. It is a counter that is
 * incremented each time we add a TempVar. It allows to use unique variable
 * name for each affectation.
 * [i] : Current program point.
 * [load_type i] : Returns the type of the variable referenced by the OpLoad
 * instruction at position i.
 * [arrayload_type jat i] : Returns the type of the variable referenced by an
 * OpArrayLoad instruction at index pp in the code. [jat] is a jvm_array_type, it
 * is used only when type checking is not done.
 * [tos] : Type inference stack.
 * [s] : Abstract stack of expression.
 * [next_store] : If next variable is an OpStore, represents the var at this
 * instruction index. Else equals None.
 * [next_is_junc_point_or_a_goto] : True if next instruction is a goto or a jump.
 * 
 * The function takes a Jcode representing a list of opcode as implicit argument.
 *)
let bc2bir_instr dico mode pp_var ch_link ssa fresh_counter i load_type
      arrayload_type tos s next_store next_is_junc_point_or_a_goto = function
  | OpNop -> s, []
  | OpConst c -> to_addr3_const dico mode ssa fresh_counter (jconst2const c) s
                   [] next_store next_is_junc_point_or_a_goto
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
		  begin
		    match mode with
		      | Addr3 ->
			  let y = make_tempvar dico ssa fresh_counter (E (Var (TBasic `Int,x))::s) None in
			    replace_var_in_stack a x s,
			    [AffectVar(x,Var (t,a));
			     AffectVar(y,Const (`Int (Int32.of_int b)));
			     AffectVar (a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Var (TBasic `Int,y)))]
		      | _ -> 
			    replace_var_in_stack a x s,
			    [AffectVar(x,Var (t,a));
			     AffectVar (a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Const (`Int (Int32.of_int b))))]
		  end
	    | _ -> 
		begin
		  match mode with
		    | Addr3 ->
			let y = make_tempvar dico ssa fresh_counter s None in
			    s,
			    [AffectVar(y,Const (`Int (Int32.of_int b)));
			     AffectVar (a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Var (TBasic `Int,y)))]
		      | _ -> 
			  s,
			  [AffectVar(a,Binop(Add `Int2Bool,Var (TBasic `Int,a),Const (`Int (Int32.of_int b))))]
		end
	end
  | OpPutField (c, f) as instr ->
      let r = topE (pop s) in
      let instrs = 
	if ch_link
	then
	  [Check (CheckLink instr); Check (CheckNullPointer r); AffectField (r,c,f,topE s)] 
	else
	  [Check (CheckNullPointer r); AffectField (r,c,f,topE s)] 
      in
	clean dico ssa fresh_counter (is_field_in_expr c f) (pop2 s) instrs
  | OpArrayStore jvm_t ->
      let v = topE s in
      let a = topE (pop2 s) in
      let idx = topE (pop s) in
      let inss = 
	if jvm_t = `Object
	then
	  [Check (CheckNullPointer a);
	   Check (CheckArrayBound (a,idx));
	   Check (CheckArrayStore (a,v));
	   AffectArray (a, idx, v)]
	else
	  [Check (CheckNullPointer a);
	   Check (CheckArrayBound (a,idx));
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
  | OpLUShr -> to_addr3_binop dico mode LUshr ssa fresh_counter s 
  | OpI2L -> to_addr3_unop dico mode (Conv I2L) ssa fresh_counter s []  
  | OpI2F -> to_addr3_unop dico mode (Conv I2F) ssa fresh_counter s []  
  | OpI2D -> to_addr3_unop dico mode (Conv I2D) ssa fresh_counter s []  
  | OpL2I -> to_addr3_unop dico mode (Conv L2I) ssa fresh_counter s []  
  | OpL2F -> to_addr3_unop dico mode (Conv L2F) ssa fresh_counter s []  
  | OpL2D -> to_addr3_unop dico mode (Conv L2D) ssa fresh_counter s []  
  | OpF2I -> to_addr3_unop dico mode (Conv F2I) ssa fresh_counter s []  
  | OpF2L -> to_addr3_unop dico mode (Conv F2L) ssa fresh_counter s []  
  | OpF2D -> to_addr3_unop dico mode (Conv F2D) ssa fresh_counter s []  
  | OpD2I -> to_addr3_unop dico mode (Conv D2I) ssa fresh_counter s []  
  | OpD2L -> to_addr3_unop dico mode (Conv D2L) ssa fresh_counter s []  
  | OpD2F -> to_addr3_unop dico mode (Conv D2F) ssa fresh_counter s []  
  | OpI2B -> to_addr3_unop dico mode (Conv I2B) ssa fresh_counter s []  
  | OpI2C -> to_addr3_unop dico mode (Conv I2C) ssa fresh_counter s []  
  | OpI2S -> to_addr3_unop dico mode (Conv I2S) ssa fresh_counter s []  
  | OpCmp op -> 
      (match op with 
	 | `DG -> to_addr3_binop dico mode (CMP DG) ssa fresh_counter s 
	 | `DL -> to_addr3_binop dico mode (CMP DL) ssa fresh_counter s 
	 | `FG -> to_addr3_binop dico mode (CMP FG) ssa fresh_counter s 
	 | `FL -> to_addr3_binop dico mode (CMP FL) ssa fresh_counter s 
	 | `L  -> to_addr3_binop dico mode (CMP L) ssa fresh_counter s 
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
	pop s, to_addr3_ifd dico mode ssa fresh_counter s guard target
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
	 (fun (guard,i) l -> (to_addr3_ifd dico mode ssa fresh_counter s guard i)@l)
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
	 (fun (c,j) l -> ((to_addr3_ifd dico mode ssa fresh_counter s (`Eq,topE s,Const (`Int c)) (j+i))@l))
	 pairs
	 [Goto (def+i)])
  | OpReturn k -> [],
    (match k with
       | `Void -> [Return None]
       | _ -> [Return (Some (topE s))])
  | OpGetField (c, f) as instr ->
      let r = topE s in
      let (exp,instrs) = 
	match mode with
	  | Normal -> 
	      E (Field (r,c,f))::(pop s), [Check (CheckNullPointer r)]
	  | _ ->
	      let x = make_tempvar dico ssa fresh_counter s next_store in
		E (Var (fs_type f,x))::(pop s), [Check (CheckNullPointer r);AffectVar(x,Field (r,c,f))]
      in
	if ch_link then
	  exp,(Check (CheckLink instr))::instrs
	else
	  exp,instrs
  | OpGetStatic (c, f) as instr ->
      (match mode with 
	 | Addr3 -> 
	     let instrs = 
	       if ch_link then
		 [Check (CheckLink instr); MayInit c]
	       else
		 [MayInit c]
	     in
	     let (s,instrs) =
	       clean dico ssa fresh_counter 
		 is_heap_sensible_element_in_expr
		 s
		 instrs
	     in
	       (* Must be done after clean because temp variables could
		  have been generated*)
	     let x = make_tempvar dico ssa fresh_counter s next_store in
	       E (Var (fs_type f,x))::s, instrs@[AffectVar(x,StaticField (c,f))]
	 | _ -> 
	     let instrs = 
	       if ch_link then
		 [Check (CheckLink instr); MayInit c]
	       else
		 [MayInit c]
	     in
	     let (s,instrs) =
	       clean dico ssa fresh_counter 
		 is_heap_sensible_element_in_expr
		 s
		 instrs
	     in
	       E (StaticField (c, f))::s, instrs)
	
  | OpPutStatic (c, f) as instr ->
      let instrs = 
	if ch_link then
	  [Check (CheckLink instr); MayInit c]
	else
	  [MayInit c]
      in
      let (s,instrs) =
	clean dico ssa fresh_counter 
	  is_heap_sensible_element_in_expr
	  s
	  instrs
      in
	pop s, instrs@[AffectStaticField (c,f,topE s);]
	  
  | OpInvoke (x, ms) as instr ->
      begin
	(match x with
	   | `Static c ->
	       (match ms_rtype ms with
		  | None ->
		      let instrs = 
			if ch_link then
			  [Check (CheckLink instr); MayInit c]
			else
			  [MayInit c]
		      in
		      let s,instrs = 
			clean dico ssa fresh_counter 
			  is_heap_sensible_element_in_expr
			  s
			  instrs
		      in
			(* Must be done after clean because
			   params could have been transformed in a
			   temp variable*)
		      let params = param (List.length  (ms_args ms)) s in
			popn (List.length (ms_args ms)) s,instrs@[InvokeStatic (None,c,ms,params)]
		  | Some t ->
		      let instrs = 
			if ch_link then
			  [Check (CheckLink instr); MayInit c]
			else
			  [MayInit c]
		      in
		      let s,instrs = 
			clean dico ssa fresh_counter is_heap_sensible_element_in_expr
			  s
			  instrs
		      in
			(* Must be done after clean
			   because temp variables could have been generated*)
		      let x = make_tempvar dico ssa fresh_counter s next_store in
			(* Must be done after clean because
			   params could have been transformed in a
			   temp variable*)
		      let params = param (List.length (ms_args ms)) s in
			(E (Var (t,x))::(popn (List.length (ms_args ms)) s),
			 instrs@[InvokeStatic (Some x,c,ms,params)])
	       )
	   | x ->
	       begin
		 let popn_s = popn (List.length (ms_args ms)) s in
		   (match top popn_s  with
		      | Uninit (c,j) ->
			  let x = make_tempvar dico ssa fresh_counter s next_store in
			  let e' = E (Var (TObject (TClass java_lang_object),x)) in
                          let instrs =
                            let instrs = 
                              [New (x,c,ms_args ms,param (List.length (ms_args ms)) s)]
                            in 
                              if ch_link
                              then
                                (Check (CheckLink instr))::instrs
                              else instrs
                          in
			    (* Ok for fresh variable because
			       Uninit is always replaced by
			       Var(_,x)*)
			    clean dico ssa fresh_counter is_heap_sensible_element_in_expr
			      (List.map
				 (function e -> if e = Uninit (c,j) then e' else e)
				 (pop popn_s))
			      instrs  
		      | E e0  ->
			  let nb_args = List.length (ms_args ms) in
			  let s_next = pop popn_s in
			  let this = topE popn_s in
			  let ins target =
			    match x with
			      | `Virtual o -> [InvokeVirtual (target,this,VirtualCall o,ms,param nb_args s)]
			      | `Interface c -> [InvokeVirtual (target,this,InterfaceCall c,ms,param nb_args s)]
			      | `Special c -> [InvokeNonVirtual (target,this,c,ms,param nb_args s)]
			      | `Static _ -> assert false (* already treated above *)
			  in
			  let checks = 
			    if ch_link then
			      [(Check (CheckLink instr)); Check (CheckNullPointer e0)]
			    else
			      [Check (CheckNullPointer e0)]
			  in
			    (match ms_rtype ms with
			       | None ->
				   clean dico ssa fresh_counter is_heap_sensible_element_in_expr s_next (checks@(ins None))
			       | Some t ->
				   let y = make_tempvar dico ssa fresh_counter s next_store in
				     clean dico ssa fresh_counter is_heap_sensible_element_in_expr (E (Var (t,y))::s_next) (checks@(ins (Some y)))
			    ))
	       end)
      end
  | OpNew c as instr-> 
      let instrs = 
	if ch_link then
	  [Check (CheckLink instr); MayInit c]
	else
	  [MayInit c]
      in
      let s,instrs = 
	clean dico ssa fresh_counter is_heap_sensible_element_in_expr
	  s
	  instrs
      in
	(Uninit (c,i))::s, instrs
  | OpNewArray t as instr->
      let x = make_tempvar dico ssa fresh_counter s next_store in
      let dim = topE s in
	if ch_link then
	  match t with
	      TBasic _ -> E (Var (TObject (TArray t),x))::(pop s), [Check (CheckNegativeArraySize dim); NewArray (x,t,[dim])]
	    | TObject _ -> 
		E (Var (TObject (TArray t),x))::(pop s), [Check (CheckLink instr); Check (CheckNegativeArraySize dim); NewArray (x,t,[dim])]
	else
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
  | OpCheckCast t as instr -> 
      let instrs = 
	if ch_link then 
	  [Check (CheckLink instr); Check (CheckCast (topE s,t))]
	else
	  [Check (CheckCast (topE s,t))]
      in
	to_addr3_unop dico mode (Cast t) ssa fresh_counter s instrs
  | OpInstanceOf c as instr -> 
      let check_instr =
	if ch_link then
	  [Check (CheckLink instr)]
	else
	  []
      in
	to_addr3_unop dico mode (InstanceOf c) ssa fresh_counter s check_instr
  | OpMonitorEnter ->
     let r = topE s in
     (* we take care of reordering between memory accesses and lock *)
     let s, instrs = clean dico ssa fresh_counter is_heap_sensible_element_in_expr s [] in 
     pop s, instrs@[Check (CheckNullPointer r); MonitorEnter r]
  | OpMonitorExit ->
     let r = topE s in
     (* we take care of reordering between memory accesses and unlock *)
     let s, instrs = clean dico ssa fresh_counter is_heap_sensible_element_in_expr s [] in
     pop s, instrs@[Check (CheckNullPointer r); MonitorExit r]
  | OpAMultiNewArray (cn,dim) as instr ->
      let x = make_tempvar dico ssa fresh_counter s next_store in
      let params = param dim s in
      let (exp,instrs) = 
	E (Var (TObject cn,x))::(popn dim s),(List.map (fun e -> Check (CheckNegativeArraySize e)) params)@[NewArray (x,remove_dim cn dim,params)]
      in
	if ch_link then 
	  exp,(Check (CheckLink instr))::instrs
	else
	  exp,instrs
  | OpBreakpoint -> failwith "breakpoint"
  | OpInvalid -> failwith "invalid"

let is_jump_instr = function
  | OpIfCmp _
  | OpTableSwitch _
  | OpLookupSwitch _
  | OpIf _
  | OpGoto _ -> true
  | _ -> false

module MapPc = Ptmap

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
  (* [Type_constraint_on_Uninit] is raised when the requirements about stacks
      for folding constructors are not satisfied. *)

exception Content_constraint_on_Uninit
  (* [Content_constraint_on_Uninit] is raised when the requirements about
      stacks for folding constructors are not satisfied. *)

exception InconsistentDebugInfo of int*int*int

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


module FastCheckInfoDebug = struct
  (* Check that debug info on local variables are consistent in
     bytecode representation during transformation:

     (1) Context must be initialized at pc=0: arguments of method using
     pp_var info at pc=0 and other local variables at UnDef value.
     Context is initialized with debug info of local_variable_table on
     the fly for pcs not initialized by propagation before access to
     their context.  + Little hack: we initialize variables that never
     have a name in method at NoName.

     (2) Retrieve and propagate debug info of a variable when a OpStore
     is done (from the pc beyond the OpStore).  Propagate info to
     successors of [pc]: if successor is a jump target do (3).

     (3) Check that our propagated debug info at a pc (for each variable
     on which we have a debug info) is consistent with debug info of pc
     successors that are jump_target pcs and propagate information
     regarding the special case. 

     (4) Check at each use of a variable (OpLoad, OpIInc) that our
     propagated info on this variable is the same that the one in the
     local_variable_table (using pp_var =
     JCode.get_local_variable_info).

     => If verified: we can use debug info on variables in
     transformation. If not it could be a false positive case (forward
     jump not always handled, ...).

  *)

  type var_name_deb = UnDef | NoName | Name of string

  let pp_var2vardeb f = 
    fun x i -> 
      match f x i with 
	  None -> NoName 
	| Some s -> Name s

  (*[run] checks debug information given by the bytecode
    method during the code transformation (flow insensitive
    iteration). It's a fast verification and it could fails in
    particular cases: - jump forward in the code ...*)
  let run debugi jump_target pp_var darray code = 
    let succs = succs code in
    let new_as_succ = 
      let noname_var_list = 
	(* Little hack: if a variable has never name in debug table we
	   could consider that it's always a NoName info.  We must use
	   this information in first pc of method to propagate it in our
	   debug info table and when we use the class debug info
	   table.  *)
	let noname_array = 
	  (Array.init (code.c_max_locals) (fun vnum -> (vnum,true))) 
	in
	  List.iter
	    (fun (_start,_len,_name,_,i) -> 
	       noname_array.(i) <- (i,false)
	    )
	    debugi;
	  Array.fold_right
	    (fun (vnum,always_noname) noname_list -> 
	       if always_noname
	       then 
		 (* End of initialization of darray *)
		 (darray.(0) <- Ptmap.add vnum NoName darray.(0);
		  vnum::noname_list)
	       else noname_list)
	    noname_array
	    []
      in
	(fun as_succ pc -> 
	   (* Initialize a pc with debug
	      info of local_variable_table *)
	   let init_by_default pc =
	     List.iter
	       (fun noname_var -> 
		  darray.(pc) <- Ptmap.add noname_var NoName darray.(pc))
	       noname_var_list;
	     List.iter
	       (fun (start,len,name,_,i) -> 
		  if pc>=start && pc < start + len
		  then 
		    darray.(pc) <- Ptmap.add i (Name name) darray.(pc))
	       debugi
	   in
	     if Ptset.mem pc as_succ 
	     then as_succ 
	     else
	       begin
		 init_by_default pc;
		 Ptset.add pc as_succ
	       end)
    in
    let iter_on_lvars = (Array.make (code.c_max_locals) 0) in
      (fun as_succ pc -> 
	 let op = code.c_code.(pc) in
	 let succs = succs pc in
	   (* If current [pc] was not the successor of a precedent
	      program point, then initialize its context with
	      local_variable_table information*)
	 let as_succ = new_as_succ as_succ pc in
	   (* see (3) *)
	 let check_all as_succ dom_bj darray jump_pc =
	   (* If [jump_pc] was not the successor of a
	      precedent program point, then initialize its context with
	      local_variable_table information *)
	   let as_succ = new_as_succ as_succ jump_pc in
	     Array.iteri
	       (fun i _ -> 
		  match
		    (Ptmap.mem i dom_bj,
		     Ptmap.mem i darray.(jump_pc))
		  with
		      (* We only have info of local_variable_table in
			 both pcs for the variable i (that is to say no
			 information in this case)*)
		      (false,false) -> ()

		    (* There is a name information on variable on each pc :
		       
		       - Check if it is the same info: 
		       
		       ** if it is ok info is consistent, no change
		       
		       ** if not: check if we already propagate info from
		       the [jump_pc] point (pc > jump_pc):

		       +++ if it is the case: check if value is UnDef, if
		       yes the variable could not have been accessed without a
		       definition (OpStore) so it's ok, if no raise an
		       Exception because variable may have been accessed
		       with an uncorrect name

		       +++ if it is not the case (pc < jump_pc): put the
		       value of variable at jump_pc to UnDef, it forbids to
		       use the variable without a definition after jump_pc.

		    *)
		    | (true,true) -> 
			let bj, aj = Ptmap.find i dom_bj, Ptmap.find i darray.(jump_pc) in
			  if not (bj = aj) 
			  then 
			    if jump_pc > pc
			    then
			      darray.(jump_pc) <- Ptmap.add i UnDef darray.(jump_pc)
			    else
			      if aj = UnDef 
			      then () 
			      else
				raise (InconsistentDebugInfo (pc,jump_pc,i))

		    (* There is information on variable at [pc] but no
		       information at [jump_pc]:
		       
		       - Check if we already propagate info from the [jump_pc]
		       point (case: jump_pc < pc):

		       ** If it is the case: ok variable must not have been
		       accessed without a definition or a merge with a NoName
		       value

		       ** If not (case: jump_pc > pc): IF variable at [pc] has a
		       NoName value THEN it's compatible with no information =>
		       put variable at NoName in [jump_pc] ELSE it's not
		       compatible with no information => put variable at UnDef
		       in [jump_pc].

		    *)
		    | (true,false) -> 
			if jump_pc > pc 
			then 
			  if (Ptmap.find i dom_bj = NoName) 
			  then darray.(jump_pc) <- Ptmap.add i NoName darray.(jump_pc)
			  else darray.(jump_pc) <- Ptmap.add i UnDef darray.(jump_pc)
			else () 

		    (* There is no information on variable at [pc] but
		       information at [jump_pc]:

		       - Check if we already propagate info from the [jump_pc]
		       point (case: jump_pc < pc):

		       ** If it is the case: 

		       IF variable at [jump_pc] has value UnDef THEN the
		       variable could not have been accessed without a
		       definition => continue propagation ELSE raise an
		       Exception because variable may have been accessed
		       with an uncorrect name

		       ** If not (case : jump_pc > pc): IF variable at
		       [jump_pc] has a NoName value THEN it's compatible
		       with no information => continue propagation (but
		       do not allow to access to variable because a
		       store with no name has not been done before) ELSE
		       it's not compatible with no information => put
		       variable at UnDef in [jump_pc].  *)
		    | (false,true) -> 
			let aj = Ptmap.find i darray.(jump_pc) in
			  if jump_pc > pc
			  then 
			    if (aj = NoName)
			    then ()
			    else darray.(jump_pc) <- Ptmap.add i UnDef darray.(jump_pc)
			  else 
			    if (aj = UnDef)
			    then () 
			    else
			      raise (InconsistentDebugInfo (pc,jump_pc,i)))
	       iter_on_lvars;
	     as_succ
	 in
	   
	 let apply_instr_context darray cur_context = 
	   try
	     (List.fold_left 
		(fun as_succ i -> 
		   let j_targ = jump_target.(i) in
		     if j_targ
		       (* case (3)*)
		     then check_all as_succ cur_context darray i
		       (* case (2) *)
		     else 
		       (darray.(i) <- cur_context; 
			(* Add successor [i] as a program point
			   initialized *)
			Ptset.add i as_succ)
		) 
		as_succ
		succs,true)
	   with InconsistentDebugInfo _ -> (as_succ,false)
	 in
	   match op with 
	       OpStore (_,n) -> 
		 apply_instr_context darray (Ptmap.add n (pp_var pc n) darray.(pc))
	     | OpLoad (_,n)
	     | OpIInc (n,_) -> 
		 let ok = 
		   (* case (4) *)
		   try 
		     Ptmap.find n darray.(pc) = (pp_var pc n)
		   with Not_found -> false
		 in
		   if not ok 
		   then (as_succ,false)
		   else apply_instr_context darray darray.(pc)

	     | _ -> apply_instr_context darray darray.(pc))

end (*End of FastCheckInfoDebug module*)

(* This module allows to check debug information on variables on IR
   code transformed.  This verification must be done only if the fast
   verification, done by function [FastCheckInfoDebug.run], failed during
   transformation (could be a false positive).*)
module CheckInfoDebug = struct

  module Lat = struct
    (* canonic lattice on (bytecode variable (int) -> Name
       information) *)
    type t = FastCheckInfoDebug.var_name_deb Ptmap.t

    let bot = Ptmap.empty

    let join = Ptmap.merge 
		 (fun ninfo1 ninfo2 -> 
		    if not (ninfo1 = ninfo2) 
		    then FastCheckInfoDebug.UnDef
		    else ninfo1)

    let get m x = 
      Ptmap.find x m

    let order m1 m2 =
      try
	Ptmap.fold
	  (fun i s b -> 
	     let s2 = (get m2 i) in 
	       b && ((s = s2) || s2 = FastCheckInfoDebug.UnDef)
	  )
	  m1 true
      with Not_found -> false

    let to_string ab =
      let dom_to_string v =
	match v with
	    FastCheckInfoDebug.Name s -> "Name("^s^")"
	  | FastCheckInfoDebug.NoName -> "NoName"
	  | FastCheckInfoDebug.UnDef -> "UnDef"
      in
	Ptmap.fold (fun i vdom -> Printf.sprintf "%d:%s %s"
		      i
		      (dom_to_string vdom)) ab ""
  end 
    
  type pc = int
  type transfer = 
    | NoP
    | KillGen of var * int

  let transfer_to_string = function 
    | NoP -> "NoP"
    | KillGen (x,i) -> Printf.sprintf "KillGen(%s,%d)" (var_name_g x) i
	
  let eval_transfer = function
    | NoP -> (fun ab -> ab)
    | KillGen (x,_i) -> 
	(match bc_num x with
	     Some xnum -> 
	       (match var_name_debug x with
		    None -> Ptmap.add xnum FastCheckInfoDebug.NoName
		  | Some name -> 
		      Ptmap.add xnum (FastCheckInfoDebug.Name name))
	   | None -> assert false)

  (* [gen_instrs i instr] computes a list of transfert function
     [(f,j);...] with [j] the successor of instruction [i] for the
     transfert function [f]. *)
  let gen_instrs i = function
    | Ifd (_, j) -> [(NoP,j);(NoP,i+1)]
    | Goto j -> [NoP,j]
    | Throw _
    | Return _  -> []
    | AffectVar (x,_) 
    | NewArray (x,_,_)
    | New (x,_,_,_) 
    | InvokeStatic (Some x,_,_,_) 
    | InvokeVirtual (Some x,_,_,_,_) 
    | InvokeNonVirtual (Some x,_,_,_,_) -> 
	if var_orig x
	then
	  [KillGen (x,i),i+1]
	else [NoP,i+1]
    | MonitorEnter _
    | MonitorExit _ 
    | AffectStaticField _
    | AffectField _
    | AffectArray _
    | InvokeStatic _
    | InvokeVirtual _ 
    | InvokeNonVirtual _ 
    | MayInit _ 
    | Check _
    | Formula _
    | Nop -> [NoP,i+1]

  (* generate a list of transfer functions *)
  let gen_symbolic m : (pc * transfer * pc) list = 
    JUtil.foldi 
      (fun i ins l ->
	 List.rev_append
	   (List.map (fun (c,j) -> (i,c,j)) (gen_instrs i ins))
	   l) 
      (List.map (fun (i,e) -> (i,NoP,e.e_handler)) (bir_exception_edges m))
      m.bir_code

  let init params =
    List.fold_right
      (fun (_,x) -> 
	 match bc_num x with
	     Some i -> 
	       (match var_name_debug x with
		    None -> Ptmap.add i FastCheckInfoDebug.NoName
		  | Some s -> Ptmap.add i (FastCheckInfoDebug.Name s))
	   | None -> assert false)
      params
      Ptmap.empty

  (* original vars that appear in an expression*)
  let rec vars = function
    | Const _ 
    | StaticField _ -> VarSet.empty
    | Binop (_,e1,e2) -> VarSet.union (vars e1) (vars e2)
    | Field (e,_,_)
    | Unop (_,e) -> vars e
    | Var (_,x) ->
	if var_orig x 
	then
	  VarSet.singleton x
	else
	  VarSet.empty
	    
  (* [check_info_on_vars_access code res] checks, given the analysis
     result [res], that the variables accessed in [code] are accessed
     with the same name they were affected before. If it is not the
     case it raises an InconsistentDebugInfo exception.*)
  let check_info_on_vars_access code res = 
    let get_accessed_vars = 
      let vars_in_check = 
	function
	  | CheckNullPointer e
	  | CheckNegativeArraySize e
	  | CheckCast (e,_)
	  | CheckArithmetic e -> vars e
	  | CheckArrayBound (e1,e2)
	  | CheckArrayStore (e1,e2) -> VarSet.union (vars e1) (vars e2)
	  | CheckLink _ -> VarSet.empty in
      let rec vars_in_formula = function
	| BoolVar x -> vars x
	| Atom (_,e1,e2) -> VarSet.union (vars e1) (vars e2)
	| And (f1,f2) | Or (f1,f2) -> VarSet.union (vars_in_formula f1) (vars_in_formula f2)
      in
	function
	  | Nop 
	  | MayInit _ 
	  | Goto _ -> VarSet.empty
	  | Throw e
	  | AffectVar (_,e) 
	  | MonitorEnter e
	  | MonitorExit e
	  | AffectStaticField (_,_,e) -> vars e
	  | AffectField (e1,_,_,e2)
	  | Ifd ((_,e1,e2),_) -> VarSet.union (vars e1) (vars e2)
	  | AffectArray (e1,e2,e3) -> 
	      VarSet.union (vars e1) (VarSet.union (vars e2) (vars e3))
	  | NewArray (_,_,el)
	  | New (_,_,_,el) 
	  | InvokeStatic (_,_,_,el) -> 
	      List.fold_left 
		(fun set e -> VarSet.union (vars e) set)
		VarSet.empty
		el
	  | InvokeVirtual (_,e,_,_,el) 
	  | InvokeNonVirtual (_,e,_,_,el) -> 
	      List.fold_left 
		(fun set e -> VarSet.union (vars e) set)
		(vars e)
		el
	  | Return e_opt -> 
	      begin match e_opt with
		  None -> VarSet.empty
		| Some e -> vars e
	      end
	  | Check check -> vars_in_check check
	  | Formula (_,f) -> vars_in_formula f
    in
      Array.iteri
	(fun i ins -> 
	   let resi = res i in
	     VarSet.iter
	       (fun var -> 
		  try 
		    let bcvar = match bc_num var with 
			Some i -> i | None -> assert false
		    in
		    let var_name = 
		      match var_name_debug var with
			  None -> FastCheckInfoDebug.NoName
			| Some s -> FastCheckInfoDebug.Name s in
		      if not (Lat.get resi bcvar = var_name)
		      then
			(raise (InconsistentDebugInfo (i,i,bcvar)))
		  with Not_found -> ()
	       )
	       (get_accessed_vars ins)
	)
	code

  let run m =
    let init = init m.bir_params in
    let res =
      Iter.run 
	{
	  Iter.bot = Lat.bot ;
	  Iter.join = Lat.join;
	  Iter.leq = Lat.order;
	  Iter.eval = eval_transfer;
	  Iter.normalize = (fun x -> x);
	  Iter.size = Array.length m.bir_code;
	  Iter.workset_strategy = Iter.Incr;
	  Iter.cstrs = gen_symbolic m;
	  Iter.init_points = [0];
	  Iter.init_value = (fun _ -> init); 
	  Iter.verbose = false;
	  Iter.dom_to_string = Lat.to_string;
	  Iter.transfer_to_string = transfer_to_string
	}
    in
      check_info_on_vars_access m.bir_code res

  let to_string = Lat.to_string
		    
end (*End of CheckInfoDebug module*)


(* [bc2ir] returns (ir_code,info_ok). [info_ok:bool] indicates that debug
   information on local variables were checked with success with FastCheckDebugInfo.
   [ir_code:(pc * instr list) list]: is the code transformed, [instr
   list] is the list of instructions generated at bytecode program
   point [pc], generation could have used precedent program points of
   [pc].*)
let bc2ir no_debug dico mode ch_link ssa pp_var jump_target load_type arrayload_type cm code =
  let rec loop (ch_debug,info_ok) as_ts_jump ins ts_in as_in pc fresh_counter =
    let (nch_debug,ninfo_ok) = 
      match ch_debug with 
	  None -> (None,info_ok) (* it should be in case there is no debug info available *)
	| Some (check_fun,as_succ) -> 
	    let (new_as_succ,info_ok) = check_fun as_succ pc in
	      if info_ok
	      then (Some (check_fun,new_as_succ),true)
	      else (None,false)
    in
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
    let next_is_junc_point_or_a_goto =
      let next_pc = try next code.c_code pc with End_of_method -> pc in
	match code.c_code.(next_pc) with
	  | OpGoto _ -> true
	  | _ -> jump_target.(next_pc) 
    in
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
    let succs = normal_next code pc in
    let jump_succs = List.filter (fun i -> jump_target.(i)) succs in
    let op = code.c_code.(pc) in
    let ts_out = type_next op ts_in in
    let (as_out,instrs) = bc2bir_instr dico mode pp_var ch_link ssa fresh_counter pc load_type arrayload_type ts_in as_in next_store next_is_junc_point_or_a_goto op  in

      (* fail on backward branchings on a non-empty stack *)
      if List.length as_out>0 then
	if (List.exists (fun j -> j<pc) succs) then raise NonemptyStack_backward_jump;
      let branch_assigns = para_assign dico pc jump_succs as_out in
      let ins =
	if is_jump_instr op then
	  (pc,branch_assigns@instrs)::ins
	else
	  (pc,instrs@branch_assigns)::ins
      in
      let as_ts_jump =
	List.fold_left
	  (fun as_jump pc' ->
	     try
	       let (ts_jmp,as_jmp) = MapPc.find pc' as_jump in
		 (* check constraint on expr uninit and jumping forward
		    on a non empty stack all defined predecessor advice
		    must match what is reached *)
		 if (ts_jmp <> ts_out) then raise Type_constraint_on_Uninit ;
		 let jmp_s =  jump_stack dico pc' as_out in
		   if (not (value_compare_stack as_jmp jmp_s)) then
		     ( Printf.printf "\n %s\n" (string_of_int pc') ;
		       Printf.printf "%s \n" (print_stackmap as_jmp) ;
		       Printf.printf "%s \n" (print_stackmap jmp_s) ;
		       raise Content_constraint_on_Uninit )
		   else as_jump
	     with Not_found ->
	       (* when first advice for pc', no constraint to check. add
		  the advice in the map *)
	       let st = jump_stack dico pc' as_out in
		 MapPc.add pc' (ts_out,st) as_jump)
	  as_ts_jump
	  jump_succs in
	try
	  loop (nch_debug,ninfo_ok) as_ts_jump ins ts_out as_out (next code.c_code pc) fresh_counter
	with End_of_method ->  (ins,ninfo_ok)
  in
    (* Initialize context for checking debug information on
       variables (see FastCheckInfoDebug.run function)*)
  let ch_debug_init = 
    if no_debug then None
    else
      match code.c_local_variable_table with
	  None -> None
	| Some debugi -> 
	    let pp_var = FastCheckInfoDebug.pp_var2vardeb pp_var in
	    let darray = 
	      Array.make (Array.length code.c_code) Ptmap.empty 
	    in
	      (* initialize method's arguments with a valid value
		 (they are considered as assigned at first pc...) and
		 other local variables with UnDef value *)
	      (* Caution: since a little hack is done, darray is also
		 initialized with NoName in 'FastCheckInfoDebug.run' function for
		 variables that never have name in the whole method code*)
	    let args_numb = 
	      (List.fold_left 
		 (fun nb_loc_arg -> 
		    function  
			TBasic jbt when ((jbt = `Double) || (jbt = `Long)) ->
			  nb_loc_arg + 2
		      | _ -> nb_loc_arg + 1)
		 (if cm.cm_static then 0 else 1)
		 (ms_args cm.cm_signature))
	    in
	      (Array.iteri
		 (fun i _ -> 
		    if i < args_numb
		    then
		      darray.(0) <- Ptmap.add i (pp_var 0 i) darray.(0)
		    else
		      darray.(0) <- Ptmap.add i FastCheckInfoDebug.UnDef darray.(0))
		 (Array.make 
		    (code.c_max_locals)
		    ()));
	      let ch_fun = FastCheckInfoDebug.run debugi jump_target pp_var darray code in
		Some (ch_fun,Ptset.singleton 0)
  in
    loop (ch_debug_init,true) MapPc.empty [] [] [] 0 (ref 0)

let search_name_localvar no_debug static code i x =
  if x=0 && (not static) then Some "this"
  else 
    if no_debug then None
    else
      match JCode.get_local_variable_info x i code with
	| None ->  None
	| Some (s,_) -> Some s

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

(* [compress_ir handlers ir jump_target] try to put minimum program
   point value for new instructions by modifying the pp of
   instructions when precedent pp has no instructions. Instructions
   that are jump_targets or handlers bounds keep the same program
   point. 

   [ir:(pc * instr list) list]: [instr list] is the list of
   instructions generated at bytecode program point [pc] (see [bc2ir]
   function).

   Returns: (bytecode_pc (for ir2bc corresp), (bytecode_pc list (for bc2ir corresp),instr) list)) list 
   
   *bytecode_pc (for ir2bc corresp): corresponds to the last bytecode
   instruction used for generate the ir instruction

   *bytecode_pc: contains all bytecode pc included in the ir
   instruction (~all bytecode instructions used for the generation of
   the IR instruction: using the compression and except corner cases
   (OpNew instruction, etc.))
*)
let compress_ir handlers ir jump_target =
  let h_ends = List.fold_right 
		 (fun e s -> Ptset.add e.JCode.e_end (Ptset.add e.JCode.e_start s)) 
		 handlers Ptset.empty 
  in
    (*pc0 always included in pcl*)
  let rec aux0 pc0 pcl = function
    | [] -> [pc0,[pcl,Nop]]
    | (pc,_)::_ as l when jump_target.(pc) || Ptset.mem pc h_ends -> (pc0,[pcl,Nop])::(aux l)
    | (pc1,[])::q -> aux0 pc0 (pc1::pcl) q
    | (pc,instrs)::q -> (pc,List.map (fun i -> (pc::pcl,i)) instrs)::(aux q)
  and aux = function
    | [] -> []
    | (pc,[])::q -> aux0 pc [pc] q
    | (pc,instrs)::q -> (pc,List.map (fun i -> ([pc],i)) instrs)::(aux q)
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

(* only used for debugging *)
let print_unflattened_code code =
  List.iter 
    (fun (i,instrs) -> 
       List.iter (fun (pc,op) -> Printf.printf"%3d (%3d) : %s\n" i pc (print_instr op)) instrs)
    code;
  print_newline ()
let print_unflattened_code_uncompress code =
  List.iter 
    (fun (i,instrs) -> 
       List.iter (fun op -> Printf.printf"%3d : %s\n" i (print_instr op)) instrs)
    code;
  print_newline ()
    
(* [flatten_code code exc_tbl] given the compressed representation
   [code] (see [flatten_code] call for description) and the exception
   table, returns a sequential code (instructions pcs are sequentials:
   +1 for next instruction), the pc correspondance table (and map)
   between IR and BC (resp. BC and IR), and the exception table (pcs
   modified with new pcs ...).*)
let flatten_code code exc_tbl =
  (* starting from i, and given the code [code], computes a triple *)
  let rec aux i map = function
      [] -> [], [], map
    | (pc,instrs)::q ->
	let (instrs',pc_list,map) = aux (i+List.length instrs) map q in
	  (List.map snd instrs)@instrs', 
          (* flatten_code the code but does not modify the instruction in it *)
	  (List.map (fun _ -> pc) instrs)@pc_list,
	  (* the list of first corresponding bytecode pc for ir instrs *)
	  List.fold_left 
	    (fun map (pcl,_) -> 
	       List.fold_left
		 (fun map pc0 -> Ptmap.add pc0 i map)
		 map
		 pcl
	    ) 
	    map
	    instrs
	    (* map from initial pcs to the future pc they are mapped to *)
  in 
  let (instrs,pc_list,map) = aux 0 Ptmap.empty code in
  let find i = (* find the flattened pc associated to i *)
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
    (instrs,(Array.of_list pc_list), map, exc_tbl)

(* find predecessors of instructions ...*)
let find_preds_instrs code handlers = 
  let pred_pcs = Array.make (Array.length code) Ptset.empty in
    pred_pcs.(0) <- Ptset.add (-1) pred_pcs.(0);
    Array.iteri
      (fun i ins -> 
	 match ins with
	   | Ifd (_ , j) -> 
	       pred_pcs.(i+1) <- Ptset.add i pred_pcs.(i+1); 
	       pred_pcs.(j) <- Ptset.add i pred_pcs.(j);
	   | Goto j -> pred_pcs.(j) <- Ptset.add i pred_pcs.(j)
	   | Throw _
	   | Return _ -> ()
	   | _ -> pred_pcs.(i+1) <- Ptset.add i pred_pcs.(i+1)
      )
      code;
    (* add predecessors of handlers (pcs at which the exception
       handler is active) *)
    List.iter 
      (fun e -> 
	 let in_scope = ref e.e_start in
	   while !in_scope < e.e_end do
	     pred_pcs.(e.e_handler) <- Ptset.add !in_scope pred_pcs.(e.e_handler);
	     in_scope := succ !in_scope
	   done
      ) 
      handlers;
    pred_pcs

(* remove instructions of code without predecessors and modify jump,
   handlers and correspondance tables in consequence. *)
let rec remove_dead_instrs code ir2bc bc2ir handlers = 
  let predsi = find_preds_instrs code handlers in
  let succ_of_dead = ref Ptset.empty in
  let nb_dead = ref 0 in
    (* returns true if pc has no predecessors (excepting itself) *)
  let has_no_preds pc = 
    (* provide a way to manage a strange code in which a handler is
       covering its own code and that is its own predecessor (=> dead
       code) *)
    let is_its_own_pred _ = 
      let (pred,rest) = Ptset.choose_and_remove predsi.(pc) in
        pred == pc && Ptset.is_empty rest
    in
      (* first instruction has (-1) as predecessor and is not a dead
         instr *)
      pc >= 0 &&
        (Ptset.is_empty predsi.(pc) || is_its_own_pred ())
  in
    (* calculate program point correspondance between old code and
       code without dead instructions *)
  let new_code_corresp = 
    Array.mapi
      (fun i _preds -> 
         (* new code pc after removing precedent dead instrs *)
         let ni = i - !nb_dead in
	   if has_no_preds i
	   then 
	     ( (* this program point will not exist anymore but we keep
		  a correspondance to modify handlers easily (case of
		  removed instruction on one of the handlers range limits) *)
	       nb_dead := succ !nb_dead;
	       ni)
	   else
	     ni)
      predsi
  in
    if !nb_dead > 0
    then
      begin
	let new_length = Array.length code - !nb_dead in
	let new_code = Array.make new_length Nop in
	let new_ir2bc = Array.make new_length (-1) in
	let nb_dead_current = ref 0 in
	  (* create new array code and new ir to bc correspondance
	     table *)
	let _ =
	  Array.iteri
	    (fun i ins -> 
	       if has_no_preds i
	       then
		 nb_dead_current := succ !nb_dead_current
	       else
		 ((* If current pc has only one predecessor and this
                     one has no predecessors => it is the successor of
                     a dead instruction. *)
                   if Ptset.cardinal predsi.(i) = 1 
                     && has_no_preds (Ptset.choose predsi.(i))
		   then succ_of_dead := Ptset.add i !succ_of_dead;
                   (* generate ir to bytecode correspondance *)
		   new_ir2bc.(i - !nb_dead_current) <- ir2bc.(i);
                   (* generate instruction in new code modifying jump
                      pc if it is a jump instruction *)
		   new_code.(i - !nb_dead_current) <- 
		     match ins with
			 Goto pc -> Goto (new_code_corresp.(pc))
		       | Ifd (g, pc) -> Ifd (g, new_code_corresp.(pc))
		       | ins -> ins)
	    )
	    code;
	in
          (* generate bytecode to ir correspondance *)
	let new_bc2ir = 
	  let to_remove = ref [] in
	  let bc2ir = 
	    Ptmap.mapi 
	      (fun bc ir -> 
		 if has_no_preds ir
		 then (to_remove := bc::(!to_remove); -1)
		 else
		   new_code_corresp.(ir)) 
	      bc2ir 
	  in
	    List.fold_left
	      (fun map bc -> Ptmap.remove bc map)
	      bc2ir
	      !to_remove
	in
	let new_exc_tbl = 
	  List.fold_right
	    (fun e new_h -> 
               let h = 
		 { e_start = new_code_corresp.(e.e_start);
		   e_end = new_code_corresp.(e.e_end);
		   e_handler = new_code_corresp.(e.e_handler);
		   e_catch_type = e.e_catch_type;
		   e_catch_var = e.e_catch_var
		 }
	       in
		 (* A handler could cover only a dead instruction ...*)
                 (* and it is not (h.e_end-1) - h.e_start > 0 because
                    of correspondance calculated in
                    new_code_corresp! *)
		 if h.e_end - h.e_start > 0
		 then
		   h::new_h
		 else
		   new_h
	    ) 
	    handlers
	    []
	in 
	  if Ptset.is_empty !succ_of_dead
	  then
	    (new_code,new_ir2bc,new_bc2ir,new_exc_tbl)
	  else
	    remove_dead_instrs new_code new_ir2bc new_bc2ir new_exc_tbl
      end
    else
      (code,ir2bc,bc2ir,handlers)      

(*For every pp, transform unreachable code to OpInvalid instrs, using the
  information given by the BCV. It also remove unreachable exception
  handlers.*)
let rm_dead_instr_from_bcv code is_typed =
  let rec rm_dead_instr_from_bcv' i =
    try 
      match (is_typed i) with
        | true -> rm_dead_instr_from_bcv' (next code.c_code i)
        | false -> code.c_code.(i) <- OpInvalid;
                   rm_dead_instr_from_bcv' (next code.c_code i)
    with End_of_method -> code 
  in
  let code = rm_dead_instr_from_bcv' 0 in
  let eh_list = 
    List.filter 
      (fun eh -> 
         match code.c_code.(eh.JCode.e_handler) with
           | OpInvalid -> 
               (*If e_handler is invalid, instructions between e_start and
                e_end must be invalid too. *)
               let rec check_is_invalid i i_end = 
                 match i with 
                   | cur_i when cur_i > i_end -1 -> ()
                   | cur_i when code.c_code.(cur_i) = OpInvalid ->
                       check_is_invalid (cur_i +1) i_end
                   | _ -> assert false
               in check_is_invalid eh.JCode.e_start eh.JCode.e_end; false
           | _ -> true
      ) 
      code.c_exc_tbl
  in {code with c_exc_tbl = eh_list}

let jsr_ir2bc_post_treatment (assoc: (int*int) list) (ir2bc: int array) : int array =
  let rec update_pp pp pp_list =
      match pp_list with
      | [] -> pp
      | (inline_pp, bc_pp)::tail -> if inline_pp = pp
				    then bc_pp
				    else update_pp pp tail in
  Array.iteri
    (fun i pp -> Array.set ir2bc i (update_pp pp assoc))
    ir2bc;
  ir2bc

let jcode2bir mode bcv ch_link ssa cm jcode =
  let code = jcode in
    match JsrInline.inline code with
      | Some (code, assoc) ->
	  (* [make_transformation no_debug] returns the code
	     transformation and a boolean value indicating if the
	     debug information on variables could be
	     trusted. [no_debug] indicates if the debug information
	     must be used in the transformation (if it exists). A fast
	     verification is done on debug information when it is use,
	     if the verification fails the boolean returned is
	     false.*)
	  let make_transformation no_debug = 
	    let pp_var = search_name_localvar no_debug cm.cm_static code in
	    let jump_target = compute_jump_target code in
            (* [is_typed i] : returns true is the current pp is typed (has been
               given a BCV.t type). If false, it means that i is unreachable.
               [load_type i] : Returns the type of the variable referenced by
               the OpLoad instruction at index i in the code.
               [arrayload_type jat pp] : Returns the type of the variable
               referenced by an OpArrayLoad instruction at index pp in the
               code. jat is a jvm_array_type, it is used only when [bcv] is
               false, which means that type checking is not done.
               *)
              (*TODO: detect dead code before running the BCV (replacing instrs
                by OpInvalid instrs). It would avoid to use the BCV for finding
                such information. *)
	    let (is_typed, load_type,arrayload_type) = BCV.run bcv cm code in
            (*rm_dead_instr_from_bcv should be run before starting bc2ir, that
              why we can't wait for remove_dead_instrs which work on bir
              code.*)
            let code = rm_dead_instr_from_bcv code is_typed in 
	    let dico = make_dictionary () in
	    let (res,debug_ok) = 
	      bc2ir no_debug dico mode ch_link ssa pp_var 
		jump_target load_type arrayload_type cm code
	    in
	      (*let _ = print_unflattened_code_uncompress (List.rev res) in*)
	    let ir_code = compress_ir code.c_exc_tbl (List.rev res) jump_target in
	    let ir_exc_tbl = List.map (make_exception_handler dico) code.c_exc_tbl in
	      (* let _ = print_unflattened_code ir_code in*)
	    let (ir_code,ir2bc,bc2ir,ir_exc_tbl) =  flatten_code ir_code ir_exc_tbl in
	    let (nir_code,nir2bc,nbc2ir,nir_exc_tbl) = 
	      remove_dead_instrs ir_code ir2bc bc2ir ir_exc_tbl
	    in
	      ({ bir_params = gen_params dico pp_var cm;
		 bir_vars = make_array_var dico;
		 bir_code = nir_code;
		 bir_pc_ir2bc = jsr_ir2bc_post_treatment assoc nir2bc;
		 bir_exc_tbl = nir_exc_tbl;
		 bir_line_number_table = code.c_line_number_table;
		 (* ssa *)
		 bir_preds = [||];
		 bir_phi_nodes = [||];
		 bir_mem_ssa = { 
		   mem_ssa_in = (fun _ -> raise No_memory_ssa_info_here);
		   mem_ssa_out = (fun _ -> raise No_memory_ssa_info_here);
		   mem_ssa_phi = (fun _ -> raise No_memory_ssa_info_here)
		 }
	       },
	       debug_ok)
	  in
	  let (ir_code, debug_ok) = make_transformation false in
	    if debug_ok
	    then ir_code
	    else 
	      begin
		try
		  CheckInfoDebug.run ir_code;
		  ir_code
		with InconsistentDebugInfo (pc,_topc,vari)-> 
		  prerr_endline 
		    ("Warning: Debug information of local_variable_table attribute of method "
		     ^(JPrint.class_method_signature cm.cm_class_method_signature)
		     ^" cannot be used for code transformation because it is inconsistent on localvar "
		     ^string_of_int vari^" at program point "^string_of_int pc^".\n");
		  fst (make_transformation true)
	      end  
      | None -> raise Subroutine


(* David: not used anywhere currently
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
				
end
*)

module Live = struct

  module Env = struct
    (* lattice of powerset of JBir variables *)
    include Set.Make(struct type t = var let compare v1 v2 = (index v1) - (index v2) end)

    let print_key  = var_name_g

    let bot = empty

    let rec vars acc = function
      | Const _ -> acc
      | Var (_,x) -> add x acc
      | Field (e,_,_) 
      | Unop (_,e) -> vars acc e
      | Binop (_,e1,e2) -> vars (vars acc e1) e2
      | StaticField _ -> acc

    (* [vars e] computes the set of variables that appear in expression [e]. *)
    let vars = vars empty

    let to_string ab =
      let ab = elements ab in
	match List.map print_key ab with
	  | [] -> "{}"
	  | [x] -> Printf.sprintf "{%s}" x
	  | x::q -> Printf.sprintf "{%s%s}" x (List.fold_right (fun x s -> ","^x^s) q "")	 
  end
    
  type transfer_fun =
    | GenVars of expr list 
	(* [GenVars l] : generate the set of variables that appear in some expressions in list [l] *)
    | Kill of var
	(* [Kill x] : remove variable [x] *)
	
  type transfer = transfer_fun list
  type pc = int

  let fun_to_string = function
    | GenVars e ->
	Printf.sprintf "GenVars(%s)" (String.concat "::" (List.map print_expr e))
    | Kill x ->
	Printf.sprintf "Kill(%s)" (var_name_g x)

  let transfer_to_string = function
    | [] -> ""
    | [f] -> fun_to_string f
    | f::q -> (fun_to_string f)^(List.fold_right (fun f s -> ";"^(fun_to_string f)^s) q "")
	
  let eval_transfer = function
    | GenVars l -> fun ab -> List.fold_right (fun e -> Env.union (Env.vars e)) l ab
	| Kill x -> fun ab -> Env.remove x ab

  let rec all_expr_in_formula acc = function
    | BoolVar x -> x::acc
    | Atom (_,e1,e2) -> e1::e2::acc
    | And (f1,f2) | Or (f1,f2) -> all_expr_in_formula (all_expr_in_formula acc f1) f2
  let all_expr_in_formula = all_expr_in_formula []

  (* [gen_instrs last i] computes a list of transfert function
     [(f,j);...] with [j] the successor of [i] for the transfert
     function [f]. [last] is the end label of the method; *)
  let gen_instrs last i = function
    | Ifd ((_,e1,e2), j) -> 
	let gen = GenVars [e1;e2] in [([gen],j);([gen],i+1)]
    | Goto j -> [[],j]
    | Throw _
    | Return None  -> []
    | Return (Some e)  ->  [[GenVars [e]],last]
    | AffectVar (x,e) -> [[GenVars [e]; Kill x],i+1]
    | NewArray (x,_,le)
    | New (x,_,_,le) 
    | InvokeStatic (Some x,_,_,le) ->  [[GenVars le;Kill x],i+1]
    | InvokeVirtual (Some x,e,_,_,le) 
    | InvokeNonVirtual (Some x,e,_,_,le) -> [[GenVars (e::le); Kill x],i+1]
    | MonitorEnter e 
    | MonitorExit e -> [[GenVars [e]],i+1]
    | AffectStaticField (_,_,e) -> [[GenVars [e]],i+1]
    | AffectField (e1,_,_,e2) -> [[GenVars [e1;e2]],i+1]
    | AffectArray (e1,e2,e3) -> [[GenVars [e1;e2;e3]],i+1]
    | InvokeStatic (None,_,_,le) -> [[GenVars le],i+1]
    | InvokeVirtual (None,e,_,_,le) 
    | InvokeNonVirtual (None,e,_,_,le) -> [[GenVars (e::le)],i+1]
    | MayInit _ 
    | Nop -> [[],i+1]
    | Check c -> begin
	match c with
	  | CheckArrayBound (e1,e2)
	  | CheckArrayStore (e1,e2) -> [[GenVars [e1;e2]],i+1]
	  | CheckNullPointer e
	  | CheckNegativeArraySize e
	  | CheckCast (e,_)
	  | CheckArithmetic e -> [[GenVars [e]],i+1]
	  | CheckLink _ -> [[],i+1]
      end
    | Formula (_,f) -> [[GenVars (all_expr_in_formula f)],i+1]

  (* generate a list of transfer functions *)
  let gen_symbolic m : (pc * transfer * pc) list = 
    let length = Array.length m.bir_code in
      JUtil.foldi 
	(fun i ins l ->
	   List.rev_append
	     (List.map (fun (c,j) -> (j,c,i)) (gen_instrs length i ins))
	     l) 
	(List.map (fun (i,e) -> (e.e_handler,[],i)) (bir_exception_edges m))
	m.bir_code

  let run m =
    Iter.run 
      {
	Iter.bot = Env.bot ;
	Iter.join = Env.union;
	Iter.leq = Env.subset;
	Iter.eval = List.fold_right eval_transfer;
	Iter.normalize = (fun x -> x);
	Iter.size = 1 + Array.length m.bir_code;
	Iter.workset_strategy = Iter.Decr;
	Iter.cstrs = gen_symbolic m;
	Iter.init_points = [Array.length m.bir_code];
	Iter.init_value = (fun _ -> Env.empty); (* useless here since we iterate from bottom *)
	Iter.verbose = false;
	Iter.dom_to_string = Env.to_string;
	Iter.transfer_to_string = transfer_to_string
      }


end

module SSA = struct

  let heap_index = -17

  let dominator instr_array preds =
    let all = 
      JUtil.foldi 
	(fun i _ -> Ptset.add i) (Ptset.singleton (-1)) instr_array 
    in
    let dom = Array.init
		(Array.length instr_array)
		(fun _ -> all) in
    let get_dom i =
      if i < 0 then Ptset.singleton (-1) else dom.(i) in
    let rec inter_list = function
	[] -> assert false
      | [x] -> get_dom x
      | x::q -> Ptset.inter (get_dom x) (inter_list q) in
    let change = ref true in
      while !change do
	change := false;
	Array.iteri
	  (fun i _ -> 
	     let new_s = 
	       Ptset.add i (inter_list (preds i)) 
	     in
	       if not (Ptset.subset dom.(i) new_s) then
		 begin
		   dom.(i) <- new_s;
		   change := true;
		 end)
	  dom	     
      done;
      dom
	
  (* build dominator tree *)
  let make_idom_tree aux =
    let assoc_list = 
      JUtil.foldi (fun i s l -> (Ptset.choose s,i)::l) [] aux in
    let assoc_list = List.sort (fun (i,_) (j,_) -> compare i j) assoc_list in
    let rec children i = function
	[] -> []
      | (j,p)::q ->
	  let c = compare i j in
	    if c>0 then children i q
	    else if c=0 then p::(children i q)
	    else [] in
      (fun i -> children i assoc_list)

  (* immediate dominator *)
  let idom dom =
    let n = Array.length dom in
    let dom_strict = Array.init n (fun i -> Ptset.remove i dom.(i)) in
    let aux = Array.init n (fun i -> dom_strict.(i)) in
      for i=0 to (n-1) do
	let workset = ref (Ptset.remove (-1) dom_strict.(i)) in
	  while not (Ptset.is_empty !workset) do
	    let j = Ptset.max_elt !workset in
	      workset := Ptset.diff !workset dom.(j);
	      aux.(i) <- Ptset.diff aux.(i) dom_strict.(j)
	  done;
      done;
      (fun i -> 
	 let s = aux.(i) in
	   assert (Ptset.cardinal s = 1);
	   Ptset.choose s), make_idom_tree aux
	
  (* dominance frontier set 
     See: 
     Cooper, Keith D.; Harvey, Timothy J.; and Kennedy, Ken (2001). 
     A Simple, Fast Dominance Algorithm *)
  let domf n preds idom = 
    let domf = Array.make (n+1) Ptset.empty in
      for i=0 to (n-1) do
	let preds = preds i in
	let idom_i = idom i in
	  if List.length preds > 1 then
	    List.iter
	      (fun p -> 
		 let runner = ref p in
		   while !runner <> idom_i do
		     domf.(!runner+1) <- Ptset.add i domf.(!runner+1);
		     runner := idom !runner 
		   done
	      )
	      preds 
      done;
      (fun i -> domf.(i+1))

  let show_digraph instr_array succs =
    let f = open_out "debug.dot" in
      Printf.fprintf f "digraph debug {\n";
      Array.iteri
	(fun i _ ->
	   List.iter 
	     (fun j -> Printf.fprintf f "  n%d -> n%d;\n" i j)
	     (succs i))
	instr_array;
      Printf.fprintf f "}\n";
      close_out f  

  (* see:  
     Cytron, Ron; Ferrante, Jeanne; Rosen, Barry K.; Wegman, Mark N.; 
     and Zadeck, F. Kenneth (1991). 
     "Efficiently computing static single assignment form and the 
     control dependence graph". 
     ACM Transactions on Programming Languages and Systems 13 (4): 451–490.*)
  let place_phi_nodes m n var_defs domf live =
    let place = ref Ptmap.empty in
    let place_node n v =
      place := Ptmap.add ~merge:Ptset.union n (Ptset.singleton v) !place in
    let iter_count = ref 0 in
    let has_already = Array.make (n+1) 0 in
    let work = Array.make (n+1) 0 in
    let workset = ref Ptset.empty in
      Ptmap.iter
	(fun v defs -> 
	   incr iter_count;
	   Ptset.iter 
	     (fun x -> 
		work.(x+1) <- !iter_count;
		workset := Ptset.add x !workset)
	     defs;
	   while not (Ptset.is_empty !workset) do
	     let x = Ptset.choose !workset in
	       workset := Ptset.remove x !workset;
	       Ptset.iter
		 (fun y -> 
		    if has_already.(y+1) < !iter_count then 
		      begin
			if v=heap_index || live y m.bir_vars.(v) then place_node y v;
			has_already.(y+1) <- !iter_count;
 			if work.(y+1) < !iter_count then 
			  begin
			    workset := Ptset.add y !workset;
			    work.(y+1) <- !iter_count
			  end
		      end)
		 (domf x)
	   done)
	var_defs;
      !place

  let debug_code m phi_nodes children var_defs search_h succs =
    Printf.printf "params(%s)\n"
      (JUtil.print_list_sep_id ","
	 (List.map 
	    (fun (_,x) -> var_name_g x) m.bir_params));
    Array.iteri 
      (fun i op -> 
	 Printf.printf "[%s]%3d: %s\n"
	   (JUtil.print_list_sep_id " "
	      (List.map 
		 (fun v -> var_name_g (m.bir_vars.(v))) 
		 (Ptmap.fold (fun v _ l -> v::l) (phi_nodes i) [])))
	   i (print_instr op))
      m.bir_code;
    List.iter
      (fun e -> Printf.printf " [%d, %d] --> %d\n" e.e_start e.e_end e.e_handler)
      m.bir_exc_tbl;
    Printf.printf "var_def:\n";
    Ptmap.iter
      (fun v defs ->
         match v with 
           | v when v = heap_index -> () (*not contained in ir_code.bir_vars *)
           | _ -> Printf.printf "   %s: {%s}\n"
                    (var_name_g (m.bir_vars.(v)))
                    (JUtil.print_list_sep_id "," 
                       (List.map string_of_int (Ptset.elements defs)))
      ) var_defs;
    Printf.printf "search: %s\n" 
      (JUtil.print_list_sep_id "::" 
	 (List.map 
	    (fun x ->
	       Printf.sprintf "%d(%s)"
		 x 
		 (JUtil.print_list_sep_id " " (List.map string_of_int (children x)))) search_h));
    show_digraph m.bir_code succs

  let use_bcvars =
    let rec vars acc = function
      | Const _ -> acc
      | Var (_,x) -> if catch_var x then acc else Ptset.add (index x) acc 
      | Field (e,_,_) -> vars (Ptset.add heap_index acc) e
      | Unop (_,e) -> vars acc e
      | Binop (ArrayLoad _,e1,e2) -> vars (vars (Ptset.add heap_index acc) e1) e2
      | Binop (_,e1,e2) -> vars (vars acc e1) e2
      | StaticField _ -> (Ptset.add heap_index acc) in
    let rec vars_f acc = function
      | BoolVar x ->  vars acc x
      | Atom (_,e1,e2) -> vars (vars acc e1) e2
      | And (f1,f2) | Or (f1,f2) -> vars_f (vars_f acc f1) f2 in
      function
	| AffectField (e1,_,_,e2) -> vars (vars (Ptset.add heap_index Ptset.empty) e1) e2
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
	| AffectStaticField (_,_,e) -> vars (Ptset.add heap_index Ptset.empty) e
	| NewArray (_,_,le)
	| New (_,_,_,le) 
	| InvokeStatic (_,_,_,le) -> List.fold_left vars (Ptset.add heap_index Ptset.empty) le
	| InvokeVirtual (_,e,_,_,le) 
	| InvokeNonVirtual (_,e,_,_,le) -> List.fold_left vars (Ptset.add heap_index Ptset.empty) (e::le)
	| AffectArray (e1,e2,e3) -> vars (vars (vars (Ptset.add heap_index Ptset.empty) e1) e2) e3
	| Check c -> begin
	    Ptset.add heap_index 
	      (match c with
		 | CheckArrayBound (e1,e2)
		 | CheckArrayStore (e1,e2) -> vars (vars Ptset.empty e1) e2
		 | CheckNullPointer e
		 | CheckNegativeArraySize e
		 | CheckCast (e,_)
		 | CheckArithmetic e -> vars Ptset.empty e
		 | CheckLink _ -> Ptset.empty)
	  end
	| Formula (_,f) -> vars_f Ptset.empty f

  let def_bcvar = function
    | NewArray (v,_,_)
    | New (v,_,_,_) 
    | InvokeStatic (Some v,_,_,_)
    | InvokeVirtual (Some v,_,_,_,_) 
    | InvokeNonVirtual (Some v,_,_,_,_) -> 
	Ptset.add heap_index (if catch_var v then Ptset.empty else Ptset.singleton (index v))
    | AffectVar (v,_) -> if catch_var v then Ptset.empty else Ptset.singleton (index v) 
    | AffectField _ 
    | AffectStaticField _
    | AffectArray _ -> Ptset.singleton heap_index
    | _ -> Ptset.empty

  let var_defs m =
    JUtil.foldi
      (fun i ins -> 
	 Ptset.fold 
	   (fun x defs -> 
	      Ptmap.add ~merge:Ptset.union x (Ptset.singleton i) defs)
	   (def_bcvar ins))
      (List.fold_right
	 (fun (_,x) -> Ptmap.add (index x) (Ptset.singleton (-1)))
	 m.bir_params 
	 (Ptmap.add heap_index (Ptset.singleton (-1)) Ptmap.empty))
      m.bir_code 

  let map_instr def use =
    let map_expr f =
      let rec aux expr = 
	match expr with
	  | Const c -> Const c
	  | StaticField (c,fs) -> StaticField (c,fs)
	  | Field (e,c,fs) -> Field (aux e,c,fs)
	  | Var (t,x) -> Var (t,f x)
	  | Unop (s,e) -> Unop (s,aux e)
	  | Binop (s,e1,e2) -> Binop (s,aux e1,aux e2)
      in aux 
    in
    let map_formula =
      let f = use in 
      let rec aux = function
        | BoolVar x -> BoolVar (map_expr f x)
	| Atom (cmp,e1,e2) -> Atom (cmp,map_expr f e1,map_expr f e2)
	| And (f1,f2) -> And (aux f1, aux f2) 
	| Or (f1,f2) -> Or (aux f1, aux f2)
      in aux 
    in
    let use = map_expr use in
      function
	| AffectField (e1,c,f0,e2) -> AffectField (use e1,c,f0,use e2)
	| Ifd ((c,e1,e2), pc) -> Ifd ((c,use e1,use e2), pc) 
	| Goto i -> Goto i
	| Throw e -> Throw (use e) 
	| MayInit c -> MayInit c
	| Nop -> Nop
	| Return None -> Return None
	| Return (Some e) -> Return (Some (use e))
	| AffectVar (x,e) -> AffectVar (def x,use e)
	| MonitorEnter e -> MonitorEnter (use e)
	| MonitorExit e -> MonitorExit (use e)
	| AffectStaticField (c,f0,e) -> AffectStaticField (c,f0,use e)
	| NewArray (x,t,le) -> NewArray (def x,t,List.map (use) le)
	| New (x,c,lt,le) -> New (def x,c,lt,List.map (use) le)
	| InvokeStatic (None,c,ms,le) -> InvokeStatic (None,c,ms,List.map (use) le)
	| InvokeStatic (Some x,c,ms,le) -> InvokeStatic (Some (def x),c,ms,List.map (use) le)
	| InvokeVirtual (None,e,c,ms,le) -> InvokeVirtual (None,use e,c,ms,List.map (use) le)
	| InvokeVirtual (Some x,e,c,ms,le) -> InvokeVirtual (Some (def x),use e,c,ms,List.map (use) le)
	| InvokeNonVirtual (None,e,c,ms,le) -> InvokeNonVirtual (None,use e,c,ms,List.map (use) le)
	| InvokeNonVirtual (Some x,e,c,ms,le) -> InvokeNonVirtual (Some (def x),use e,c,ms,List.map (use) le)
	| AffectArray (e1,e2,e3) -> AffectArray (use e1,use e2,use e3)
	| Formula (cmd,f) -> Formula (cmd, map_formula f)
	| Check c -> Check begin
	    match c with
	      | CheckArrayBound (e1,e2) -> CheckArrayBound (use e1,use e2)
	      | CheckArrayStore (e1,e2) -> CheckArrayStore (use e1,use e2)
	      | CheckNullPointer e -> CheckNullPointer (use e)
	      | CheckNegativeArraySize e -> CheckNegativeArraySize (use e)
	      | CheckCast (e,t) -> CheckCast (use e,t)
	      | CheckArithmetic e -> CheckArithmetic (use e)
	      | CheckLink op -> CheckLink op
	  end

  let map_exception_handler f e = {
    e_start = e.e_start;
    e_end = e.e_end;
    e_handler = e.e_handler;
    e_catch_type = e.e_catch_type;
    e_catch_var = f e.e_catch_var 0
  }

  let live_analysis ir_code = 
    let live = Live.run ir_code in
      fun i x  ->  Live.Env.mem x (live i)
	
  let preds m =
    let preds = Array.make (Array.length m.bir_code) Ptset.empty in
    let add_pred i j = preds.(i) <- Ptset.add j preds.(i) in
      add_pred 0 (-1);
      Array.iteri 
	(fun i ins ->
	   match ins with
	     | Ifd (_ , j) -> add_pred (i+1) i; add_pred j i
	     | Goto j -> add_pred j i
	     | Throw _
	     | Return _ -> ()
	     | _ -> add_pred (i+1) i) m.bir_code;
      List.iter
	(fun (i,e) -> add_pred e.e_handler i) (bir_exception_edges m);
      let preds = Array.map Ptset.elements preds in
      let preds i = preds.(i) in
	preds

  let succs m =
    let succs = Array.make (Array.length m.bir_code) Ptset.empty in
    let add i j = succs.(i) <- Ptset.add j succs.(i) in
      Array.iteri 
	(fun i ins ->
	   match ins with
	     | Ifd (_ , j) -> add i (i+1); add i j
	     | Goto j -> add i j
	     | Throw _
	     | Return _ -> ()
	     | _ -> add i (i+1)) m.bir_code;
      List.iter
	(fun (i,e) -> add i e.e_handler) (bir_exception_edges m);
      let succs = Array.map Ptset.elements succs in
      let succs i =
	if i=(-1) then [0] else succs.(i) in
	succs

  (* Compute the rights indexes for each variable use and def.
     See:  
     Cytron, Ron; Ferrante, Jeanne; Rosen, Barry K.; Wegman, Mark N.; 
     and Zadeck, F. Kenneth (1991). 
     "Efficiently computing static single assignment form and the 
     control dependence graph". 
     ACM Transactions on Programming Languages and Systems 13 (4): 451–490.*)
  let rename m var_defs children preds succs phi_nodes =
    let c = ref (Ptmap.map (fun _ -> 0) var_defs) in
    let s = ref (Ptmap.map (fun _ -> []) var_defs) in
    let rename_use = Array.make (Array.length m.bir_code) Ptmap.empty in
    let rename_def = ref Ptmap.empty in
    let rename_def_phi = ref Ptmap.empty in
    let phi_nodes = 
      Ptmap.mapi 
	(fun n s -> 
	   let n_preds = List.length (preds n) in
	     Ptset.fold 
	       (fun v -> Ptmap.add v (Array.make n_preds (-1))) 
	       s 
	       Ptmap.empty) 
	phi_nodes 
    in
    let phi_nodes i =
      try Ptmap.find i phi_nodes with Not_found -> Ptmap.empty in
    let search_h = ref [] in
    let top_s i x = 
      try
	(match Ptmap.find x !s  with
	   | [] -> 
	       Printf.printf "ERROR top(s(%s)) in %d\n" 
		 (var_name_g (m.bir_vars.(x))) i;
	       debug_code m phi_nodes children var_defs !search_h succs;
	       assert false
	   | i::_ -> i)
      with Not_found -> 
	Printf.printf "ERROR s(%s) not found at node %d\n" 
	  (var_name_g (m.bir_vars.(x))) i;
	debug_code m phi_nodes children var_defs !search_h succs;
	assert false in
    let pop_s x = 
      try
	(match Ptmap.find x !s  with
	   | [] -> assert false
	   | _::q -> s := Ptmap.add x q !s)
      with Not_found -> assert false in
    let rec search x =
      search_h := x :: !search_h;
      (* def : set of variables that are defined in x*)
      let def = if x<0 then
        (* at entry point, the set contains all parameters *)
        (List.fold_right (fun (_,x) -> Ptset.add (index x))
	   m.bir_params (Ptset.singleton heap_index))
      else def_bcvar m.bir_code.(x) in
	Ptmap.iter
	  (fun v _ -> 
	     let xmap = 
	       try Ptmap.find x !rename_def_phi
	       with Not_found -> Ptmap.empty
	     in
	     let i = Ptmap.find v !c in
	       rename_def_phi := 
		 Ptmap.add 
		   x (Ptmap.add v i xmap) (* at point x, v |-> v_i *)
		   !rename_def_phi;
	       s := Ptmap.add v (i::(Ptmap.find v !s)) !s;
	       c := Ptmap.add v (i+1) !c)
	  (phi_nodes x);
	if x>=0 then begin
	  let var_defs = use_bcvars m.bir_code.(x) in
	    rename_use.(x) <-
	      Ptset.fold 
	      (fun v -> Ptmap.add v (top_s x v)) var_defs Ptmap.empty
	end;
	Ptset.iter
	  (fun v ->
	     let xmap = 
	       try Ptmap.find x !rename_def
	       with Not_found -> Ptmap.empty
	     in
	     let i = Ptmap.find v !c in
	       rename_def := 
		 Ptmap.add 
		   x (Ptmap.add v i xmap) 
		   !rename_def;
	       s := Ptmap.add v (i::(Ptmap.find v !s)) !s;
	       c := Ptmap.add v (i+1) !c)
	  def;
	List.iter
	  (fun y -> 
	     let preds = preds y in
	     let index_x = JUtil.find_index x preds in
	     let phi = phi_nodes y in
	       Ptmap.iter (fun v args -> args.(index_x) <- top_s y v) phi)
	  (succs x);
	List.iter search (children x);
	Ptset.iter pop_s def;
	Ptmap.iter
	  (fun v _ -> pop_s v)
	  (phi_nodes x)
    in
      search (-1);
      (fun i -> 
	 let xmap = Ptmap.find i !rename_def in
	   fun v -> Ptmap.find v xmap), 
      (fun i -> 
	 let xmap = Ptmap.find i !rename_def_phi in
	   fun v -> Ptmap.find v xmap),
      (fun i -> (rename_use.(i))),
      phi_nodes

  let immediate_dominators (ir_code: bir) : int -> int =
    let preds = preds ir_code in
    let dom = dominator ir_code.bir_code preds in
    let (idom,_children) = idom dom in
      idom

 (* We follow 'A Fast Algorithm for Finding Dominators in a Flowgraph'
   by Lengauer and Tarjan *)
  (* [r] is an entry point in [0 .. n-1] *)
  (* returns (dom,_) where, dom(w) = v is the immediate dominator of w. *)
  let tarjan_idom succ pred r n =
    let semi = Array.make n (-1) in
    let vertex = Array.make n (-1) in
    let label = Array.make n (-1) in
    let dom = Array.make n (-1) in
    let ancestor = Array.make n (-1) in
    let parent = Array.make n (-1) in
    let bucket = Array.make n [] in
    let nb_dfs = ref 0 in
    let nb_dead = ref 0 in
    let rec dfs v =
      semi.(v) <- !nb_dfs;
      vertex.(!nb_dfs) <- v;
      label.(v) <- v;
      incr nb_dfs;
      List.iter
	(fun w -> 
	 if semi.(w) = -1 then 
	   begin
	     parent.(w) <- v;
	     dfs w
	   end)
	(succ v) in
    let rec compress v =
      if not (ancestor.(ancestor.(v)) = -1) then
	begin
	  compress ancestor.(v);
	  if semi.(label.(ancestor.(v))) < semi.(label.(v)) then
	    label.(v) <- label.(ancestor.(v));
	  ancestor.(v) <- ancestor.(ancestor.(v))
	end in 
    let eval v =
      if ancestor.(v) = -1 then v
      else begin
	  compress v;
	  label.(v)
	end in 
    let link v w = ancestor.(w) <- v in
    dfs r;
    Array.iteri
      (fun i w ->
	 if (w = -1) then begin
	     (* Printf.printf "Error: point %d seems not reachable !!!\n" (i+1); *)
	     incr nb_dead
	   end)
      semi;
    let add v t w =
      t.(w) <- v :: t.(w) in
    for i= !nb_dfs-1 downto 1 do 
      let w = vertex.(i) in
	List.iter 
	  (fun v -> 
	   if semi.(v) >=0 then
	     let u = eval v in 
	     if semi.(u) < semi.(w) then semi.(w) <- semi.(u))
	  (pred w);
	add w bucket vertex.(semi.(w));
	link parent.(w) w;
	List.iter 
	  (fun v ->
	   let u = eval v in
	   dom.(v) <- if semi.(u) < semi.(v) then u else parent.(w))
	  bucket.(parent.(w));
	bucket.(parent.(w)) <- []
    done;
    for i=1 to !nb_dfs-1 do
      let w = vertex.(i) in
      if w>=0 && not (dom.(w) = vertex.(semi.(w))) then
	dom.(w) <- dom.(dom.(w))
    done;
    let children = Array.make n [] in
    for i=0 to n-1 do
      let d = dom.(i) in
      if d>=0 then children.(d) <- i :: children.(d)
    done; 
    let idom i =
      try dom.(i)
      with Invalid_argument _ -> assert false in
    let children i =
      if i=(-1) then [0]
      else try children.(i)
	   with Invalid_argument _ ->
	     Printf.printf "children[%d]?\n" i; assert false in
    (idom, children) (* (fun i -> semi.(i)<0), vertex) *)

 let run ir_code live =
    (*
      let rd = ReachDef.run ir_code in
      let jump_target = jump_target ir_code in
    *)    
    let n = Array.length ir_code.bir_code in
    let preds = preds ir_code in
    let succs = succs ir_code in
    let (idom,children) = tarjan_idom succs preds 0 n  in 

(*    let dom = dominator ir_code.bir_code preds in
    let (idom,children) = idom dom in *)
(*    for i=0 to n-1 do
      assert (idom i=idom' i);
      assert (List.sort compare (children i)=List.sort compare (children' i))
    done;   *)
    let domf = domf n preds idom in
    let var_defs = var_defs ir_code in
    let phi_nodes = place_phi_nodes ir_code n var_defs domf live in
    let rename = rename ir_code var_defs children preds succs phi_nodes in
    let phi_nodes i =
      try
	  (Ptset.elements (Ptmap.find i phi_nodes)) 
      with Not_found -> [] in
    (* ((fun i -> dom.(i)), *)
     (idom,domf,phi_nodes,var_defs,rename,preds)

  let to_string s =
    Printf.sprintf "{%s}"
      (JUtil.print_list_sep "," string_of_int (Ptset.elements s))

  let vars_to_string s =
    Printf.sprintf "{%s}"
      (JUtil.print_list_sep "," var_name_g s)

 let debug ir_code (idom,domf,phi_nodes,var_defs,(_rename_def,_rename_def_phi, rename_use,phi_nodes'),_preds) =
    let jump_target = bir_jump_target ir_code in
    let var_defs = 
      JUtil.foldi
        (fun pc _ def_map -> 
           let var_list = phi_nodes pc in
             List.fold_left
               (fun def_m v_i -> 
                  Ptmap.add ~merge:Ptset.union v_i 
                    (Ptset.singleton pc) def_m)
               def_map
               var_list
        )
        var_defs
        ir_code.bir_code
    in
      Ptmap.iter
        (fun v defs ->
           match v with 
             | v when v = heap_index -> () (*not contained in ir_code.bir_vars *)
             | _ -> Printf.printf "  %s:" (var_name_g (ir_code.bir_vars.(v)));
                    Ptset.iter (Printf.printf " %d") defs;
                    print_newline ()) var_defs;
      Array.iteri 
        (fun i op -> 
(*           Printf.printf "     --> DOM[%d]: %s\n" i
             (to_string (dom i)); *)
           Printf.printf "     --> IDOM[%d]: %d\n" i
             (idom i);
          Printf.printf "     --> DOMF[%d]: %s\n" i
             (to_string (domf i)); 
           let phis = List.filter (fun v -> v<>heap_index) (phi_nodes i) in
           let phis = List.map (fun v -> (ir_code.bir_vars.(v))) phis in
             Printf.printf "     --> PHI[%d]: %s\n" i
               (vars_to_string phis);
             (*(try Printf.printf "Def: %d\n" (rename_def i)
              with Not_found -> ());*)
             let rename_use = rename_use i in
               Printf.printf "Use:";
               Ptmap.iter
                 (fun v i -> 
                    match v with
                      | v when v = heap_index -> () (*not contained in ir_code.bir_vars *)
                      | _ -> Printf.printf " %s_[%d]" 
                               (var_name_g ir_code.bir_vars.(v)) i)
                 rename_use;
               print_newline ();		 
               let phi_nodes = phi_nodes' i in
                 Ptmap.iter 
                   (fun v args -> 
                      if v<>heap_index then
                        let v = var_name_g ir_code.bir_vars.(v) in
                          Printf.printf "      %s := PHI(%s)\n"
                            v (JUtil.print_list_sep_id "," (List.map (Printf.sprintf "%s_%d" v) (Array.to_list args))))
                   phi_nodes;
                 Printf.printf "%s%3d: %s\n"
                   (if jump_target.(i) then "x" else " ")
                   i (print_instr op))
	ir_code.bir_code;
      print_newline ()

  let get_heap_index def use phi_node code =
    let in_index = ref Ptmap.empty in
    let out_index = ref Ptmap.empty in
    let phi = ref Ptmap.empty in
    let n = Array.length code in
      for i=0 to n-1 do
	if Ptset.mem heap_index (def_bcvar code.(i))
	then out_index := Ptmap.add i (def i) !out_index;

	if Ptset.mem heap_index (use_bcvars code.(i))
	then in_index := Ptmap.add i (use i) !in_index;

	try
	  match snd (phi_node i) with
	    | None -> ()
	    | Some (x,args) -> phi := Ptmap.add i (x,args) !phi
	with Not_found -> ()
      done;
      !in_index, !out_index, !phi

  let transform_from_ir ir_code =
    let live = live_analysis ir_code in
    let run = run ir_code live in
    let debug i msg = 
      Printf.printf "-----------------\nFailure %s line %d\n-----------------\n" msg i;
      debug ir_code run in
    let (_,_,_,_,
	 (rename_def, rename_def_phi, rename_use,phi_nodes'),preds) = run in
    let dico = make_dictionary_from ir_code.bir_vars in
    let make_var x i = make_var dico (make_var_ssa x i)in
    let def i x = 
      if catch_var x then make_var x 0
      else  
	try make_var x (rename_def i (index x)) 
	with Not_found -> debug i "def lookup"; assert false in
    let use i = 
      let rename_use = try rename_use i with Not_found -> debug i "use lookup"; assert false in
	function x -> 
	  if catch_var x then make_var x 0 else 
	    try make_var x (Ptmap.find (index x) rename_use) 
	    with Not_found -> debug i (Printf.sprintf "use var %s lookup" (var_name_g x)); assert false in
    let phi_nodes i =
      try
	Ptmap.fold
	  (fun v args (l,o) -> 	     
	     if v = heap_index then (l,Some (rename_def_phi i v,args))
	     else
	       let x_ir = ir_code.bir_vars.(v) in
	       let x = make_var x_ir (rename_def_phi i v) in
		 ({
		    def = x;
		    use = Array.map (make_var x_ir) args
		  }::l,o))
	  (phi_nodes' i) ([],None)
      with Not_found -> debug i "phi lookup"; assert false in
    let code = Array.mapi
		 (fun i -> map_instr (def i) (use i)) ir_code.bir_code in
    let (mem_in, mem_out, mem_phi) = get_heap_index 
				       (fun i -> rename_def i heap_index)
				       (fun i -> Ptmap.find heap_index (rename_use i))
				       phi_nodes
				       ir_code.bir_code in
    let exc_t = List.map (map_exception_handler make_var) ir_code.bir_exc_tbl in
    let params = 
      List.map 
	(fun (t,x) -> (t, make_var x 0)) ir_code.bir_params
    in
    let preds = 
      Array.init (Array.length code) (fun i -> Array.of_list (preds i))
    in
    let phi_nodes = 
      Array.init (Array.length code) (fun i -> fst (phi_nodes i))
    in
    let vars = 
      if Array.length ir_code.bir_vars = 0
      then [||]
      else make_array_var dico
    in
      {
	bir_vars = vars;
	bir_params = params;
	bir_code  = code;
	bir_preds = preds;
	bir_phi_nodes = phi_nodes;
	bir_mem_ssa = {
	  mem_ssa_in = (fun i -> 
			  try Ptmap.find i mem_in
			  with Not_found -> raise No_memory_ssa_info_here);
	  mem_ssa_out = (fun i -> 
			  try Ptmap.find i mem_out
			  with Not_found -> raise No_memory_ssa_info_here);
	  mem_ssa_phi = (fun i -> 
			  try Ptmap.find i mem_phi
			  with Not_found -> raise No_memory_ssa_info_here)
	};
	bir_exc_tbl = exc_t;
	bir_line_number_table = ir_code.bir_line_number_table;
	(*bir_pc_bc2ir = ir_code.bir_pc_bc2ir;*)
	bir_pc_ir2bc = ir_code.bir_pc_ir2bc
      }

end

module GetFormula = struct
  type fh = class_method_signature list

  type use_formula = 
    | F_Default
    | F_Perso of fh

  let empty_formula =
    []

  let default_formula = 
    [make_cms (make_cn "sawja.Assertions") (make_ms "assume" [TBasic `Bool] None); 
     make_cms (make_cn "sawja.Assertions") (make_ms "check" [TBasic `Bool] None);
     make_cms (make_cn "sawja.Assertions") (make_ms "invariant" [TBasic `Bool] None); ]

(*
  let is_assertion_cn fh = 
    cn_equal (JBasics.make_cn (fst fh))
 *)
  
  let is_command f_meths c ms = 
    let cms2find = make_cms c ms in
      List.exists
	(fun cmsEl -> cms_equal cmsEl cms2find)
	f_meths

  let is_command_cn f_meths cn = 
    List.exists
      (fun cmsEl -> cn_equal (fst (cms_split cmsEl)) cn)
      f_meths

  let is_command_checklink f_meths = function
    | OpInvoke (`Static cn,ms) -> is_command f_meths cn ms
    | _ -> false
  
  let neg_cond (c,e1,e2) =
    match c with
      | `Eq -> (`Ne,e1,e2)
      | `Ne -> (`Eq,e1,e2)
      | `Lt -> (`Ge,e1,e2)
      | `Ge -> (`Lt,e1,e2)
      | `Gt -> (`Le,e1,e2)
      | `Le -> (`Gt,e1,e2)
  
  exception Not_a_decision_tree
  exception Not_a_boolean_affectation
  
  type cond = [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr 
      
  type decision_tree =
    | LeafTrue
    | LeafFalse
    | Node of 
  	cond (* b *)
  	* decision_tree (* b true *)
  	* decision_tree (* b false *)
  
  let compute_decision_tree code target_var target_pc i =
    let rec tree i  = 
      (* Printf.printf "  tree(%d)...\n" i;*)
      match code.(i) with
        | Ifd (c,j) ->
  	    if j < i then raise Not_a_decision_tree
  	    else Node (neg_cond c,tree (i+1),tree j)
        | Goto j -> 
  	    if j < i then raise Not_a_decision_tree
  	    else tree j
        | AffectVar (x,Const (`Int i1))
  	    when (var_equal x target_var) && 
  	      ((i+1=target_pc) || code.(i+1) = Goto target_pc) ->
  	    if i1 = Int32.one then LeafTrue
  	    else if i1 = Int32.zero then LeafFalse
  	    else raise Not_a_decision_tree
        | _ -> raise Not_a_decision_tree 
    in
      tree i

  let remove_code code i target_pc =
    let rec aux i  = 
      match code.(i) with
        | Ifd (_,j) ->
  	    assert (j >= i);
	    code.(i) <- Nop;
  	    aux (i+1);
	    aux j
        | Goto j -> 
  	    assert (j >= i);
	    code.(i) <- Nop;
  	    aux j
        | AffectVar (_,Const (`Int _)) ->
	    code.(i) <- Nop;
	    if (code.(i+1) = Goto target_pc) then
	      code.(i+1) <- Nop;	
	    aux (i+1);
	| Nop -> ()    
	| _ -> assert false
    in
      aux i
        
  (* Add condition [c0] to each list of the list. *)
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
    let filter_nil l = List.filter (fun d -> d<>[]) l in
    let rec to_cnj = function
      | [] -> assert false
      | [(c,e1,e2)] -> Atom (c,e1,e2)
      | (c,e1,e2)::q -> And (Atom (c,e1,e2),to_cnj q) in
    let rec to_dij = function
      | [] -> raise Not_a_decision_tree
      | [c] -> to_cnj c
      | c::q -> Or (to_cnj c, to_dij q) 
    in
      to_dij (filter_nil (aux t))
        
  let build_boolean_expr code x pc from =
    try
      let tree = compute_decision_tree code x pc from in
      let guard = guard_of_decision_tree tree in
  	remove_code code from pc; 
  	Some guard
    with
      | Not_a_decision_tree -> None

  (* [first_nop code pc] returns the smallest [pc'] such that
     for all index [i] in [pc' ... pc], [code.(i)=Nop].
     We assume that [code.(pc)=Nop]. *)
  let first_nop code pc =
    let i = ref pc in
      while code.(!i) = Nop && !i>0 do decr i done;
	let pc' = !i+1 in
	  assert (code.(pc')=Nop);
	  pc'
      
  let extract_fomula_aux f_meths idom code i =
    match code.(i) with
      | InvokeStatic (None,c,ms,[e]) 
	  when is_command f_meths c ms ->
          let cms_cmd = make_cms c ms in
	    (match e with
	       | Var (_,x) ->
		   let idom = Lazy.force idom in
		   let pc = first_nop code (i-1) in
  		     (match build_boolean_expr code x pc (idom pc) with
  			| Some f -> Some (cms_cmd,f)
  			| None -> Some (cms_cmd,BoolVar e))
	       | _ -> Some (cms_cmd,BoolVar e))
      | MayInit cn
	  when is_command_cn f_meths cn ->
	  code.(i) <- Nop; 
	    None
      | Check (CheckLink jcode_op) 
	  when is_command_checklink f_meths jcode_op ->
	  code.(i) <- Nop;
	    None
      | _ -> None
  
  let run f_meths m =
    (*Before really running the transformation, we check that each methods
      given is valid.*)
    let _chk =
      List.iter
        (fun cms -> 
           let (_cn, ms) = JBasics.cms_split cms in
             match (ms_rtype ms, ms_args ms) with
               | (None, [JBasics.TBasic `Bool;]) -> ()
               | _ -> 
                   Printf.eprintf 
                     "warning: Trying to use an invalid method to build formula: \"%s\" has an invalid signature and will not be used as command for formulae.\n"
                     (ms_name ms)
        )
        f_meths
    in
    let idom = lazy (SSA.immediate_dominators m) in
    let code_new = Array.copy m.bir_code in
      for i=0 to (Array.length m.bir_code) -1 do
        match extract_fomula_aux f_meths idom code_new i with
          | Some (cmd,f) -> code_new.(i) <- (Formula (cmd,f))
          | None -> ()
      done;
      {
        bir_vars = m.bir_vars;
        bir_params = m.bir_params;
        bir_code = code_new;
        bir_exc_tbl = m.bir_exc_tbl;
        bir_line_number_table = m.bir_line_number_table;
	(*        bir_pc_bc2ir = m.bir_pc_bc2ir;*)
        bir_pc_ir2bc = m.bir_pc_ir2bc;
        bir_preds = m.bir_preds; (* not computed yet *)
        bir_phi_nodes = m.bir_phi_nodes; (* not computed yet *)
        bir_mem_ssa = m.bir_mem_ssa; (* not computed yet *)
      }
	
end

let default_formula_cmd = GetFormula.default_formula


open JPrintHtml

let phi_nodes get ioc ms =
  match Javalib.get_method ioc ms with
      ConcreteMethod cm -> 
        (match cm.cm_implementation with
             Java laz -> 
      	 let code = get (Lazy.force laz) in
      	   if Array.length code.bir_phi_nodes <> 0 then
      	     (fun pp -> code.bir_phi_nodes.(pp), code.bir_preds.(pp))
      	   else (fun _ -> [], Array.make 0 0)
           | Native -> fun _ -> [], Array.make 0 0)
    | AbstractMethod _ -> fun _ -> [], Array.make 0 0
        
let inst_html program ioc ms pp op =
  let cs = Javalib.get_name ioc in
    match op with
      | AffectStaticField (ccs,fs,e) ->
          let p1 = field_elem program cs ccs fs in
          let p2 = simple_elem
            (Printf.sprintf ":= %s" (print_expr e)) in
            [p1;p2]
      | AffectField (e1,ccs,fs,e2) ->
          let p1 = field_elem program ~called_cname:(print_expr e1)
            cs ccs fs in
          let p2 = simple_elem
            (Printf.sprintf ":= %s" (print_expr e2)) in
            [p1;p2]
      | New (x,ccs,_,le) ->
          let v = TObject (TClass ccs) in
          let p1 = simple_elem
            (Printf.sprintf "%s := new" (var_name_g x)) in
          let p2 = value_elem program cs v in
          let p3 = simple_elem
            (Printf.sprintf "(%s)"
      	 (JUtil.print_list_sep ", " print_expr le)) in
            [p1;p2;p3]
      | NewArray (x,v,le) ->
          let p1 = simple_elem
            (Printf.sprintf "%s := new" (var_name_g x)) in
          let p2 = value_elem program cs v in
          let p3 = simple_elem
            (Printf.sprintf "%s"
      	 (JUtil.print_list_sep ""
      	    (fun e -> 
      	       Printf.sprintf "[%s]" (print_expr e)) le)
            ) in
            [p1;p2;p3]
      | InvokeStatic (None,ccs,cms,le) ->
          let p1 = 
            invoke_elem 
      	program cs ms pp ccs cms 
          in
          let p2 = simple_elem
            (Printf.sprintf "(%s)" (JUtil.print_list_sep ", " (print_expr) le)) in
            [p1;p2]
      | InvokeStatic (Some x,ccs,cms,le) ->
          let p1 = simple_elem
            (Printf.sprintf "%s :=" (var_name_g x)) in
          let p2 = 
            invoke_elem 
      	program cs ms pp ccs cms 
          in
          let p3 = simple_elem
            (Printf.sprintf "(%s)" (JUtil.print_list_sep ", " (print_expr) le)) in
            [p1;p2;p3]
      | InvokeVirtual (r,e1,k,cms,le) ->
          let p2 =
            (match k with
      	 | VirtualCall o ->
      	     let ccs = match o with
      	       | TClass ccs -> ccs
      	       | _ -> JBasics.java_lang_object in
      	       invoke_elem ~called_cname:(print_expr e1) program
      		 cs ms pp ccs cms
      	 | InterfaceCall ccs ->
      	     invoke_elem ~called_cname:(print_expr e1) program
      	       cs ms pp ccs cms
            ) in
          let p3 = simple_elem
            (Printf.sprintf "(%s)"
      	 (JUtil.print_list_sep ", " (print_expr) le)) in
            (match r with
      	 | None -> [p2;p3]
      	 | Some x ->
      	     let p1 = simple_elem
      	       (Printf.sprintf "%s :="  (var_name_g x)) in
      	       [p1;p2;p3]
            )
      | InvokeNonVirtual (r,e1,ccs,cms,le) ->
          let p1 = simple_elem
            (match r with
      	 | None -> (print_expr e1) ^ "."
      	 | Some x -> Printf.sprintf "%s := %s." (var_name_g x)
      	     (print_expr e1)
            ) in
          let p2 = 
            invoke_elem 
      	program cs ms pp ccs cms 
          in
          let p3 = simple_elem
            (Printf.sprintf "(%s)" (JUtil.print_list_sep ", " print_expr le)) in
            [p1;p2;p3]
      | MayInit ccs ->
          let v = TObject (TClass ccs) in
          let p1 = simple_elem "mayinit" in
          let p2 = value_elem program cs v in
            [p1;p2]
      | Check (CheckCast (e,t)) ->
          let p1 = simple_elem
            (Printf.sprintf "checkcast %s:" (print_expr e)) in
          let p2 = value_elem program cs (TObject t) in
            [p1;p2]
      | _ -> [simple_elem (print_instr op)]

let inst_html get program ioc ms pp op = 
  let res = inst_html program ioc ms pp op in
  let (phi_nodes,preds) = phi_nodes get ioc ms pp in
    if phi_nodes = [] then res
    else
      (simple_elem 
         (Printf.sprintf
            "(preds(%d) := (%s)): "
            pp
            (JUtil.print_list_sep
      	 ", " string_of_int (Array.to_list preds))))
      ::(List.fold_right
           (fun  phi_n elts -> 
      	let el = 
      	  simple_elem (("  "^print_phi_node ~phi_simpl:false phi_n)^";") 
      	in
      	  el::elts
           )
           phi_nodes
           res)

let iter_code f code =
  let instrs = code.bir_code in
    Array.iteri (fun i ins -> f i [ins]) instrs
      
let iter_exc_handler f code =
  List.iter f code.bir_exc_tbl

let method_param_names get ioc ms =
  let m = Javalib.get_method ioc ms in
    match m with
      | AbstractMethod _
      | ConcreteMethod {cm_implementation = Native} -> None
      | ConcreteMethod ({cm_implementation = Java code} as cm) ->
          try
            let code = Lazy.force code in
            let is_static = cm.cm_static in
      	Some
      	  (ExtList.List.mapi
      	     (fun i _ ->
      		let n = if is_static then i else i + 1 in
      		let var = snd (List.nth (get code).bir_params n) in
      		  var_name_g var
      	     ) (ms_args ms)
      	  )
          with _ -> None

let exc_handler_html _program _ioc _ms handler =
  simple_elem 
    (Printf.sprintf 
       "try start: %d; try end: %d: catch start: %d; catched type: %s."
       handler.e_start handler.e_end handler.e_handler 
       (match handler.e_catch_type with
          | None -> "None (finally block)"
          | Some t -> (cn_name t)
       )
    )

(*************** FIELD Resolution END ********************)


let get_method_calls p cs cm =
  let open JProgram in
  let l = ref Ptmap.empty in
  let f_lookup = p.static_lookup_method in
    begin
      match cm with
	| {cm_implementation = Java code}
	    when
	      ClassMethodMap.mem cm.cm_class_method_signature p.parsed_methods ->
	    let ms = cm.cm_signature in
	      Array.iteri
		(fun pp op ->
		   match op with
		     | InvokeStatic _
		     | InvokeVirtual _
                     | InvokeNonVirtual _ ->
			 let lookup = (f_lookup cs ms pp) in
			   l := Ptmap.add pp lookup !l
		     | _ -> ())
		((Lazy.force code).bir_code)
	| _ -> ()
    end;
    !l

let get_callgraph p =
  let open JProgram in
  let methodcalls2callsite cs ms calls =
    let l = ref [] in
      Ptmap.iter
	(fun pp cmset ->
	   ClassMethodSet.iter
	     (fun ccms ->
		let (ccs,cms) = cms_split ccms in
		  l := ((cs,ms,pp),(ccs,cms)) :: !l
	     ) cmset
	) calls;
      !l in
  let calls = ref [] in
    iter
      (fun ioc ->
	 match ioc with
	   | Interface {i_info = {i_name = cs; i_initializer = Some cm}} ->
               calls :=
                 (methodcalls2callsite cs cm.cm_signature
		    (get_method_calls p cs cm)) @ !calls
           | Interface _ -> ()
	   | Class c ->
	       MethodMap.iter
		 (fun _ m ->
		    match m with
		      | ConcreteMethod cm ->
			  let cs = c.c_info.c_name in
			    calls :=
			      (methodcalls2callsite cs cm.cm_signature
				 (get_method_calls p cs cm))
			    @ !calls
		      | AbstractMethod _ -> ()
		 ) c.c_info.c_methods
      ) p;
    !calls

let get_callgraph_from_entries p entries = 
  let open JProgram in
  let methodcalls2callsite cs ms calls =
    let l = ref [] in
      Ptmap.iter
	(fun pp cmset ->
	   ClassMethodSet.iter
	     (fun ccms ->
		let (ccs,cms) = cms_split ccms in
		  l := ((cs,ms,pp),(ccs,cms)) :: !l
	     ) cmset
	) calls;
      !l in
  let calls = ref [] in
  let history = ref (List.fold_left 
                       (fun map el -> ClassMethodSet.add el map) 
                       (ClassMethodSet.empty) entries) 
  in
  let workset = ref entries in
    while ((List.length !workset) > 0 ) do
      (
        let cur_cms = List.hd !workset in
          workset :=List.tl !workset;
          let (cur_cn, cur_ms) = cms_split cur_cms in
          let m = get_method (get_node p cur_cn) cur_ms in
            match m with
              | ConcreteMethod cm ->
                  let mcalls = (get_method_calls p cur_cn cm) in
                    Ptmap.iter
                      (fun _pp cmsSet ->
                         ClassMethodSet.iter 
                           (fun cms ->
                              if (ClassMethodSet.mem cms !history)
                              then ()
                              else 
                                (history := ClassMethodSet.add cms !history;
                                 workset := cms::!workset
                                )
                           )
                           cmsSet
                      )
                      mcalls;
                    calls := ((methodcalls2callsite cur_cn cm.cm_signature mcalls)
                              @ !calls)
              | AbstractMethod _ -> ()
      )
    done;
    !calls



let bir_code_map (f: instr -> instr) (m: bir) : bir =
  { m with bir_code =
    Array.init (Array.length m.bir_code)
    (fun i -> f m.bir_code.(i))
  }

let bir_resolve_field prog cn fs = 
  let class_node = JProgram.get_node prog cn in
  let res_node = JControlFlow.resolve_field_strong fs class_node in
    JProgram.get_name res_node


let bir_resolve_field_in_expr prog (e:expr) : expr =
  let rec aux e =
  match e with
  | Field (x,cls,fs) -> Field (aux x, bir_resolve_field prog cls fs, fs)
  | StaticField (cls,fs) -> StaticField (bir_resolve_field prog cls fs, fs)
  | Unop (op, e') -> Unop (op, aux e')
  | Binop (op, e1, e2) -> Binop (op, aux e1, aux e2)
  | Const _
  | Var _ -> e
  in
  aux e

let bir_field_resolve_in_code prog (inst:instr) : instr =
  let resolve = bir_resolve_field_in_expr prog in
  let resolve_check = function
  | CheckNullPointer e -> CheckNullPointer (resolve e)
  | CheckArrayBound (a,idx) -> CheckArrayBound (resolve a, resolve idx)
  | CheckArrayStore (a,e) -> CheckArrayStore (resolve a, resolve e)
  | CheckNegativeArraySize e -> CheckNegativeArraySize(resolve e)
  | CheckCast (e,t) -> CheckCast (resolve e, t)
  | CheckArithmetic e -> CheckArithmetic (resolve e)
  | CheckLink op -> CheckLink op
  in
  let rec resolve_formula = function
  | Atom (op,e1,e2) -> Atom (op, resolve e1, resolve e2)
  | BoolVar e -> BoolVar (resolve e)
  | And (f,g) -> And (resolve_formula f, resolve_formula g)
  | Or (f,g) -> Or (resolve_formula f, resolve_formula g)
  in
  match inst with
  | AffectVar (x,e) -> AffectVar (x, resolve e)
  | AffectArray (a,idx,e) -> AffectArray (resolve a, resolve idx, resolve e)
  | AffectField (e,c,fs,e') -> AffectField (resolve e, bir_resolve_field prog c fs, fs, resolve e')
  | AffectStaticField (c,fs,e) -> AffectStaticField (bir_resolve_field prog c fs, fs, resolve e)
  | Ifd ((op,e1,e2),pc) -> Ifd ((op, resolve e1, resolve e2), pc)
  | Throw e -> Throw (resolve e)
  | Return (Some e) -> Return (Some (resolve e))
  | New (x,c,tl,args) -> New (x, c, tl, List.map resolve args)
  | NewArray (x,t,el) -> NewArray (x, t, List.map resolve el)
  | InvokeStatic (x,c,ms,args) -> InvokeStatic (x, c, ms, List.map  resolve args)
  | InvokeVirtual (x,e,k,ms,args) -> InvokeVirtual (x, resolve e, k, ms, List.map resolve args)
  | InvokeNonVirtual (x,e,c,ms,args) -> InvokeNonVirtual (x, resolve e, c, ms, List.map resolve args)
  | MonitorEnter e -> MonitorEnter (resolve e)
  | MonitorExit e -> MonitorExit (resolve e)
  | Check e -> Check (resolve_check e)
  | Formula (cms, f) -> Formula (cms, resolve_formula f)
  | Nop
  | Goto _
  | Return None
  | MayInit _
  -> inst

let resolve_all_fields (prog: bir JProgram.program) : bir JProgram.program =
  JProgram.map_program
  (fun _ _ -> bir_code_map (bir_field_resolve_in_code prog))
  None prog


(*************** FIELD Resolution ********************)

module PrintIR =
struct
  type p_instr = instr
  type p_code = bir
  type p_handler = exception_handler

  let iter_code = iter_code	
  let iter_exc_handler = iter_exc_handler
  let method_param_names = method_param_names (fun x -> x)
  let inst_html = inst_html (fun x -> x)
  let exc_handler_html = exc_handler_html
end

module Printer = JPrintHtml.Make(PrintIR)


(*  JPrintPlugin utilities *)
open JPrintPlugin.NewCodePrinter

module MakeCodeExcFunctions =
struct

  open AdaptedASTGrammar

  let to_plugin_warning' get_code get_exc jm pp_warn_map ast_of_i ast_of_e =
    let handlers_pc_map code = 
      if (Ptmap.is_empty pp_warn_map)
      then Ptmap.empty
      else
	List.fold_left
	  (fun map eh -> 
	     Ptmap.add eh.e_handler eh.e_catch_type map
	  )
	  Ptmap.empty
	  (get_exc code)
    in
    let get_precise_warn_generation handlers_pc_map pc op lw msg =
      let try_catch_or_finally = 
	try 
	  Some(
	    match Ptmap.find pc handlers_pc_map with
		None -> PreciseLineWarning (msg,Statement Finally)
	      | Some cn -> PreciseLineWarning (msg, Statement (Catch cn)))
	with Not_found ->
	  None
      in
	match try_catch_or_finally with
	    Some pw -> pw
	  | None -> 
	      (match ast_of_i op with
		   Some node -> PreciseLineWarning (msg,node)
		 | None -> lw)
    in
      match jm with
	  AbstractMethod _ -> pp_warn_map
	| ConcreteMethod cm -> 
	    begin
	      match cm.cm_implementation with
		  Native -> pp_warn_map
		| Java laz -> let cod = Lazy.force laz in
		  let handlers_map = handlers_pc_map cod in
		    Ptmap.mapi
		      (fun pc ppwlist -> 
			 let op = (get_code cod).(pc) in
			   List.map 
			     (fun ppw -> 
				match ppw with
				    PreciseLineWarning _ -> ppw
				  | LineWarning (msg,None) as lw -> 
				      get_precise_warn_generation handlers_map pc op lw msg
				  | LineWarning (msg,Some expr) as lw -> 
				      (match ast_of_e expr with
					   Some node -> PreciseLineWarning (msg,node)
					 | None -> 
					     get_precise_warn_generation handlers_map pc op lw msg)
			     )
			     ppwlist)
		      pp_warn_map
	    end


  let inst_disp' printf pp cod = 
    let printf_esc = 
      (fun i -> JPrintUtil.replace_forb_xml_ch ~repl_amp:true (printf i))
    in
      printf_esc (cod.bir_code).(pp)

  let get_source_line_number pp code =
    bir_get_source_line_number pp code


end

(* we perform some simple optimizations to remove some null pointer checks *)
let simplify_CheckNullPointer (cm:'a concrete_method) (b:bir) : unit =
  if not cm.cm_static then
    begin
      let this = b.bir_vars.(0) in
      let code = b.bir_code in
      if (ExtArray.Array.for_all (* we check that no instruction assigns the [this] variable *)
		(fun ins ->
		 match ins with
		 | AffectVar (x,_)
		 | NewArray (x,_,_)
		 | New (x,_,_,_) 
		 | InvokeStatic (Some x,_,_,_)
		 | InvokeVirtual (Some x,_,_,_,_) 
		 | InvokeNonVirtual (Some x,_,_,_,_) -> not (var_equal this x)
		 | _ -> true) code)
      then Array.iteri
	(fun i ins ->
	 match ins with
	 | Check (CheckNullPointer (Var (_,x))) when var_equal this x ->
	    code.(i) <- Nop
	 | _ -> ())
	code
    end
  

module IRUtil =
struct
  
  let iter_code f lazy_code =
    try
      let instrs =  (Lazy.force lazy_code).bir_code in
	Array.iteri (fun i ins -> f i [ins]) instrs
    with _ -> 
      print_endline "Lazy.force fail";
      ()

end


(* Common functions for JBir-like representation (JBir and JBirSSA)*)
module MakeBirLikeFunctions (* (S : JBir.Internal.CodeInstrSig) = *) =
struct

  include IRUtil

  include MakeCodeExcFunctions

  type p_code = bir
  type p_instr = instr
  type p_expr = expr

  let method_param_names = method_param_names (fun x -> x)

  let find_ast_node_of_expr =
    function 
      | Const _ -> None
      | Binop (ArrayLoad vt,_,_) -> 
	  Some (AdaptedASTGrammar.Expression (AdaptedASTGrammar.ArrayAccess (Some vt)))
      | Binop (_,_,_) -> None
      | Unop (InstanceOf ot,_) -> 
	  Some (AdaptedASTGrammar.Expression(AdaptedASTGrammar.InstanceOf ot))
      | Unop (Cast ot,_) -> 
	  Some (AdaptedASTGrammar.Expression(AdaptedASTGrammar.Cast ot))
      | Unop (_,_) -> None
      | Var (vt,var) -> 
	  Some (AdaptedASTGrammar.Name (AdaptedASTGrammar.SimpleName (var_name_g var,Some vt)))
      | Field (_,_,fs) 
      | StaticField (_,fs) -> 
	  Some (AdaptedASTGrammar.Name
		  (AdaptedASTGrammar.SimpleName (fs_name fs,Some (fs_type fs))))

  let find_ast_node =
    function
      | Goto _ 
      | MayInit _ 
      | Nop -> None
      | AffectField (_,_,fs,_)
      | AffectStaticField (_,fs,_) -> 
	  Some ( AdaptedASTGrammar.Expression
		   (AdaptedASTGrammar.Assignment 
		      (AdaptedASTGrammar.SimpleName (fs_name fs,Some (fs_type fs)))))
      | Ifd ((_cmp,_e1,_e2), _pc) -> 
	  Some (AdaptedASTGrammar.Statement 
		  AdaptedASTGrammar.If)
      | Return _ -> 
	  Some (AdaptedASTGrammar.Statement AdaptedASTGrammar.Return)
      | Throw _ -> 
	  Some (AdaptedASTGrammar.Statement AdaptedASTGrammar.Throw)
      | AffectVar (v,e) -> 
	  Some (AdaptedASTGrammar.Expression
		  (AdaptedASTGrammar.Assignment 
		     (AdaptedASTGrammar.SimpleName (var_name_g v,Some (type_of_expr e)))))
      | MonitorEnter _e -> 
	  Some (AdaptedASTGrammar.Statement
		  (AdaptedASTGrammar.Synchronized true))
      | MonitorExit _e -> 
	  Some (AdaptedASTGrammar.Statement
		  (AdaptedASTGrammar.Synchronized false))
      | NewArray (_v,vt,_le) -> 
	  Some (AdaptedASTGrammar.Expression
		  (AdaptedASTGrammar.ArrayCreation vt))
      | New (_v,cn,_vtl,_le) -> 
	  Some (AdaptedASTGrammar.Expression
		  (AdaptedASTGrammar.ClassInstanceCreation cn))
      | AffectArray (_e1,_e2,e3) -> 
	  Some (AdaptedASTGrammar.Expression
		  (AdaptedASTGrammar.ArrayStore (Some (type_of_expr e3))))		  
      | InvokeVirtual (_,_e,vk,ms,_le) ->
	  begin
	    match vk with
		VirtualCall ot -> 
		  Some (AdaptedASTGrammar.Expression
			  (AdaptedASTGrammar.MethodInvocationVirtual (ot,ms)))
	      | InterfaceCall cn -> 
		  Some (AdaptedASTGrammar.Expression
			  (AdaptedASTGrammar.MethodInvocationNonVirtual (cn,ms)))
	  end
      | InvokeStatic (_,cn,ms,_le) 
      | InvokeNonVirtual (_,_,cn,ms,_le) -> 
	  Some (AdaptedASTGrammar.Expression
		  (AdaptedASTGrammar.MethodInvocationNonVirtual (cn,ms)))
      | Check _ -> None
      | Formula _ -> None

  let inst_disp = 
    inst_disp' print_instr
      
  let to_plugin_warning jm pp_warn_map = 
    to_plugin_warning' (fun c -> c.bir_code) (fun c -> c.bir_exc_tbl)
      jm pp_warn_map find_ast_node find_ast_node_of_expr


end

module PluginPrinter = JPrintPlugin.NewCodePrinter.Make(MakeBirLikeFunctions)

