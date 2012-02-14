(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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

type tvar = JBasics.value_type * var

type expr =
  | Const of const 
  | Var of tvar 
  | Unop of unop * tvar
  | Binop of binop * tvar * tvar
  | Field of tvar * JBasics.class_name * JBasics.field_signature  
  | StaticField of JBasics.class_name * JBasics.field_signature  

      
let type_of_tvar (t,_) = t

let type_of_expr = function
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
  | Field (_,_,f) 
  | StaticField (_,f) -> fs_type f
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

type check = 
  | CheckNullPointer of tvar
  | CheckArrayBound of tvar * tvar
  | CheckArrayStore of tvar * tvar
  | CheckNegativeArraySize of tvar
  | CheckCast of tvar * object_type
  | CheckArithmetic of tvar
  | CheckLink of jopcode

type formula =
  | Atom of [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * tvar * tvar
  | And of formula * formula
  | Or of formula * formula

type instr =
  | Nop
  | AffectVar of var * expr
  | AffectArray of tvar * tvar * tvar
  | AffectField of tvar * class_name * field_signature * tvar
  | AffectStaticField of class_name * field_signature * tvar
  | Goto of int
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * tvar * tvar ) * int
  | Throw of tvar
  | Return of tvar option
  | New of var * class_name * value_type list * (tvar list)
      (* var :=  class (parameters) *)
  | NewArray of var * value_type * (tvar list)
      (* var :=  value_type[e1]...[e2] *) 
  | InvokeStatic of var option * class_name * method_signature * tvar list
  | InvokeVirtual of var option * tvar * virtual_call_kind * method_signature * tvar list
  | InvokeNonVirtual
      of var option * tvar * class_name * method_signature * tvar list
  | MonitorEnter of tvar
  | MonitorExit of tvar 
  | MayInit of class_name
  | Check of check 
  | Formula of command_formula * formula
      
type t = {
  bir : Bir.bir;
  (* code is a copy of bir.code modulo a cast from jBir.instr to A3Bir.instr *)
  code : instr array;
}

let vars m = m.bir.Bir.bir_vars
let params m = m.bir.Bir.bir_params
let code m = m.code
let exc_tbl m = m.bir.Bir.bir_exc_tbl
let line_number_table m = m.bir.Bir.bir_line_number_table
let pc_bc2ir m = m.bir.Bir.bir_pc_bc2ir
let pc_ir2bc m = m.bir.Bir.bir_pc_ir2bc 

let get_source_line_number pc_ir m = 
  Bir.bir_get_source_line_number pc_ir m.bir

let exception_edges m = Bir.bir_exception_edges m.bir

let print m = Bir.bir_print m.bir

let jump_target m = Bir.bir_jump_target m.bir

(************* PRINT ************)

let rec print_tvar ?(show_type=true) = function 
  | (t,x) -> 
      if show_type then Printf.sprintf "%s:%s" (var_name_g x) (print_typ t) 
      else var_name_g x  

and print_expr ?(show_type=true) first_level = function
  | Const i -> print_const i
  | Var x -> print_tvar ~show_type:show_type x
  | Field (v,c,f) -> Printf.sprintf "%s.%s" (print_tvar ~show_type:show_type v) (JUtil.print_field c f)
  | StaticField (c,f) -> Printf.sprintf "%s.%s" (JPrint.class_name c) (fs_name f)
  | Unop (ArrayLength,e) -> Printf.sprintf "%s.length" (print_tvar ~show_type:show_type e)
  | Unop (Cast ot,e) -> Printf.sprintf "(%s) %s" (Javalib.JPrint.object_type ot) (print_tvar ~show_type:show_type  e)
  | Unop (op,e) -> Printf.sprintf "%s(%s)" (print_unop op) (print_tvar ~show_type:show_type  e)
  | Binop (ArrayLoad t,e1,e2) -> Printf.sprintf "%s[%s]:%s" (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type e2) (print_typ t)
  | Binop (Add _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s+%s" (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type  e2))
  | Binop (Sub _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s-%s" (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type e2))
  | Binop (Mult _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s*%s" (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type e2))
  | Binop (Div _,e1,e2) -> JUtil.bracket first_level
      (Printf.sprintf "%s/%s" (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type  e2))
  | Binop (op,e1,e2) -> Printf.sprintf "%s(%s,%s)" (print_binop op) (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type e2) 

let print_cmp ?(show_type=true) (c,e1,e2) =
  match c with
    | `Eq -> Printf.sprintf "%s == %s" (print_tvar ~show_type:show_type e1) (print_tvar ~show_type:show_type  e2)
    | `Ne -> Printf.sprintf "%s != %s" (print_tvar ~show_type:show_type e1) (print_tvar ~show_type:show_type e2)
    | `Lt -> Printf.sprintf "%s < %s" (print_tvar ~show_type:show_type e1) (print_tvar ~show_type:show_type  e2)
    | `Ge -> Printf.sprintf "%s >= %s" (print_tvar ~show_type:show_type e1) (print_tvar ~show_type:show_type e2)
    | `Gt -> Printf.sprintf "%s > %s" (print_tvar ~show_type:show_type e1) (print_tvar ~show_type:show_type  e2)
    | `Le -> Printf.sprintf "%s <= %s" (print_tvar ~show_type:show_type  e1) (print_tvar ~show_type:show_type  e2)

let rec print_formula ?(show_type=true) = function
  | Atom (cmp,e1,e2) -> print_cmp ~show_type:show_type (cmp,e1,e2)
  | And (f1,f2) -> Printf.sprintf "(%s) && (%s)" 
      (print_formula ~show_type:show_type f1) (print_formula ~show_type:show_type f2)
  | Or (f1,f2) -> Printf.sprintf "(%s) || (%s)" 
      (print_formula ~show_type:show_type f1) (print_formula ~show_type:show_type f2)

let print_instr ?(show_type=true) = function
  | Nop -> "nop"
  | AffectVar (x,e) -> 
      Printf.sprintf "%s := %s" (var_name_g x) (print_expr ~show_type:show_type true e)
  | AffectStaticField (c,f,e) -> Printf.sprintf "%s.%s := %s" (JPrint.class_name c) (fs_name f) (print_tvar ~show_type:show_type e)
  | AffectField (v,c,f,e2) ->  Printf.sprintf "%s.%s := %s" (print_tvar ~show_type:show_type v) (JUtil.print_field c f) (print_tvar ~show_type:show_type e2)
  | AffectArray (v,e2,e3) -> Printf.sprintf "%s[%s] := %s"  (print_tvar ~show_type:show_type v) (print_tvar ~show_type:show_type  e2) (print_tvar ~show_type:show_type e3)
  | Goto i -> Printf.sprintf "goto %d" i
  | Ifd (g, el) -> Printf.sprintf "if (%s) goto %d" (print_cmp ~show_type:show_type g) el
  | Throw e -> Printf.sprintf "throw %s" (print_tvar ~show_type:show_type  e)
  | Return None -> Printf.sprintf "return"
  | Return (Some e) -> Printf.sprintf "return %s" (print_tvar ~show_type:show_type e)
  | New (x,c,_,le) -> Printf.sprintf "%s := new %s(%s)" (var_name_g x) (JPrint.class_name c) (JUtil.print_list_sep "," (print_tvar ~show_type:show_type) le) 
  | NewArray (x,c,le) -> Printf.sprintf "%s := new %s%s" (var_name_g x) (JPrint.value_type c) (JUtil.print_list_sep "" (fun e -> Printf.sprintf "[%s]" (print_tvar ~show_type:show_type  e)) le) 
  | InvokeStatic (None,c,ms,le) -> Printf.sprintf "%s.%s(%s) // static" (JPrint.class_name c) (ms_name ms) (JUtil.print_list_sep "," (print_tvar ~show_type:show_type) le) 
  | InvokeStatic (Some x,c,ms,le) -> Printf.sprintf "%s := %s.%s(%s) // static" (var_name_g x) (JPrint.class_name c) (ms_name ms) (JUtil.print_list_sep "," (print_tvar ~show_type:show_type) le) 
  | InvokeVirtual (r,x,k,ms,le) -> 
      Printf.sprintf "%s%s.%s(%s) // %s"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_tvar ~show_type:show_type x) (ms_name ms) (JUtil.print_list_sep "," (print_tvar ~show_type:show_type) le)
	(match k with
	   | VirtualCall objt -> "virtual "^(JPrint.object_type objt)
	   | InterfaceCall cn -> "interface "^(JPrint.class_name cn)
	)
  | InvokeNonVirtual (r,x,kd,ms,le) -> 
      Printf.sprintf "%s%s.%s.%s(%s)"
	(match r with
	   | None -> ""
	   | Some x -> Printf.sprintf "%s := "  (var_name_g x))
	(print_tvar ~show_type:show_type x) (JPrint.class_name kd) (ms_name ms) (JUtil.print_list_sep "," (print_tvar ~show_type:show_type) le) 
  | MonitorEnter e -> Printf.sprintf "monitorenter(%s)" (print_tvar ~show_type:show_type e)
  | MonitorExit e -> Printf.sprintf "monitorexit(%s)" (print_tvar ~show_type:show_type e)
  | MayInit c -> Printf.sprintf "mayinit %s" (JPrint.class_name c)
  | Check c ->
      begin
	match c with 
	    CheckNullPointer e -> Printf.sprintf "notnull %s" (print_tvar ~show_type:show_type  e)
	  | CheckArrayBound (a,i) -> Printf.sprintf "checkbound %s[%s]"  (print_tvar ~show_type:show_type  a) (print_tvar ~show_type:show_type  i)
	  | CheckArrayStore (a,v) -> Printf.sprintf "checkstore %s[] <- %s"  (print_tvar ~show_type:show_type  a) (print_tvar ~show_type:show_type  v)
	  | CheckNegativeArraySize e -> Printf.sprintf "checknegsize %s" (print_tvar ~show_type:show_type  e)
	  | CheckCast (e,t) -> Printf.sprintf "checkcast %s:%s" (print_tvar ~show_type:show_type  e) (JDumpBasics.object_value_signature t)
	  | CheckArithmetic e -> Printf.sprintf "notzero %s" (print_tvar ~show_type:show_type e)
	  | CheckLink op -> Printf.sprintf "checklink (%s)" (JPrint.jopcode op)
      end
  | Formula (cmd,f) -> Printf.sprintf "%s(%s)" (print_command_formula cmd) 
      (print_formula ~show_type:show_type f)

let print_expr ?(show_type=true) = print_expr ~show_type:show_type true

exception Bad_Multiarray_dimension = Bir.Bad_Multiarray_dimension 
exception Bad_stack = Bir.Bad_stack
exception Subroutine = Bir.Subroutine
exception Content_constraint_on_Uninit = Bir.Content_constraint_on_Uninit
exception Type_constraint_on_Uninit = Bir.Type_constraint_on_Uninit
exception NonemptyStack_backward_jump = Bir.NonemptyStack_backward_jump
exception Uninit_is_not_expr = Bir.Uninit_is_not_expr


exception Exc_expr2tvar
let expr2tvar expr = 
  match expr with 
    | Bir.Var (t,v) -> (t,v)
    | _ -> begin
	Printf.printf "expr2tvar fails on expr %s\n" (Bir.print_expr expr);
	raise Exc_expr2tvar
      end

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

let rec bir2a3bir_expr e = match e with 
  | Bir.Const c -> Const c
  | Bir.Var (t,v) -> Var (t,v)
  | Bir.Unop (unop, expr) -> Unop(unop,expr2tvar expr)
  | Bir.Binop(binop,expr1,expr2) ->  Binop(bir2a3bir_binop binop,expr2tvar expr1,expr2tvar expr2) 
  | Bir.Field(expr,cn,fs) -> Field (expr2tvar expr, cn, fs)
  | Bir.StaticField(cn,fs) -> StaticField(cn,fs)

let kind2kind = function 
  | Bir.VirtualCall objt -> VirtualCall objt 
  | Bir.InterfaceCall cn -> InterfaceCall cn

let check2check = function 
  | Bir.CheckNullPointer e -> CheckNullPointer (expr2tvar e)
  | Bir.CheckArrayBound (e1, e2) -> CheckArrayBound (expr2tvar e1, expr2tvar e2)
  | Bir.CheckArrayStore (e1,e2) -> CheckArrayStore (expr2tvar e1,  expr2tvar e2)
  | Bir.CheckNegativeArraySize e -> CheckNegativeArraySize (expr2tvar e) 
  | Bir.CheckCast (e,t) -> CheckCast (expr2tvar e,t)
  | Bir.CheckArithmetic e -> CheckArithmetic (expr2tvar e)
  | Bir.CheckLink op -> CheckLink op

let rec bir2a3bir_formula = function
  | Bir.Atom (a,e1,e2) -> Atom (a, expr2tvar e1, expr2tvar e2)
  | Bir.And (f1,f2) -> And (bir2a3bir_formula f1, bir2a3bir_formula f2)
  | Bir.Or (f1,f2) -> Or (bir2a3bir_formula f1, bir2a3bir_formula f2)

let bir2a3bir_instr = function
    Bir.Nop -> Nop
  | Bir.AffectVar (v,expr) -> AffectVar (v,bir2a3bir_expr expr)
  | Bir.AffectArray(e1,e2,e3) -> AffectArray(expr2tvar e1, expr2tvar e2, expr2tvar e3)
  | Bir.AffectField(e1,cn,fs,e2) -> AffectField(expr2tvar e1,cn,fs,expr2tvar e2) 
  | Bir.AffectStaticField(cn,fs,e) -> AffectStaticField(cn,fs,expr2tvar e)
  | Bir.Goto i -> Goto i
  | Bir.Ifd ((cmp,e1,e2),i) -> Ifd ((cmp, expr2tvar e1,expr2tvar e2),i)
  | Bir.Throw e -> Throw (expr2tvar e)
  | Bir.Return (Some e) -> Return (Some (expr2tvar e))
  | Bir.Return None -> Return None
  | Bir.New(v,cn,vtl,el) -> New (v,cn,vtl,List.map expr2tvar el)
  | Bir.NewArray(v,vt,el) -> NewArray(v,vt,List.map expr2tvar el)
  | Bir.InvokeStatic(v,cn,ms,el) -> InvokeStatic(v,cn,ms,List.map expr2tvar el) 
  | Bir.InvokeVirtual(optv,expr, kind, ms, el) ->InvokeVirtual(optv, expr2tvar expr, kind2kind kind, ms, List.map expr2tvar el)
  | Bir.InvokeNonVirtual(optv, e, cn, ms, el) -> InvokeNonVirtual(optv,expr2tvar  e, cn, ms, List.map expr2tvar el) 
  | Bir.MonitorEnter e -> MonitorEnter (expr2tvar e)
  | Bir.MonitorExit e ->  MonitorExit (expr2tvar e)
  | Bir.MayInit cn -> MayInit cn
  | Bir.Check c -> Check (check2check c)
  | Bir.Formula (cmd,f) -> Formula (cmd, bir2a3bir_formula f)
      
let bir2a3bir bir = 
  try
    { bir = bir;
      code = Array.map bir2a3bir_instr bir.Bir.bir_code;
    }
  with Exc_expr2tvar ->
    List.iter (Printf.printf "  %s\n") (Bir.bir_print bir);
    assert false

module PrintIR =
struct
  open JPrintHtml

  type p_instr = Bir.instr
  type p_code = t

  let iter_code f m = Bir.iter_code f m.bir
  let method_param_names = Bir.method_param_names (fun x -> x.bir)
  let inst_html = Bir.inst_html (fun x -> x.bir)
end

module Printer = JPrintHtml.Make(PrintIR)

let print_class = Printer.print_class

let print_program = Printer.print_program

      