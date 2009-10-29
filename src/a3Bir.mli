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

(** Stackless, 3-address like and unstructured intermediate representation for Java Bytecode, in which basic expression trees are reconstructed and method and constructor calls are folded.*)

(** {2 Language} *)

(** {3 Expressions} *)

(** Constants *)
type const =
    [ `ANull
    | `Byte of int
    | `Class of JBasics.object_type
    | `Double of float
    | `Float of float
    | `Int of int32
    | `Long of int64
    | `Short of int
    | `String of string ]

(** Abstract data type for variables *)
type var

(** Catched exception are store in [catch_var]. *)
val catch_var : var

(** [var_orig v] is [true] if and only if the variable [v] was already used at bytecode level. *)
val var_orig : var -> bool

(** [var_equal v1 v2] tests the equality of variables [v1] and [v2]. *)
val var_equal : var -> var -> bool

(** [var_name v] returns a string representation of the variable [v]. If the initial class was compiled using debug information, original variable names are build on this information. *)
val var_name : var -> string

(** [bcvar i] returns the canonic var name associated with the [i]th local var. *)
val bcvar : int -> var

(** Conversion operators *)
type conv = I2L  | I2F  | I2D
  | L2I  | L2F  | L2D
  | F2I  | F2L  | F2D
  | D2I  | D2L  | D2F
  | I2B  | I2C  | I2S

(** Unary operators *)
type unop =
    Neg of JBasics.jvm_basic_type
  | Conv of conv
  | ArrayLength
  | InstanceOf of JBasics.object_type

(** Comparison operators *)
type comp = DG | DL | FG | FL | L


(** Binary operators *)
type binop =
    ArrayLoad of JBasics.jvm_array_type
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl  | IShr  | IAnd  | IOr  | IXor  | IUshr
  | LShl  | LShr  | LAnd  | LOr  | LXor  | LUshr
  | CMP of comp


(** Side-effect free basic expressions *)
type basic_expr = 
  | Const of const (** constants *)
  | Var of JBasics.value_type * var (** variables are given a type information. *)

(** Side-effect free expressions. Only variables and static fields can be assigned such expressions. *)
type expr =
    BasicExpr of basic_expr (** basic expressions *)
  | Unop of unop * basic_expr
  | Binop of binop * basic_expr * basic_expr
  | Field of basic_expr * JBasics.class_name * JBasics.field_signature  (** Reading fields of arbitrary expressions *)
  | StaticField of JBasics.class_name * JBasics.field_signature  (** Reading static fields *)

(** [type_of_expr e] returns the type of the expression [e]. *)      
val type_of_expr : expr -> JBasics.value_type

(** {3 Instructions} *)
	  
type virtual_call_kind =
  | VirtualCall of JBasics.object_type
  | InterfaceCall of JBasics.class_name

(** [check] is the type of A3Bir assertions. They are generated by the transformation so that execution errors arise 
in the same order in the initial bytecode program and its JBir version. Next to each of them is the informal semantics they should be given. *)
type check = 
  | CheckNullPointer of basic_expr  (** [CheckNullPointer e] checks that the expression [e] is not a null pointer. *)
  | CheckArrayBound of basic_expr * basic_expr (** [CheckArrayBound(a,idx)] checks the index [idx] is a valid index for the array denoted by the expression [a]. *)
  | CheckArrayStore of basic_expr * basic_expr (** [CheckArrayStore(a,e)] checks [e] can be stored as an element of the array [a]. *)
  | CheckNegativeArraySize of basic_expr (** [CheckNegativeArray e] checks that [e], denoting an array size, is positive or zero. *)
  | CheckCast of basic_expr * JBasics.object_type (** [CheckCast(e,t)] checks the object denoted by [e] can be casted to the object type [t]. *)
  | CheckArithmetic of basic_expr (** [CheckArithmetic e] checks that the divisor [e] is not zero. *)

(** A3Bir instructions are register-based and unstructured. Their operands are [basic_expressions], except variable and static field assigments.
    Next to them is the informal semantics (using a traditional instruction notations) they should be given. *)
type instr =
  | Nop
  | AffectVar of var * expr  (** [AffectVar(x,e)] denotes x := e.  *)
  | AffectArray of basic_expr * basic_expr * basic_expr (** [AffectArray(x,i,e)] denotes   x\[i\] := e. *)
  | AffectField of basic_expr * JBasics.class_name * JBasics.field_signature * basic_expr  (** [AffectField(x,c,fs,y)] denotes   x.<c:fs> := y. *)
  | AffectStaticField of JBasics.class_name * JBasics.field_signature * expr   (** [AffectStaticField(c,fs,e)] denotes   <c:fs> := e .*)
  | Goto of int (** [Goto pc] denotes goto pc. *)
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * basic_expr * basic_expr ) * int (** [Ifd((op,x,y),pc)] denotes    if (x op y) goto pc. *)
  | Throw of basic_expr (** [Throw x] denotes throw x.  *)
  | Return of basic_expr option (** [Return x] denotes 
- return void when [x] is [None] 
- return x otherwise 
*)
  | New of var * JBasics.class_name * JBasics.value_type list * (basic_expr list)  (** [New(x,c,tl,args)] denotes x:= new c<tl>(args),  [tl] gives the type of [args]. *)
  | NewArray of var * JBasics.value_type * (basic_expr list)   (** [NewArray(x,t,xl)] denotes x := new c\[x1\]...\[xn\] where xi are the elements of type [t] of the list [xl].  *)
  | InvokeStatic 
      of var option * JBasics.class_name * JBasics.method_signature * basic_expr list  (** [InvokeStatic(x,c,ms,args)] denotes 
- c.m<ms>(args) if [x] is [None] (void returning method)
-  x :=  c.m<ms>(args) otherwise 
*)
  | InvokeVirtual
      of var option * basic_expr * virtual_call_kind * JBasics.method_signature * basic_expr list (** [InvokeVirtual(x,y,k,ms,args)] denotes the [k] call
-  y.m<ms>(args) if [x] is [None]  (void returning method)
-  x := y.m<ms>(args) otherwise 
*)
  | InvokeNonVirtual
      of var option * basic_expr * JBasics.class_name * JBasics.method_signature * basic_expr list  (** [InvokeNonVirtual(x,y,c,ms,args)] denotes the non virtual call
-  y.C.m<ms>(args) if [x] is [None]  (void returning method)
-  x := y.C.m<ms>(args) otherwise 
*)
  | MonitorEnter of basic_expr (** [MonitorEnter x] locks the object [x]. *)
  | MonitorExit of basic_expr (** [MonitorExit x] unlocks the object [x]. *)
  | MayInit of JBasics.class_name (** [MayInit c] initializes the class [c] whenever it is required. *)
  | Check of check (** [Check c] evaluates the assertion [c]. *)

(** [t] is the type of JBir codes. *)
type t = {
  a3_params : (JBasics.value_type * var) list; (** [a3_params] contains the method parameters. *)
  a3_code : (int * instr list) list; (** Each element of [a3_code] is a pair [(pc,instrs)] where 
				      each [pc] indexes an [instr] list corresponding to the instructions generated from the  bytecode  at [pc]. *)
  a3_exc_tbl : JCode.exception_handler list; (** [a3_exc_tbl] is the exception table of the method code. *)
  a3_line_number_table : (int * int) list option; (** [a3_line_number_table] contains debug information. It is a list of pairs [(i,j)] meaning the code line [i] corresponds to the line [j] at the java source level. *) 
  a3_jump_target : bool array (** [a3_jump_target] indicates whether program points are join points or not. *) 
}
  
(** {2 Printing functions} *)

(** [print_instr ins] returns a string representation for instruction [ins]. *)
val print_instr : instr -> string

(** [print_instrs (pc,insl)] returns a string representation for instructions [insl] at program point [pc]. *)
val print_instrs : (int * instr list) -> string

(** [print c] returns a list of string representations for instruction of [c] (one string for each program point of the code [c]). *)
val print : t -> string list

(** {2 Bytecode transformation} *)

(** [transform b cm jcode] transforms the code [jcode] into its A3Bir representation. The transformation is performed in the context of a given concrete method [cm]. According to the boolean [~compress:b], all program points made of a single [Nop] instruction are removed from the obtained A3Bir representation. 
[transform b cm jcode] can raise several exceptions. See exceptions below for details. *) 
val transform : ?compress:bool -> JCode.jcode Javalib.concrete_method -> JCode.jcode -> t 

(** {2 Exceptions} *)


(** {3 Exceptions due to the transformation limitations} *)

exception Uninit_is_not_expr (** [Uninit_is_not_expr] is raised in case an initialised reference is used as a traditionnal expression (variable assignment, field reading etc).*)
exception NonemptyStack_backward_jump (** [NonemptyStack_backward_jump] is raised when the requirements about stacks for folding constructors are not satisfied. *)
exception Type_constraint_on_Uninit (** [Type_constraint_on_Uninit] is raised when the requirements about stacks for folding constructors are not satisfied. *)
exception Content_constraint_on_Uninit (** [Content_constraint_on_Uninit] is raised when the requirements about stacks for folding constructors are not satisfied. *)
exception Subroutine (** [Subroutine] is raised in case the bytecode contains a subroutine. *)


(** {3 Exceptions due to a non-Bytecode-verifiable bytecode} *)

exception Bad_stack (** [Bad_stack] is raised in case the stack does not fit the length/content constraint of the bytecode instruction being transformed. *)
exception Bad_Multiarray_dimension (** [Bad_Multiarray_dimension] is raise when attempting to transforming a multi array of dimension zero. *)



