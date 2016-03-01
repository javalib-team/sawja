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

(** Stackless, 3-address like and unstructured intermediate
    representation for Java Bytecode, in which basic expression trees
    are reconstructed and method and constructor calls are folded.*)

(** {2 Language} *)

(** {3 Variables} *)

(** Abstract data type for variables *)
type var

(** [var_equal v1 v2] is equivalent to [v1 = v2], but is faster.  *)
val var_equal : var -> var -> bool

(** [var_orig v] is [true] if and only if the variable [v] was already used at
    bytecode level. *)
val var_orig : var -> bool

(** [var_name v] returns a string representation of the variable [v]. *)
val var_name : var -> string

(** [var_name_debug v] returns, if possible the original variable names of [v], 
    if the initial class was compiled using debug information. *)
val var_name_debug : var -> string option

(** [var_name_g v] returns a string representation of the variable [v]. 
    If the initial class was compiled using debug information, original 
    variable names are build on this information. It is equivalent to
    [var_name_g x = match var_name_debug with Some s -> s | _ -> var_name x] *)
val var_name_g : var -> string

(** [bc_num v] returns the local var number if the variable comes from the initial bytecode program. *)
val bc_num : var -> int option

(** [index v] returns the hash value of the given variable. *)
val index : var -> int


(** This module allows to build efficient sets of [var] values. *)
module VarSet : GenericSet.GenericSetSig with type elt = var

(** This module allows to build maps of elements indexed by [var] values. *)
module VarMap : GenericMap.GenericMapSig with type key = var

(** {3 Expressions} *)

(** Constants *)
type const = JBir.const

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
  | Cast of JBasics.object_type

(** Comparison operators *)
type comp = 
    DG (** double comparison, if a value is NaN, push 1 into operand stack. *)
  | DL (** double comparison, if a value is NaN, push -1 into operand stack. *)
  | FG (** float comparison, if a value is NaN, push 1 into operand stack.*)
  | FL (** float comparison, if a value is NaN, push -1 into operand stack.*)
  | L  (** long comparison. *)


(** Binary operators *)
type binop =
    ArrayLoad of JBasics.value_type
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl  | IShr  | IAnd  | IOr  | IXor  | IUshr
  | LShl  | LShr  | LAnd  | LOr  | LXor  | LUshr
  | CMP of comp


(** variables are given a type information. *)
type tvar = JBasics.value_type * var

(** Side-effect free expressions. Only variables can be assigned such expressions. *)
type expr =
  | Const of const (** constants *)
  | Var of tvar 
  | Unop of unop * tvar
  | Binop of binop * tvar * tvar
  | Field of tvar * JBasics.class_name * JBasics.field_signature  (** Reading fields of arbitrary expressions *)
  | StaticField of JBasics.class_name * JBasics.field_signature  (** Reading static fields *)

(** [type_of_tvar e] returns the type of the expression [e]. *)      
val type_of_tvar : tvar -> JBasics.value_type

(** [type_of_expr e] returns the type of the expression [e]. *)      
val type_of_expr : expr -> JBasics.value_type

(** {3 Formulae} *)

(** Represents a reconstructed boolean expression. In the good cases (the
  expression can be precisely reconstructed), we obtain conjunctions/disjunctions
  of Atoms. Sometimes the expression cannot be precisely constructed (for
  example when the expression contains side effects). In this case, we just get
  a BoolVar. *)
type formula =
  | Atom of [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * tvar * tvar (** Atomic expression. *)
  | BoolVar of tvar 
  | And of formula * formula
  | Or of formula * formula

(** Give a default set of methods used to generate formulae. Those methods are all
    defined in the class 'sawja.Assertions' and are the following:
    - public static void assume (boolean)
    - public static void check (boolean)
    - public static void invariant (boolean)
    *)
val default_formula_cmd : JBasics.class_method_signature list

(** {3 Instructions} *)
  
type virtual_call_kind =
  | VirtualCall of JBasics.object_type
  | InterfaceCall of JBasics.class_name

(** [check] is the type of A3Bir assertions. They are generated by the transformation so that execution errors arise 
    in the same order in the initial bytecode program and its A3Bir version. Next to each of them is the informal semantics they should be given. *)

type check =
  | CheckNullPointer of tvar  (** [CheckNullPointer e] checks that the expression [e] is not a null pointer and raises the Java NullPointerException if this not the case. *)
  | CheckArrayBound of tvar * tvar (** [CheckArrayBound(a,idx)] checks the index [idx] is a valid index for the array denoted by the expression [a] and raises the Java IndexOutOfBoundsException if this is not the case. *)
  | CheckArrayStore of tvar * tvar (** [CheckArrayStore(a,e)] checks [e] can be stored as an element of the array [a] and raises the Java ArrayStoreException if this is not the case. *)
  | CheckNegativeArraySize of tvar (** [CheckNegativeArray e] checks that [e], denoting an array size, is positive or zero and raises the Java NegativeArraySizeException if this is not the case.*)
  | CheckCast of tvar * JBasics.object_type (** [CheckCast(e,t)] checks the object denoted by [e] can be casted to the object type [t] and raises the Java ClassCastException if this is not the case. *)
  | CheckArithmetic of tvar (** [CheckArithmetic e] checks that the divisor [e] is not zero, and raises ArithmeticException if this is not the case. *)
  | CheckLink of JCode.jopcode
      (** [CheckLink op] checks if linkage mechanism, depending on
	  [op] instruction, must be started and if so if it
	  succeeds. These instructions are only generated if the
	  option is activated during transformation (cf. {!transform}).

	  Linkage mechanism and errors that could be thrown
	  are described in chapter 6 of JVM Spec 1.5 for each bytecode
	  instruction (only a few instructions imply linkage
	  operations: checkcast, instanceof, anewarray,
	  multianewarray, new, get_, put_, invoke_). *)

(** A3Bir instructions are register-based and unstructured. Their operands are [tvar] (typed local vars), except variable assigments.
    Next to them is the informal semantics (using a traditional instruction notations) they should be given. 
    
    Exceptions that could be raised by the virtual
    machine are described beside each instruction, except for the
    virtual machine errors, subclasses of
    [java.lang.VirtualMachineError], that could be raised at any time
    (cf. JVM Spec 1.5 §6.3 ).
*)

type instr =
  | Nop
  | AffectVar of var * expr  (** [AffectVar(x,e)] denotes x := e.  *)
  | AffectArray of tvar * tvar * tvar (** [AffectArray(x,i,e)] denotes   x\[i\] := e. *)
  | AffectField of tvar * JBasics.class_name * JBasics.field_signature * tvar  (** [AffectField(x,c,fs,y)] denotes   x.<c:fs> := y. *)
  | AffectStaticField of JBasics.class_name * JBasics.field_signature * tvar   (** [AffectStaticField(c,fs,e)] denotes   <c:fs> := e .*)
  | Goto of int (** [Goto pc] denotes goto pc. (absolute address)  *)
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * tvar * tvar ) * int (** [Ifd((op,x,y),pc)] denotes    if (x op y) goto pc. (absolute address)  *)
  | Throw of tvar (** [Throw x] denotes throw x.  

			    The exception [IllegalMonitorStateException] could be thrown by the virtual machine.*)
  | Return of tvar option (** [Return x] denotes 
				    - return void when [x] is [None] 
				    - return x otherwise 

				    The exception [IllegalMonitorStateException] could be thrown
				    by the virtual machine.
				*)
  | New of var * JBasics.class_name * JBasics.value_type list * (tvar list)  (** [New(x,c,tl,args)] denotes x:= new c<tl>(args),  [tl] gives the type of [args]. *)
  | NewArray of var * JBasics.value_type * (tvar list)  (** [NewArray(x,t,el)] denotes x := new c\[e1\]...\[en\] where ei are the elements of [el] ; they represent the length of the corresponding dimension. Elements of the array are of type [t].  *)
  | InvokeStatic 
      of var option * JBasics.class_name * JBasics.method_signature * tvar list  (** [InvokeStatic(x,c,ms,args)] denotes 
											   - c.m<ms>(args) if [x] is [None] (void returning method)
											   -  x :=  c.m<ms>(args) otherwise 

											   The exception [UnsatisfiedLinkError] could be
											   thrown if the method is native and the code cannot be
											   found.
										       *)
  | InvokeVirtual
      of var option * tvar * virtual_call_kind * JBasics.method_signature * tvar list (** [InvokeVirtual(x,y,k,ms,args)] denotes the [k] call
												      -  y.m<ms>(args) if [x] is [None]  (void returning method)
												      -  x := y.m<ms>(args) otherwise

												      If [k] is a [VirtualCall _] then the virtual machine could throw the following errors in the same order: [AbstractMethodError, UnsatisfiedLinkError].

												      If [k] is a [InterfaceCall _] then the virtual machine could throw the following errors in the same order: [IncompatibleClassChangeError, AbstractMethodError, IllegalAccessError, AbstractMethodError, UnsatisfiedLinkError].
												  *)
  | InvokeNonVirtual
      of var option * tvar * JBasics.class_name * JBasics.method_signature * tvar list  (** [InvokeNonVirtual(x,y,c,ms,args)] denotes the non virtual call
													-  y.C.m<ms>(args) if [x] is [None]  (void returning method)
													-  x := y.C.m<ms>(args) otherwise 

													The exception [UnsatisfiedLinkError] could be thrown 
													if the method is native and the code cannot be found.
												    *)
  | MonitorEnter of tvar (** [MonitorEnter x] locks the object [x]. *)
  | MonitorExit of tvar (** [MonitorExit x] unlocks the object [x]. 
				  
				  The exception
				  [IllegalMonitorStateException] could be
				  thrown by the virtual machine.  *)
  | MayInit of JBasics.class_name (** [MayInit c] initializes the class [c] whenever it is required. 
				      
				      The exception [ExceptionInInitializerError] could be thrown by the virtual machine.*)
  | Check of check (** [Check c] evaluates the assertion [c]. 
		       
		       Exceptions that could be thrown by the virtual
		       machine are described in {!check} type declaration.*)
  | Formula of JBasics.class_method_signature * formula 
    (** [Formula cms f]: [cms] is the method declaring the formula. [f] is
          the formula to be verified. *)


type exception_handler = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : JBasics.class_name option;
  e_catch_var : var
}

(** [t] is the parameter type for A3Bir methods. *)
type t 

(*Create an empty bir representation. Can be used for stubs.*)
val empty : t

(** All variables that appear in the method. [vars.(i)] is the variable of
    index [i]. *)
val vars : t -> var array

(** [params] contains the method parameters (including the receiver this for
     virtual methods). *)
val params : t -> (JBasics.value_type * var) list

(** Array of instructions the immediate successor of [pc] is [pc+1].  Jumps
    are absolute. *)
val code : t -> instr array

(** [exc_tbl] is the exception table of the method code. Jumps are
    absolute. The list is ordered in the same way as in the bytecode 
    (See JVM Spec 7 $2.10). *)
val exc_tbl : t -> exception_handler list

(** [line_number_table] contains debug information. It is a list of pairs
    [(i,j)] where [i] indicates the index into the bytecode array at which the
    code for a new line [j] in the original source file begins.  *)
val line_number_table : t -> (int * int) list option

(** map from bytecode code line to ir code line. It raises Not_found
    if pc is not an original bytecode pc or if the corresponding
    bytecode instruction has no predecessors and has been removed
    because it is unreachable.*)
(*val pc_bc2ir : t -> int Ptmap.t*)

(** map from ir code line to bytecode code line: the last bytecode
    instruction corresponding to the given ir instruction is
    returned (i.e. the last bytecode instruction used for the ir
    instruction generation).*)
val pc_ir2bc : t -> int array 

(** [jump_target m] indicates whether program points are join points or not in [m]. *)
val jump_target : t -> bool array

(** [exception_edges m] returns a list of edges [(i,e);...] where
    [i] is an instruction index in [m] and [e] is a handler whose
    range contains [i]. *)
val exception_edges :  t -> (int * exception_handler) list
  
(** [get_source_line_number pc m] returns the source line number corresponding
    the program point [pp] of the method code [m].  The line number give a rough
    idea and may be wrong.  It uses the field [t.pc_ir2bc] of the code
    representation and the attribute LineNumberTable (cf. JVMS §4.7.8).*)
val get_source_line_number : int -> t -> int option

 
(** {2 Printing functions} *)

(** [print_handler exc] returns a string representation for exception handler
    [exc]. *)
val print_handler : exception_handler -> string

(** [print_tvar e] returns a string representation for basic expression
    [e]. *)
val print_tvar : ?show_type:bool -> tvar -> string

(** [print_expr e] returns a string representation for expression [e]. *)
val print_expr : ?show_type:bool -> expr -> string

(** [print_instr ins] returns a string representation for instruction [ins]. *)
val print_instr : ?show_type:bool -> instr -> string

(** [print c] returns a list of string representations for instruction of [c]
    (one string for each program point of the code [c]). *)
val print : t -> string list

(** [print_program ~css ~js ~info program outputdir] generates html
    files representing the program [p] in the output directory
    [outputdir], given the annotation information [info]
    ([void_info] by default), an optional Cascading Style Sheet
    (CSS) [css] and an optional JavaScript file [js]. If [css] or
    [js] is not provided, a default CSS or JavaScript file is
    generated. @raise Sys_error if the output directory [outputdir]
    does not exist. @raise Invalid_argument if the name
    corresponding to [outputdir] is a file. *)
val print_program :
  ?css:string -> ?js:string -> ?info:JPrintHtml.info -> t JProgram.program -> string -> unit

(** [print_class ~css ~js ~info ioc outputdir] generates html files
    representing the interface or class [ioc] in the output
    directory [outputdir], given the annotation information [info]
    ([void_info] by default), an optional Cascading Style Sheet
    (CSS) [css] and an optional JavaScript file [js]. If [css] or
    [js] is not provided, a default CSS or JavaScript file is
    generated. No links on types and methods are done when
    [print_class] is used, it should only be used when user does not
    have the program representation. @raise Sys_error if the output
    directory [outputdir] does not exist. @raise Invalid_argument if
    the name corresponding to [outputdir] is a file.*)
val print_class :
  ?css:string -> ?js:string -> ?info:JPrintHtml.info -> t Javalib.interface_or_class -> string -> unit

(** Printer for the Sawja Eclipse Plugin (see module JPrintPlugin) *)
module PluginPrinter : JPrintPlugin.NewCodePrinter.PluginPrinter 
  with type code = t 
  and type expr = expr

(** {2 Bytecode transformation} *)


(** [transform ~bcv ~ch_link ~formula ~formula_cmd cm jcode] transforms
    the code [jcode] into its A3bir representation. The transformation 
    is performed in the context of a given concrete method [cm].  

    - [?bcv]: The type checking normally performed by the ByteCode
    Verifier (BCV) is done if and only if [bcv] is [true].

    - [?ch_link]: Check instructions are generated when a linkage
    operation is done if and only if [ch_link] is [true].

    - [?formula]: Enables or disables formulae. Every static call to a method in
    the list [formula_cmd] generates a [Formula _] statement. Its default value
    is [false].

    - [?formula_cmd]: A list of method for which calls are replaced by
    formulae in the JBir representation. Those methods must be static,
    they must return null and only takes a single boolean variable as
    argument. {!default_formula_cmd} methods will be used by default.
    The argument is only relevant when argument [formula] equals true.
    
    [transform] can raise several exceptions. See exceptions below for details. *)
val transform : ?bcv:bool -> ?ch_link:bool -> 
  ?formula:bool -> ?formula_cmd:JBasics.class_method_signature list 
  -> JCode.jcode Javalib.concrete_method -> JCode.jcode -> t 

(** [resolve_all_fields prog -> prog] : return a new program where every fields
  has been resolved to the exact class where they have been defined. If there
  are some ambigous fields (it means that some interfaces fields are present)
  throw an JControlFlow.AmbigousFieldResolution exception.
 *)
val resolve_all_fields : t JProgram.program -> t JProgram.program

(** {2 Exceptions} *)

exception NonemptyStack_backward_jump
  (** [NonemptyStack_backward_jump] is raised when a backward jump on a
      non-empty stack is encountered. This should not happen if you compiled your
      Java source program with the javac compiler *)

exception Subroutine
  (** [Subroutine] is raised in case the bytecode contains a subroutine. *)
