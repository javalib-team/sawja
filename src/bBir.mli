(** Stackless intermediate representation for Java Bytecode
*)

(** {2 Language} *)

(** {3 Expressions} *)

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

type var 

(** Catched exception are store in [catch_var] *)
val catch_var : var
(** [var_orig v] is [true] if and only if the variable [v] was already used at bytecode level *)
val var_orig : var -> bool

(** [var_equal v1 v2] tests the equality of variables [v1] and [v2] *)
val var_equal : var -> var -> bool

(** [var_name v] returns a string representation of the variable [v]. If the initial class was compiled using debug information, original variable names are build on this information *)
val var_name : var -> string 

(** [bcvar i] returns the canonic var name associated with the [i]th local var. *)
val bcvar : int -> var

type conv = I2L  | I2F  | I2D  
  | L2I  | L2F  | L2D  
  | F2I  | F2L  | F2D
  | D2I  | D2L  | D2F
  | I2B  | I2C  | I2S

type unop =
    Neg of JBasics.jvm_basic_type
  | Conv of conv 
  | ArrayLength
  | InstanceOf of JBasics.object_type

type comp = DG | DL | FG | FL | L 
    
type binop =
    ArrayLoad
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl  | IShr  | IAnd  | IOr  | IXor  | IUshr
  | LShl  | LShr  | LAnd  | LOr  | LXor  | LUshr
  | CMP of comp

type expr =
    Const of const
  | Var of JBasics.value_type * var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * JBasics.class_name * JBasics.field_signature
  | StaticField of JBasics.class_name * JBasics.field_signature

val type_of_expr : expr -> JBasics.value_type

(** {3 Instructions} *)

type virtual_call_kind =
  | VirtualCall of JBasics.object_type
  | InterfaceCall of JBasics.class_name

type check = 
  | CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr
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
  params : var list;  (** method parameters *)
  code : block list
}


(** {2 Printing functions} *)

val print_block : ?explicit_exception:bool -> block -> string
val print : ?explicit_exception:bool -> t -> string list

(** {2 Bytecode transformation} *)

(** JCode transformation, compressed or not *)
val transform : ?compress:bool -> JCode.jcode Lazy.t Javalib.concrete_method -> JCode.jcode Lazy.t -> t 

(** {2 Exceptions} *)


(** - Exceptions raised because of the restrictions on the bytecode needed by the transformation: *)

exception Uninit_is_not_expr
exception NonemptyStack_backward_jump
exception Type_constraint_on_Uninit
exception Content_constraint_on_Uninit
exception Subroutine


(** - Exceptions raised because of a non BCV-ok bytecode: *)

exception Bad_stack
exception Bad_Multiarray_dimension




