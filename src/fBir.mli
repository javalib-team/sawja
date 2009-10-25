(** Stackless, flat intermediate representation for Java Bytecode
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
    ArrayLoad of JBasics.jvm_array_type
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl  | IShr  | IAnd  | IOr  | IXor  | IUshr
  | LShl  | LShr  | LAnd  | LOr  | LXor  | LUshr
  | CMP of comp

type expr =
  | Const of const
  | Var of JBasics.value_type * var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of var * JBasics.class_name * JBasics.field_signature
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
  | CheckCast of expr * JBasics.object_type
  | CheckArithmetic of expr

type instr =
  | Nop
  | AffectVar of var * expr
  | AffectArray of var * expr * expr
  | AffectField of var * JBasics.class_name * JBasics.field_signature * expr
  | AffectStaticField of JBasics.class_name * JBasics.field_signature * expr
  | Goto of int
  | Ifd of ( [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr ) * int
  | Throw of expr
  | Return of expr option
  | New of var * JBasics.class_name * JBasics.value_type list * (expr list)
  | NewArray of var * JBasics.value_type * (expr list)
      (* value_type is the type of the array content *)
  | InvokeStatic 
      of var option * JBasics.class_name * JBasics.method_signature * expr list
  | InvokeVirtual
      of var option * var * virtual_call_kind * JBasics.method_signature * expr list
  | InvokeNonVirtual
      of var option * var * JBasics.class_name * JBasics.method_signature * expr list
  | MonitorEnter of expr
  | MonitorExit of expr 
  | MayInit of JBasics.class_name
  | Check of check 

type t = {
  f_params : (JBasics.value_type * var) list; 
  f_code : (int * instr list) list; 
  f_exc_tbl : JCode.exception_handler list;
  f_line_number_table : (int * int) list option;
}

(** {2 Printing functions} *)

val print_instr : instr -> string
val print_instrs : (int * instr list) -> string
val print : t -> string list

(** {2 Bytecode transformation} *)

(** JCode transformation, compressed or not *)
val transform : ?compress:bool -> JCode.jcode Javalib.concrete_method -> JCode.jcode -> t 

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




