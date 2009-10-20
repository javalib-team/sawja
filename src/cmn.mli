

(**   *)


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
	
val print_const : const -> string

val varname : string 

type var =
  | OriginalVar of int * string option  (* register number, name (debug if available) *)
  | TempVar of int * int option
  | BranchVar of int * int
  | BranchVar2 of int * int
      
(** [var_orig v] is [true] if and only if the variable [v] comes from the initial bytecode program *)
val var_orig : var -> bool
  
(** [var_name v] returns the string identifying the variable [v] *)
val var_name : var -> string
  
(** [var_name_debug v] returns, if possible the original variable names of [v], if the initial class was compiled using debug information. *)
val var_name_debug : var -> string option

(** [var_name_g v] returns the string identifying the variable [v], according to the local variable table provided in the class file from which it has been created *)
val var_name_g : var -> string

(** [var_equal v1 v2] tests the equality of variables [v1] and [v2] *)
val var_equal : var -> var -> bool

  
type conv = | I2L  | I2F  | I2D  | L2I  | L2F  | L2D  | F2I  | F2L  | F2D | D2I  | D2L  | D2F | I2B  | I2C  | I2S

type unop =
    Neg of JBasics.jvm_basic_type
  | Conv of conv 
  | ArrayLength
  | InstanceOf of JBasics.object_type

val print_unop : unop -> string

type comp =  DG | DL | FG | FL | L 

type typ = Ref | Num 

val print_typ : typ -> string

(* type statistics = { *)
(*     mutable nb_jump_with_non_empty_stacks : int; *)
(*     mutable nb_back_jump_with_non_empty_stacks : int; *)
(*     mutable nb_store_is_var_in_stack : int; *)
(*     mutable nb_incr_is_var_in_stack : int; *)
(*     mutable nb_arraystore_is_array_access_in_stack : int; *)
(*     mutable nb_putfield_is_field_in_stack : int; *)
(*     mutable nb_putstatic_is_static_in_stack : int; *)
(*     mutable nb_method_call_with_modifiable_in_stack : int; *)
(*     mutable nb_store : int; *)
(*     mutable nb_incr : int; *)
(*     mutable nb_putfield : int; *)
(*     mutable nb_arraystore : int; *)
(*     mutable nb_putstatic : int; *)
(*     mutable nb_method_call : int; *)
(*     mutable nb_tempvar : int; *)
(*     mutable nb_tempvar_branch : int; *)
(*     mutable nb_tempvar_removed : int; *)
(*     mutable nb_tempvar_method_effect : int; *)
(*     mutable nb_tempvar_putfield : int; *)
(*     mutable nb_tempvar_arraystore : int; *)
(*     mutable nb_tempvar_side_effect : int; *)
(*     mutable nb_tempvar_flat : int; *)
(*     mutable nb_tempvar_3a : int; *)
(*   } *)

type mode = Normal | Flat | Addr3
