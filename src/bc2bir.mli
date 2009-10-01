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
type var =
  Cmn.var =
    OriginalVar of int * int * string
  | TempVar of int * int option
  | BranchVar of int * int
  | BranchVar2 of int * int
val var_orig : var -> bool
val var_name : var -> string
val var_name_g : var -> string
val var_equal : var -> var -> bool
type conv =
  Cmn.conv =
    I2L
  | I2F
  | I2D
  | L2I
  | L2F
  | L2D
  | F2I
  | F2L
  | F2D
  | D2I
  | D2L
  | D2F
  | I2B
  | I2C
  | I2S
type unop =
  Cmn.unop =
    Neg of JBasics.jvm_basic_type
  | Conv of conv
  | ArrayLength
  | InstanceOf of JBasics.object_type
val print_unop : unop -> string
type comp = Cmn.comp = DG | DL | FG | FL | L
type binop =
  Cmn.binop =
    ArrayLoad
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl
  | IShr
  | IAnd
  | IOr
  | IXor
  | IUshr
  | LShl
  | LShr
  | LAnd
  | LOr
  | LXor
  | LUshr
  | CMP of comp
val print_binop : binop -> string
type statistics =
  Cmn.statistics = {
  mutable nb_jump_with_non_empty_stacks : int;
  mutable nb_back_jump_with_non_empty_stacks : int;
  mutable nb_store_is_var_in_stack : int;
  mutable nb_incr_is_var_in_stack : int;
  mutable nb_putfield_is_field_in_stack : int;
  mutable nb_arraystore_is_array_access_in_stack : int;
  mutable nb_putstatic_is_static_in_stack : int;
  mutable nb_method_call_with_modifiable_in_stack : int;
  mutable nb_store : int;
  mutable nb_incr : int;
  mutable nb_putfield : int;
  mutable nb_arraystore : int;
  mutable nb_putstatic : int;
  mutable nb_method_call : int;
  mutable nb_tempvar : int;
  mutable nb_tempvar_branch : int;
  mutable nb_tempvar_removed : int;
  mutable nb_tempvar_method_effect : int;
  mutable nb_tempvar_putfield : int;
  mutable nb_tempvar_arraystore : int;
  mutable nb_tempvar_side_effect : int;
  mutable average_tempvar : float list;
  mutable average_tempvar_side_effect : float list;
  mutable average_tempvar_after_simplification : float list;
  mutable average_tempvar_branch : float list;
  mutable average_tempvar_method_effect : float list;
  mutable average_tempvar_putfield : float list;
  mutable average_tempvar_arraystore : float list;
  mutable tempvars : var list;
  mutable nb_classes : int;
  mutable nb_methods : int;
  mutable nb_subroutines : int;
}
type expr =
  Bir.expr =
    Const of const
  | Var of var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * JBasics.class_name * JBasics.field_signature
  | StaticField of JBasics.class_name * JBasics.field_signature
type opexpr = Bir.opexpr = Uninit of JBasics.class_name * int | E of expr
type virtual_call_kind =
  Bir.virtual_call_kind =
    VirtualCall of JBasics.object_type
  | InterfaceCall of JBasics.class_name
type check =
  Bir.check =
    CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr
  | CheckArithmetic of expr
type instr =
  Bir.instr =
    Nop
  | AffectVar of var * expr
  | AffectArray of expr * expr * expr
  | AffectField of expr * JBasics.class_name * JBasics.field_signature * expr
  | AffectStaticField of JBasics.class_name * JBasics.field_signature * expr
  | Goto of int
  | Ifd of ([ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr) * int
  | Throw of expr
  | Return of expr option
  | New of var * JBasics.class_name * JBasics.value_type list * expr list
  | NewArray of var * JBasics.value_type * expr list
  | InvokeStatic of var option * JBasics.class_name *
      JBasics.method_signature * expr list
  | InvokeVirtual of var option * expr * virtual_call_kind *
      JBasics.method_signature * expr list
  | InvokeNonVirtual of var option * expr * JBasics.class_name *
      JBasics.method_signature * expr list
  | MonitorEnter of expr
  | MonitorExit of expr
  | MayInit of JBasics.class_name
  | Check of check
type bir = (int * instr list) list
type op_size = Bir.op_size = Op32 | Op64
val print_list_sep_rec : string -> ('a -> string) -> 'a list -> string
val print_list_sep_list_rec :
  string -> ('a -> string) -> 'a list -> string list
val print_list_sep : string -> ('a -> string) -> 'a list -> string
val print_list_sep_list : string -> ('a -> string) -> 'a list -> string list
val last : 'a list -> 'a
val print_field :
  ?long_fields:bool ->
  JBasics.class_name -> JBasics.field_signature -> string
val bracket : bool -> string -> string
val print_expr : bool -> expr -> string
val print_cmp :
  [< `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] * expr * expr -> string
val print_oexpr : opexpr -> string
val print_stackmap : opexpr list -> string
val print_instr : instr -> string
val print_bir_intra : (int * instr list) list -> string list
val print_bir : (int * instr list) list -> string list
exception Bad_stack
val top : 'a list -> 'a
val pop : 'a list -> 'a list
val popn : int -> 'a list -> 'a list
val pop2 : 'a list -> 'a list
val pop3 : 'a list -> 'a list
val param_acc : int -> 'a list -> 'a list -> 'a list
exception Uninit_is_not_expr
val topE : opexpr list -> expr
val getE : opexpr -> expr
val param : int -> opexpr list -> expr list
exception Subroutine
val convert_type :
  [< `Bool
   | `Byte
   | `ByteBool
   | `Char
   | `Double
   | `Float
   | `Int
   | `Int2Bool
   | `Long
   | `Object
   | `Short ] ->
  op_size
val convert_const :
  [< `ANull
   | `Byte of 'a
   | `Class of 'b
   | `Double of 'c
   | `Float of 'd
   | `Int of 'e
   | `Long of 'f
   | `Short of 'g
   | `String of 'h ] ->
  op_size
val convert_field_type : JBasics.value_type -> op_size
val convert_object_type : JBasics.object_type -> op_size
val type_next : JCode.jopcode -> op_size list -> op_size list
exception Bad_Multiarray_dimension
val remove_dim : JBasics.object_type -> int -> JBasics.value_type
val is_in_expr :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  bool -> expr -> bool
val replace_in_expr :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  expr -> expr -> expr
val is_in_stack :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  opexpr list -> bool
val replace_in_stack :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  expr -> opexpr list -> opexpr list
val is_var_in_stack : var -> opexpr list -> bool
val is_static_in_stack :
  JBasics.class_name -> JBasics.field_signature -> opexpr list -> bool
val is_field_in_expr :
  JBasics.class_name -> JBasics.field_signature -> expr -> bool
val is_array_in_expr : expr -> bool
val is_var_in_expr_not_var : var -> expr -> bool
val is_heap_sensible_element_in_expr : expr -> bool
val replace_var_in_expr : var -> var -> expr -> expr
val replace_var_in_stack : var -> var -> opexpr list -> opexpr list
val replace_static_in_stack :
  JBasics.class_name ->
  JBasics.field_signature -> var -> opexpr list -> opexpr list
val test_expr_in_instr : (expr -> bool) -> instr -> bool
val is_var_in_expr_instr_not_var : var -> instr -> bool
val is_var_in_expr_bir_not_var : var -> ('a * instr list) list -> bool
val clean :
  int ->
  int ->
  int ->
  (expr -> bool) ->
  opexpr list -> instr list -> opexpr list * instr list * int * int
val add_tempvars : statistics option -> var -> unit
val bc2bir_instr :
  'a ->
  (int -> int -> string) ->
  int ->
  op_size list ->
  opexpr list ->
  statistics option ->
  JCode.jopcode -> opexpr list * instr list * statistics option
exception End_of_method
val next : JCode.jopcode array -> int -> int
val normal_next : JCode.jcode -> int -> int list
val is_jump_instr : JCode.jopcode -> bool
module MapPc :
  sig
    type key = int
    type 'a t = 'a Bir.MapPc.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val newStackJump : int -> int -> opexpr list
val is_branchvar_in_stack : int list -> opexpr list -> bool
val para_assign : int -> int list -> opexpr list -> instr list
val jump_stack : int -> int -> int -> opexpr list -> opexpr list * int * int
val simplify_assign :
  statistics option ->
  ('a * instr list) list ->
  opexpr list -> ('a * instr list) list * statistics option
exception NonemptyStack_backward_jump
exception Type_constraint_on_Uninit
exception Content_constraint_on_Uninit
val bc2ir :
  'a ->
  (MapPc.key -> int -> string) ->
  bool array ->
  JCode.jcode ->
  statistics option -> (MapPc.key * instr list) list * statistics option
val varname : string
val search_name_localvar : bool -> JCode.jcode -> int -> int -> string
val compute_jump_target : JCode.jcode -> bool array
val transform_intra_stats :
  'a ->
  ?stats:bool ->
  ?stats_opt:statistics option ->
  JCode.jcode Lazy.t Javalib.concrete_method ->
  (MapPc.key * instr list) list Javalib.concrete_method * statistics option
val transform_intra :
  'a ->
  ?stats_opt:statistics option ->
  JCode.jcode Lazy.t Javalib.concrete_method ->
  (MapPc.key * instr list) list Javalib.concrete_method * statistics option
val stats0 : statistics
val reset_stats0 : unit
val cm_transform_stats :
  'a ->
  ?stats:bool ->
  JCode.jcode Lazy.t Javalib.concrete_method ->
  (MapPc.key * instr list) list Javalib.concrete_method
val cm_transform :
  JCode.jcode Lazy.t Javalib.concrete_method ->
  (MapPc.key * instr list) list Javalib.concrete_method
val jmethod_accu :
  'a ->
  bool ->
  JCode.jcode Lazy.t Javalib.jmethod ->
  (MapPc.key * instr list) list Javalib.jmethod JBasics.MethodMap.t *
  statistics option ->
  (MapPc.key * instr list) list Javalib.jmethod JBasics.MethodMap.t *
  statistics option
val iorc_transform_intra_stats :
  'a ->
  bool ->
  stats:statistics option ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  (MapPc.key * instr list) list Javalib.interface_or_class *
  statistics option
val iorc_transform_intra :
  'a ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  (MapPc.key * instr list) list Javalib.interface_or_class *
  statistics option
val iorc_transform_stats :
  'a ->
  ?cstats:bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  (MapPc.key * instr list) list Javalib.interface_or_class
val iorc_transform :
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  (MapPc.key * instr list) list Javalib.interface_or_class
val is_dir : string -> bool
val is_file : string -> bool
val cn_transform_stats :
  'a ->
  ?cstats:bool ->
  string -> (MapPc.key * instr list) list Javalib.interface_or_class
val cn_transform :
  string -> (MapPc.key * instr list) list Javalib.interface_or_class
val incr_stats :
  statistics option ->
  [< `Nb_arraystore
   | `Nb_arraystore_is_array_access_in_stack
   | `Nb_back_jump_with_non_empty_stacks
   | `Nb_classes
   | `Nb_incr
   | `Nb_incr_is_var_in_stack
   | `Nb_jump_with_non_empty_stacks
   | `Nb_method_call
   | `Nb_method_call_with_modifiable_in_stack
   | `Nb_methods
   | `Nb_putfield
   | `Nb_putfield_is_field_in_stack
   | `Nb_putstatic
   | `Nb_putstatic_is_static_in_stack
   | `Nb_store
   | `Nb_store_is_var_in_stack
   | `Nb_subroutines
   | `Nb_tempvar
   | `Nb_tempvar_arraystore
   | `Nb_tempvar_branch
   | `Nb_tempvar_method_effect
   | `Nb_tempvar_putfield
   | `Nb_tempvar_removed
   | `Nb_tempvar_side_effect ] ->
  unit
val show :
  'a ->
  bool ->
  bool ->
  JCode.jcode Lazy.t Javalib.jmethod ->
  statistics option -> statistics option
val show_iorc :
  'a ->
  bool ->
  bool ->
  stats:statistics option ->
  JCode.jcode Lazy.t Javalib.interface_or_class -> statistics option
val average : float list -> float
val show_average_stat : statistics -> unit
val show_file :
  'a ->
  bool ->
  bool -> Javalib.class_path -> JBasics.class_name -> statistics option
val run_on_class : 'a -> string -> unit
val run_on_jar : 'a -> string -> unit
val make_dir_absolute : string -> string
val run_on_dir : 'a -> string -> unit
