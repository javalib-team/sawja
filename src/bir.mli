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
    OriginalVar of int * string option
  | TempVar of int * int option
  | BranchVar of int * int
  | BranchVar2 of int * int
val var_orig : var -> bool
val var_name : var -> string
val var_name_debug : var -> string option
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
type typ = Cmn.typ = Ref | Num
val print_typ : typ -> string
type mode = Cmn.mode = Normal | Flat | Addr3
type binop =
    ArrayLoad of typ
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
type expr =
    Const of const
  | Var of typ * var
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Field of expr * JBasics.class_name * JBasics.field_signature
  | StaticField of JBasics.class_name * JBasics.field_signature
type opexpr = Uninit of JBasics.class_name * int | E of expr
type virtual_call_kind =
    VirtualCall of JBasics.object_type
  | InterfaceCall of JBasics.class_name
type check =
    CheckNullPointer of expr
  | CheckArrayBound of expr * expr
  | CheckArrayStore of expr * expr
  | CheckNegativeArraySize of expr
  | CheckCast of expr
  | CheckArithmetic of expr
type instr =
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
type t = {
  params : var list;
  code : (int * instr list) list;
  exc_tbl : JCode.exception_handler list;
  line_number_table : (int * int) list option;
}
type op_size = Op32 | Op64
val print_binop : binop -> string
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
val print_instrs : int * instr list -> string
val print_code_intra : (int * instr list) list -> string list
val print_bir_intra : t -> string list
val print_code : (int * instr list) list -> string list
val print : t -> string list
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
val type_of_value_type : JBasics.value_type -> typ
val type_of_expr : expr -> typ
exception Bad_Multiarray_dimension
val remove_dim : JBasics.object_type -> int -> JBasics.value_type
val binop_is_array : binop -> bool
val is_in_expr :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  bool -> expr -> bool
val var_in_expr : (var -> bool) -> expr -> typ option
val replace_in_expr :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  var -> expr -> expr
val is_in_stack :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  opexpr list -> bool
val var_in_stack : (var -> bool) -> opexpr list -> typ option
val replace_in_stack :
  (var -> bool) ->
  (JBasics.class_name -> JBasics.field_signature -> bool) ->
  var -> opexpr list -> opexpr list
val is_var_in_stack : var -> opexpr list -> typ option
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
val temp_in_expr : Ptset.t -> expr -> Ptset.t
val temp_in_opexpr : Ptset.t -> opexpr -> Ptset.t
val temp_in_stack : opexpr list -> Ptset.t
val choose_fresh_in_stack : opexpr list -> int
val make_tempvar : bool -> opexpr list -> int * 'a -> var
val clean :
  int ->
  int ->
  int ->
  (expr -> bool) ->
  opexpr list -> instr list -> opexpr list * instr list * int * int
val stats_nb_tempvar : int ref
val stats_nb_tempvar_branch : int ref
val stats_nb_tempvar_putfield : int ref
val stats_nb_tempvar_method_effect : int ref
val stats_nb_tempvar_arraystore : int ref
val stats_nb_tempvar_removed : int ref
val stats_nb_tempvar_side_effect : int ref
val stats_nb_tempvar_flat : int ref
val stats_nb_tempvar_3a : int ref
val stats_nb_jump_with_non_empty_stacks : int ref
val stats_nb_back_jump_with_non_empty_stacks : int ref
val stats_nb_store_is_var_in_stack : int ref
val stats_nb_incr_is_var_in_stack : int ref
val stats_nb_putfield_is_field_in_stack : int ref
val stats_nb_arraystore_is_array_access_in_stack : int ref
val stats_nb_putstatic_is_static_in_stack : int ref
val stats_nb_method_call_with_modifiable_in_stack : int ref
val stats_nb_store : int ref
val stats_nb_incr : int ref
val stats_nb_putfield : int ref
val stats_nb_arraystore : int ref
val stats_nb_putstatic : int ref
val stats_nb_method_call : int ref
val to_addr3_binop :
  int ->
  mode ->
  binop -> opexpr list -> instr list -> bool -> opexpr list * instr list
val to_addr3_unop :
  int ->
  mode ->
  unop -> opexpr list -> instr list -> bool -> opexpr list * instr list
val type_of_jvm_type :
  [< `Double | `Float | `Int2Bool | `Long | `Object ] -> typ
val type_of_array_type :
  [< `ByteBool | `Char | `Double | `Float | `Int | `Long | `Object | `Short ] ->
  typ
val bc2bir_instr :
  mode ->
  (int -> int -> string option) ->
  int ->
  op_size list ->
  opexpr list -> bool -> JCode.jopcode -> opexpr list * instr list
exception End_of_method
val next : JCode.jopcode array -> int -> int
val normal_next : JCode.jcode -> int -> int list
val is_jump_instr : JCode.jopcode -> bool
module MapPc :
  sig
    type key = int
    type +'a t
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
val is_branchvar_in_stack : int list -> opexpr list -> bool
val para_assign : int -> int list -> opexpr list -> instr list
val jump_stack : int -> int -> int -> opexpr list -> opexpr list * int * int
val simplify_assign_flag : bool ref
val simplify_assign :
  mode -> ('a * instr list) list -> opexpr list -> ('a * instr list) list
val fold_ir : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b * 'c list) list -> 'a
type tempvar_stats = {
  stat_nb_total : int;
  stat_nb_branchvar : int;
  stat_nb_tempvar_may_alias : int;
  stat_nb_tempvar_must_alias : int;
  stat_nb_tempvar_side_effect : int;
}
module SetInt2 :
  sig
    type elt = int * int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val make_tempvar_stats : ('a * instr list) list -> tempvar_stats option
val reset_stats : unit -> unit
exception NonemptyStack_backward_jump
exception Type_constraint_on_Uninit
exception Content_constraint_on_Uninit
val bc2ir :
  mode ->
  (MapPc.key -> int -> string option) ->
  bool array ->
  JCode.jcode -> bool -> (MapPc.key * instr list) list * tempvar_stats option
val varname : string
val search_name_localvar : bool -> JCode.jcode -> int -> int -> string option
val bcvar : int -> var
val compute_jump_target : JCode.jcode -> bool array
val gen_params :
  (int -> int -> string option) -> 'a Javalib.concrete_method -> var list
val compress_ir_flag : bool ref
val compress_ir :
  JCode.jcode -> (int * instr list) list -> (int * instr list) list
val ret_stats : tempvar_stats option ref
val jcode2bir :
  'a Javalib.concrete_method -> bool -> mode -> JCode.jcode Lazy.t -> t
val transform_intra_stats :
  mode ->
  ?stats:bool ->
  JCode.jcode Lazy.t Javalib.concrete_method ->
  t Javalib.concrete_method * tempvar_stats option
val transform_intra :
  mode ->
  JCode.jcode Lazy.t Javalib.concrete_method -> t Javalib.concrete_method
val cm_transform :
  bool ->
  JCode.jcode Lazy.t Javalib.concrete_method -> t Javalib.concrete_method
val cm_transform_flat :
  bool ->
  JCode.jcode Lazy.t Javalib.concrete_method -> t Javalib.concrete_method
val cm_transform_addr3 :
  bool ->
  JCode.jcode Lazy.t Javalib.concrete_method -> t Javalib.concrete_method
val jmethod_accu :
  mode ->
  bool ->
  JCode.jcode Lazy.t Javalib.jmethod ->
  t Javalib.jmethod JBasics.MethodMap.t ->
  t Javalib.jmethod JBasics.MethodMap.t
val iorc_transform_intra_stats :
  mode ->
  bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  t Javalib.interface_or_class
val iorc_transform_intra :
  mode ->
  bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  t Javalib.interface_or_class
val iorc_transform_stats :
  mode ->
  ?cstats:bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  t Javalib.interface_or_class
val iorc_transform :
  bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  t Javalib.interface_or_class
val iorc_transform_flat :
  bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  t Javalib.interface_or_class
val iorc_transform_3addr :
  bool ->
  JCode.jcode Lazy.t Javalib.interface_or_class ->
  t Javalib.interface_or_class
val is_file : string -> bool
val is_dir : string -> bool
val cn_transform_stats :
  mode -> ?cstats:bool -> string -> t Javalib.interface_or_class
val cn_transform_nbvars : string -> t Javalib.interface_or_class
val cn_transform : bool -> string -> t Javalib.interface_or_class
val cn_transform_flat : bool -> string -> t Javalib.interface_or_class
val cn_transform_3addr : bool -> string -> t Javalib.interface_or_class
