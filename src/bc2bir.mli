

type mode  = Normal | Flat | Addr3  

(** Compress empty lines *)
val compress_ir_flag : bool ref
(** Basic simplification of consecutive assignments *)
val simplify_assign_flag : bool ref

val add_nbvariables : int -> int -> unit
val set_out_nbvariables : string -> unit 
val close_out_nbvariables : unit -> unit

val run : mode -> ?verbose:bool -> string -> unit

val set_out_statistics : string -> unit
val close_out_statistics : unit -> unit
