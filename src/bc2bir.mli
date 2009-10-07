

type mode  = Normal | Flat | Addr3  


val show_file :
  mode  ->
  bool ->
  bool -> Javalib.class_path -> JBasics.class_name -> unit

val add_nbvariables : int -> int -> unit
val set_out_nbvariables : string -> unit 
val close_out_nbvariables : unit -> unit
val run_on_class : mode -> string -> unit
val run_on_jar : mode -> string -> unit
val run_on_dir : mode -> string -> unit
