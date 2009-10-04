

type mode  = Normal | Flat | Addr3  

val show_average_stat : Cmn.statistics -> unit
val show_file :
  mode  ->
  bool ->
  bool -> Javalib.class_path -> JBasics.class_name -> Cmn.statistics option
val run_on_class : mode -> string -> unit
val run_on_jar : mode -> string -> unit
val run_on_dir : mode -> string -> unit
