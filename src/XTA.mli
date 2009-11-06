(** Refine the call graph of a program using the XTA class analysis. *)

(** [get_XTA_program field_analysis program entry_points] returns the same
    program where the control flow function has been improved with the result of
    the XTA analysis. Beware than XTA on a large program may take some time,
    specially if you have not compiled the library with the BDD support. *)
val get_XTA_program :
  [< `Class | `Field | `Global ] ->
  JCode.jcode JProgram.program ->
  JBasics.class_method_signature list -> JCode.jcode JProgram.program
