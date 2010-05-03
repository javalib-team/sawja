(** Refine the call graph of a program using the XTA class analysis.  Beware
    than XTA on a large program may take some time (around a minute for 10,000
    methods, including the run-time), specially if you have not compiled the
    library with the BDD support. For more information on XTA, you can read the
    article of Tip and Palsberg:
    {{:http://www.cs.ucla.edu/~palsberg/paper/oopsla00.pdf}Scalable
    Propagation-Based Call Graph Construction Algorithms}.  For an overview of
    class analyses in general (including RTA and XTA), you can check at Barbara
    G. Ryder's lecture on
    {{:http://www.cs.rutgers.edu/~ryder/516/sp03/lectures/ClassAnal-4-304.pdf}Class
    Analyses} *)

open Javalib_pack

(** [get_XTA_program field_analysis program entry_points] returns the same
    program where the control flow function has been improved with the result of
    an XTA analysis.  The parameter [field_analysis] specifies which sensitivity
    should be used for abstraction of fields. [`Field] is more precise than
    [`Class] which is more precise than [`Global]. *)
val get_XTA_program :
  [< `Field | `Class | `Global ] ->
  JCode.jcode JProgram.program ->
  JBasics.class_method_signature list -> JCode.jcode JProgram.program
