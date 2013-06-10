open Javalib_pack
open JBasics
open Javalib
open JProgram
open JControlFlow


type t

val get_class : t -> A3BirSSA.t node

val get_meth : t -> A3BirSSA.t concrete_method

val get_pc : t -> int

val get_pp : A3BirSSA.t node -> A3BirSSA.t concrete_method -> int -> t

(** [get_first_pp p cn ms] gets a pointer to the first instruction
    of the method [ms] of the class [cn].

    @raise Not_found if [cn] is not a class of [p], or [ms] is not a
    method of [cn].

    @raise NoCode if the method [ms] has no associated code.*)
val get_first_pp : A3BirSSA.t program -> class_name -> method_signature -> t

(** [get_first_pp_wp n ms] gets a pointer to the first instruction
    of the method [ms] of the node [n].

    @raise NoCode if the method [ms] has no associated code.*)
val get_first_pp_wp : A3BirSSA.t node -> method_signature -> t

(** get internal representation. *)
val get_ir : t -> A3BirSSA.t

val get_opcode : t -> A3BirSSA.instr

(** [goto_absolute pp i]: return the program pointer at instruction [i] in
   the method of program pointer [pp].*)
val goto_absolute : t -> int -> t

(** [goto_relative pp i]: return the program pointer at instruction [i'+i]
    (with i' the current program point of pp)in the method of program pointer
    [pp].*)
val goto_relative : t -> int -> t

val equal : t -> t -> bool

val compare : t ->  t -> int

val hash : t -> int

(** get the instruction at the next program point (do not follow control
  flow). *)
val next_instruction: t ->  t

(**[static_pp_lookup program pp]: Return a list of method entries which can be
  reached by a call from the current program pointer. It returns fully
  implemented methods pp only. The computation is based on the RTA or CRA 
  result (depending on the one used to build the program).*)
val static_pp_lookup : A3BirSSA.t program -> t -> t list

(**[static_lookup program pp]: Return a list of class in which a method called
* from [pp] might be defined. None means that [pp] does not contains an
  instruction which can lead to a method call. *)
val static_lookup : A3BirSSA.t program -> t -> A3BirSSA.t node list option

(** [handlers pp] returns the list of exception handlers which can be raised at
  program point [pp]. This is a surapproximation.*)
val handlers : t -> A3BirSSA.exception_handler list

(** Return the list of possible successors.*)
val normal_successors : t -> t list 

(** Return the list of possible exceptionnal successors.*)
val exceptional_successors : t -> t list

val to_string : t -> string

val pprint : Format.formatter -> t -> unit

