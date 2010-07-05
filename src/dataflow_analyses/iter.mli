type workset_strategy =
  | Decr (* program points are chosen in decreasing order *)
  | Incr (* program points are chosen in increasing order *)

(* manager of the workset iterator. 
   ['dom] is the type of abstract elements.
   [tf] is the type of transfer functions. *)
type ('dom,'tf) manager = {
  bot : 'dom; (* bottom element *)
  join : 'dom -> 'dom -> 'dom; (* binary least upper bound *)
  leq :  'dom -> 'dom -> bool; (* partial order test *)
  normalize :  'dom -> 'dom; (* normalize after each join (identity is correct) *)
  eval : 'tf -> 'dom -> 'dom; (* evaluates a transfer function *)
  size : int; (* analysis will be computed for points in [0 .. size-1] *)
  workset_strategy : workset_strategy;
  cstrs : (int * 'tf * int) list;
  init_points : int list; (* entry points in the equation system *)
  init_value : int -> 'dom; (* constant contraints on entry points *)
  verbose : bool;
  dom_to_string : 'dom -> string;
  transfer_to_string : 'tf -> string
}

(* [run manager] computes the least solution of the constraints given in [manager].
   The result is a function of type [int -> 'dom] that attaches an abstract element to
   each point in [0 .. manager.size -1]. *)
val run : ('dom,'tf) manager -> int -> 'dom
