(*
 * This file is part of SAWJA
 * Copyright (c)2009, 2010 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)


module Make (Constraints:Constraints.S) :sig

  (** [debug_level] defines the debugging level (verbosity) of the
      solver *)
  val debug_level : int ref

  (** [solve_constraints ~optimize_join prog csts state init] compute the
      fixpoint of the constraints [csts], starting from the initial state
      [state] by applying the constraints that depends on nothing or on initial
      variables [init].  If [optimize_join] is true, then it try do avoid
      joining useless values, but this cost some computations. *)
  val solve_constraints :
    ?optimize_join:bool ->
    'a ->
    Constraints.cst list ->
    Constraints.State.t ->
    Constraints.State.Var.t list -> Constraints.State.t

end = struct
  module State = Constraints.State
  module Var = State.Var

  module HashVar = Hashtbl.Make(Var)

  (* A value of type [cst_to_apply] is uniquely identified by an
     integer in [cst.id].  It contains a constraint [cst] along with
     the index of the target variable [cst.target] of the constraint
     in the field [target], and the index of the dependency variables
     in [dep]. *)
  type cst_to_apply = {
    id:int; (* identifier for the constraint *)
    target: int;
    dep: int list;
    cst:Constraints.cst; (* the constraint *)
  }

  let print_times () =
    let times = Unix.times ()
    in Printf.eprintf "utime: %f\nstime: %f\n%!"
         times.Unix.tms_utime times.Unix.tms_stime

  let debug_level = ref 0
  let print_debug level string =
    if level <= !debug_level
    then prerr_endline string

  (* [do_join () vari] returns true if vari depends on more than one
     constraints, false otherwise. *)
  let do_join var_csts =
    (* let _ = print_times (); Printf.eprintf "computing dep csts of each targets\n" in *)
    let do_join_bs =
      let do_join_bs =
        BitSet.create (Array.length var_csts) in
      let nb_depcsts = Array.make (Array.length var_csts) 0 in
        (* nb_depcsts contains, for each variable index [var], the number of
           constraints on which [var] depends, i.e.m the number of constraints
           that have [var] has target.  If this number is 1, then there is no need
           to join the result of the constraint application with the previous
           value, otherwise it is needed. *)
        Array.iter
          (fun csts ->
             List.iter
               (fun cst ->
                  nb_depcsts.(cst.target) <- (succ nb_depcsts.(cst.target)))
               csts
          )
          var_csts;
        Array.iteri
          (fun vari nb_depcst -> if nb_depcst > 1 then BitSet.set do_join_bs vari)
          nb_depcsts;
        (* let _ = *)
        (*   print_times (); *)
        (*   Printf.eprintf *)
        (*     "computing dep csts of each targets finished (%i out of %i have several deps)\n%!" *)
        (*     (BitSet.count do_join_bs) (Array.length var_csts); *)
        (* in *)
        do_join_bs
    in
      function vari -> BitSet.is_set do_join_bs vari

  (* Algorithm - Main ideas:

     This analysis is based on a work list algorithm.  It also uses a "work set"
     to avoid constraints to be added twice in the work list: before adding a
     constraint in the work list, we first check that it is not already in the
     work list by checking that it is not in the work set.

     The work list is (almost) a FILO implemented efficiently with 2 Caml FIFO
     (stack): one is list used for "add", the other for "get".  When the "get"
     list is empty, the "add" one is reversed and used as the "get" one, while
     an empty list is now used for the add operations.

     The work set is a bit field: it allows to use only a bit per constraints
     along with a constant access time.

  *)

  (* TODO : we should probably use an external fixpoint engine *)
  let work_set ?(optimize_join=false) constraints (var_init:Var.t list) abState : State.t =
    let nb_constraints = List.length constraints in
    let abState = ref abState
    and work_set_size = ref 0
    and work_list = ref [] 		(* list for "add" operations *)
    and work_list_bis = ref []		(* list for "get" operations *)
    and work_set = BitSet.create nb_constraints
    in
    let get_from_work_stack () =
      try
	let cst =
          try List.hd !work_list_bis
          with Failure "hd" ->
            work_list_bis :=
              List.fast_sort
                (fun cst1 cst2 -> cst1.target - cst2.target)
                !work_list;
            work_list := [];
            List.hd !work_list_bis
	in
	  work_list_bis := List.tl !work_list_bis;
	  BitSet.unset work_set cst.id;
	  decr work_set_size;
	  cst
      with Failure "hd" ->
	failwith "get_from_work_stack"
    in
    let add_to_work_stack cst_list =
      List.iter
	(fun cst ->
           if not (BitSet.is_set work_set cst.id)
           then
	     (incr work_set_size;
	      work_list := cst::!work_list;
	      BitSet.set work_set cst.id))
	cst_list
    in
      (* used for statistics *)
    let didnt_modified = ref 0
    and did_modified = ref 0
      (* and time_to_modify = ref 0. *)
      (* and time_to_idle = ref 0. *)
      (* and prev_didnt_modified = ref 0 *)
      (* and prev_did_modified = ref 0 *)
      (* and prev_time = ref (Unix.time ()) *)
    in
    let var_csts =
      (* array which associates to each node (var) the list of constraints that
         depend on it *)
      let var_csts2 = DynArray.make nb_constraints in
      let var_indices = HashVar.create nb_constraints in
	(* var_indices is the "dictionary" of variables: it associate to each
	   variable its index. *)
      let get_var : Var.t -> int =
        (* [get_var var] returns the index of the node/var/target [var] found in
           the dictionary [var_indices].  If [var] is not yet in the dictionary,
           it is given an new fresh index (using [var_current]) and it is added
           in the dictionary. *)
        let var_current = ref 0 in
          function var ->
            try 
              let vari = HashVar.find var_indices var in
                vari
            with Not_found ->
              let new_var = !var_current in
                incr var_current;
                HashVar.add var_indices var new_var;
                let length = DynArray.length var_csts2 in
                  if new_var >= length
                  then
                    DynArray.append
                      (DynArray.init (new_var+1-length) (fun _ -> []))
                      var_csts2;
                  new_var
      in
      let current_cst = ref 0 in
        List.iter
          (fun cst ->
             let target' = get_var (Constraints.get_target cst) in
             let dep = List.map get_var (Constraints.get_dependencies cst) in
             let id_cst = !current_cst in
	     let _ =
               assert (target' = HashVar.find var_indices (Constraints.get_target cst));
               incr current_cst;
	     in
             let cst = {id = id_cst; target = target'; dep= dep; cst = cst} in
	       if dep = [] then
                 add_to_work_stack [cst]
               else
                 List.iter
                   (fun vari ->
                      DynArray.set var_csts2 vari
                        (cst::DynArray.get var_csts2 vari))
                   dep;
               assert
                 (cst.target =
                     HashVar.find var_indices (Constraints.get_target cst.cst))
          )
          constraints;
        List.iter
	  (fun v ->
             let v = get_var v in
               add_to_work_stack (DynArray.get var_csts2 v))
	  var_init;

        if !debug_level > 3 then
          begin
            let print_cst cst =
              print_string ("{id:"^string_of_int cst.id
                            ^ ", target:"^string_of_int cst.target
                            ^ ", dep:"^String.concat "," (List.map string_of_int cst.dep)
                            ^ ", cst:");
              flush stdout;
              Constraints.pprint Format.std_formatter cst.cst;
              Format.pp_print_string  Format.std_formatter "}";
              Format.pp_print_newline Format.std_formatter ()
            in
              DynArray.iteri
                (fun i cstl ->
                   print_int i;
                   print_string " |-> ";
                   List.iter
                     (fun cst -> print_cst cst; print_string "; ")
                     cstl;
                   print_newline ();
                )
                var_csts2;
          end;
        DynArray.to_array var_csts2
    in

    let do_join =
      if optimize_join
      then do_join var_csts
      else (fun _ -> true)
    in

      if !debug_level > 2 then
        (print_debug 3 "start iterating\n";
         print_times ();
         print_debug 3 (string_of_int (nb_constraints) ^" constraints for the analysis\n");
         print_debug 3 (string_of_int (List.length !work_list)
                        ^ " constraints in the work list at the beginning\n"));
      try

	while true do
          let cst = get_from_work_stack () in
          let modifies = ref false in
            (* let start = (Unix.times ()).Unix.tms_utime in *)
            (* if !debug_level > 1 then *)
            (*   Constraints.pprint Format.std_formatter cst.cst; *)
            Constraints.apply_cst ~do_join:(do_join cst.target) ~modifies !abState cst.cst;
            if !debug_level > 3 then
              (let print_cst cst =
                 print_string ("{id:"^string_of_int cst.id
                               ^ ", target:"^string_of_int cst.target
                               ^ ", dep:"^String.concat "," (List.map string_of_int cst.dep)
                               ^ ", cst:");
                 flush stdout;
                 Constraints.pprint Format.std_formatter cst.cst;
                 Format.pp_print_string  Format.std_formatter "}";
                 Format.pp_print_newline Format.std_formatter ()
               in
                 print_string "applying : ";
                 print_cst cst;
                 if !modifies then
                   Format.fprintf Format.std_formatter
                     "state modified. New State:@. %a@."
                     State.pprint !abState);
            if !modifies then (
              (* if !debug_level > 1 then *)
              (*   (Format.pp_print_string Format.std_formatter " modified"; *)
              (*    Format.pp_print_newline Format.std_formatter ()); *)
              incr did_modified;
              (* if !debug_level > 1 then *)
              (*   print_endline ("target:"^string_of_int cst.target); *)
              let csts_to_apply = var_csts.(cst.target)
              in
                assert(
                  List.for_all
                    (fun cst' ->
                       List.exists
                         (fun dep -> 0 = Var.compare dep (Constraints.get_target cst.cst))
                         (Constraints.get_dependencies cst'.cst))
                    csts_to_apply);
	        add_to_work_stack csts_to_apply)
	    else
              ( (* if !debug_level > 1 then *)
                (*   Format.pp_print_string Format.std_formatter " did not modify\n"; *)
                incr didnt_modified;)
	done;
	assert false;
      with Failure "get_from_work_stack" ->
        if !debug_level > 2 then 
          (print_times ();
	   print_debug 3
             (string_of_int !did_modified
              ^ " cst application modified the abstract state and\n"
	      ^string_of_int !didnt_modified
              ^ " cst application did not modified the abstract state.\n"));
        !abState

  let solve_constraints
      ?(optimize_join=false)
      (_:'a)                            (* not used anymore *)
      (constraints:Constraints.cst list)
      (abState:State.t)
      (var_init:Var.t list) : State.t =
    print_debug 3 "start the solver\n";
    if !debug_level > 2 then print_times ();
    work_set ~optimize_join constraints var_init abState
end
