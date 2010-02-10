module Make (Constraints:Constraints.S) :sig

  (** [debug_level] defines the debugging level (verbosity) of the
      solver *)
  val debug_level : int ref

  (** [solve_constraints prog csts state init] compute the fixpoint of
      the constraints [csts], starting from the initial state [state] by
      applying the constraints that depends on nothing or on initial
      variables [init]. *)
  val solve_constraints :
    'a ->
    Constraints.cst list ->
    Constraints.State.t ->
    Constraints.State.Var.t list -> Constraints.State.t

end = struct
  module State = Constraints.State
  module Var = State.Var

  module HashVar = Hashtbl.Make(Var)

  type cst_to_apply = {
    id:int; (* identifier for the constraint *)
    target: int;
    dep: int list;
    cst:Constraints.cst; (* the constraint *)
  }

  exception Found of (cst_to_apply)

  let print_times () =
    let times = Unix.times ()
    in Printf.eprintf "utime: %f\nstime: %f\n%!"
         times.Unix.tms_utime times.Unix.tms_stime

  let debug_level = ref 0
  let print_debug level string =
    if level <= !debug_level
    then prerr_endline string

  (* TODO : we should probably use an external fixpoint engine *)
  let work_set constraints (var_init:Var.t list) abState : State.t =
    let nb_constraints = List.length constraints in
    let abState = ref abState
    and work_set_size = ref 0
    and work_list = ref []
    and work_list_bis = ref []
    and work_set = BitSet.create nb_constraints in
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
      (* array which associates to each node the list of constraints that depends
         on it *)
      let var_csts2 = DynArray.create () in
      let var_indices = HashVar.create nb_constraints in
      let var_current = ref 0 in
      let get_var var =
        (* get the index of the node/var/target [var] *)
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
               assert (target' = HashVar.find var_indices (Constraints.get_target cst));
               incr current_cst;
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

        if !debug_level > 1 then
          begin
            (* print_debug 4 "dictionnary :\n"; *)
            (* HashVar.iter *)
            (*   (fun var index -> *)
            (*      (Var.pprint Format.str_formatter var); *)
            (*      print_debug 4 (string_of_int index ^" <-| "); *)
            (*      print_debug 4 (Format.flush_str_formatter ()); *)
            (*      print_debug 4 "\n") *)
            (*   var_indices; *)
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
            Constraints.apply_cst ~modifies !abState cst.cst;
            if !debug_level > 2 then
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
	  print_times ();
	  print_debug 3
            (string_of_int !did_modified
             ^ " cst application modified the abstract state and\n"
	     ^string_of_int !didnt_modified
             ^ " cst application did not modified the abstract state.\n");
        !abState

  (* TODO : compute the predecessors of each pp and if a pp contains
     only one predecessors then do not join the values *)
  let solve_constraints
      (_:'a)                            (* not used anymore *)
      (constraints:Constraints.cst list)
      (abState:State.t)
      (var_init:Var.t list) : State.t =
    print_debug 3 "start the solver\n";
    (*print_times ();*)
    work_set constraints var_init abState
end
