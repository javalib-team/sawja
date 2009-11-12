module Make (Constraints:Constraints.S) :sig

  (** [debug_level] defines the debugging level (verbosity) of the
      solver *)
  val debug_level : int ref

  (** [solve_constraints prog csts state init] compute the fixpoint of
      the constraints [csts], starting from the initial state [state] by
      applying the constraints that depends on nothing or on initial
      variables [init]. *)
  val solve_constraints :
    'a JProgram.program ->
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
    cst:Constraints.cst; (* the constraint *)
  }

  exception Found of (cst_to_apply)

  let print_times () =()
    (* let times = Unix.times () *)
    (* in Printf.eprintf "utime: %f\nstime: %f\n%!" *)
    (*      times.Unix.tms_utime times.Unix.tms_stime *)

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
      let var_indices = HashVar.create nb_constraints in
      let get_var var =
        try HashVar.find var_indices var
        with Not_found ->
          let new_var = HashVar.length var_indices in
            HashVar.add var_indices var new_var;
            new_var
      in
      let module VarMap = Ptmap in
      let current_cst = ref 0 in
      let map_var_csts =
        List.fold_left
          (fun map_var_csts cst ->
             let target = get_var (Constraints.get_target cst) in
             let dep = Constraints.get_dependencies cst in
             let id_cst = !current_cst in
               incr current_cst;
	       if dep = [] then
                 (add_to_work_stack [{id = id_cst; target = target; cst = cst}];
                  map_var_csts)
	       else
	         List.fold_left
                   (fun map_var_csts v ->
                      let v = get_var v in
                        VarMap.add
                          ~merge:List.rev_append
                          v
                          [{id = id_cst; target=target; cst = cst}]
                          map_var_csts)
                   map_var_csts
                   dep)
          VarMap.empty
          constraints
      in
      let var_csts = Array.create (HashVar.length var_indices) []
      in
        VarMap.iter
          (fun var csts ->
             var_csts.(var) <- csts)
          map_var_csts;
        List.iter
	  (fun v ->
             let v = get_var v in
               add_to_work_stack var_csts.(v))
	  var_init;
        var_csts
    in
      print_debug 3 "start iterating";
      print_times ();
      print_debug 3 (string_of_int (nb_constraints) ^" constraints for the analysis");
      print_debug 3 (string_of_int (List.length !work_list)
                     ^ " constraints in the work list at the beginning");
      try
	while true do
          let cst = get_from_work_stack () in
          let modifies = ref false in
          (* let start = (Unix.times ()).Unix.tms_utime in *)
            Constraints.apply_cst ~modifies !abState cst.cst;
            if !modifies then
              ((* time_to_modify := *)
               (*   !time_to_modify +. ((Unix.times ()).Unix.tms_utime -. start); *)
               incr did_modified;
	       add_to_work_stack (var_csts.(cst.target)))
	    else
              ((* time_to_idle := *)
               (*   !time_to_idle +. ((Unix.times ()).Unix.tms_utime -. start); *)
               incr didnt_modified;)
	done;
	assert false;
      with Failure "get_from_work_stack" ->
        print_times ();
        (* print_debug 3 (Printf.sprintf "analysis spent %f s to work and %f s to idle" !time_to_modify !time_to_idle); *)
	print_debug 3
          (string_of_int !did_modified
           ^ " cst application modified the abstract state and\n"
	   ^string_of_int !didnt_modified
           ^ " cst application did not modified the abstract state.\n");
        !abState

  (* TODO : compute the predecessors of each pp and if a pp contains
     only one predecessors then do not join the values *)
  let solve_constraints
      (_prog:'a JProgram.program)
      (constraints:Constraints.cst list)
      (abState:State.t)
      (var_init:Var.t list) : State.t =
    print_debug 3 "start the solver";
    print_times ();
    work_set constraints var_init abState
end

(* module MakeB (Constraints:Constraints.S) :sig *)

(*   (\** [debug_level] defines the debugging level (verbosity) of the *)
(*       solver *\) *)
(*   val debug_level : int ref *)

(*   (\** [solve_constraints prog csts state init] compute the fixpoint of *)
(*       the constraints [csts], starting from the initial state [state] by *)
(*       applying the constraints that depends on nothing or on initial *)
(*       variables [init]. *\) *)
(*   val solve_constraints : *)
(*     'a JProgram.program -> *)
(*     Constraints.cst list -> *)
(*     Constraints.State.t -> Constraints.State.Var.t list -> Constraints.State.t *)

(* end = struct *)

(*   let debug_level = ref 0 *)

(*   open Constraints *)
(*   module Var = State.Var *)
(*   module HashVar = Hashtbl.Make(Var) *)

(*   open Constraints *)

(* let print_times () = *)
(*   let times = Unix.times () *)
(*   in Printf.printf "utime: %f\nstime: %f\n%!" *)
(*        times.Unix.tms_utime times.Unix.tms_stime *)


(*   (\* parameter module for hyper-graphs *\) *)
(*   let solve_constraints _program csts state_init entry_points = *)

(*     let _ = prerr_endline "calling solver"; print_times () in *)

(*     let module T = struct *)
(*       type vertex = int * Var.t *)
(*       type hedge = int * cst *)
(*       let vertex_dummy = -1, List.hd entry_points *)
(*       let hedge_dummy = -1, List.hd csts *)
(*       module IntIndex (D:sig type t end)= struct *)
(*         type t=int * D.t *)
(*         let equal (i1,_) (i2,_) = i1 == i2 *)
(*         let hash (x,_) = x *)
(*         let compare (i1,_) (i2,_) = i1 - i2 *)
(*       end *)
(*       module VIndexed = IntIndex(struct type t=Var.t end) *)
(*       module HIndexed = IntIndex(struct type t=cst end) *)
(*       module SetV = Sette.Make(VIndexed) *)
(*       module SetH = Sette.Make(HIndexed) *)
(*       module HashV = Hashhe.Make(VIndexed) *)
(*       module HashH = Hashhe.Make(HIndexed) *)
(*     end in *)
(*     let module Graph = SHGraph.Make(T) in  (\* Hyper-graph module *\) *)
(*     let module Fixpoint = MkFixpoint.Make(Graph) in (\* fixpoint engine *\) *)

(*     let csts_length = List.length csts in *)
(*     let graph = Graph.create csts_length () in *)
(*     let vertex_entry_points = *)
(*       (\* adding vertices and edges (variables and constraints) *\) *)
(*       let next_edge = ref 0 in *)
(*       let get_vertex : Var.t -> 'b = *)
(*         let hashvar = HashVar.create csts_length in *)
(*         let next_vertex = ref 0 in *)
(*           function var -> *)
(*             try HashVar.find hashvar var *)
(*             with Not_found -> *)
(*               let vertex = (!next_vertex,var) in *)
(*                 HashVar.add hashvar var vertex; *)
(*                 Graph.add_vertex graph vertex (); *)
(*                 incr next_vertex; *)
(*                 vertex *)
(*       in *)
(*         List.iter *)
(*           (fun cst -> *)
(*              let dep = *)
(*                Array.of_list *)
(*                  (List.map get_vertex (get_dependencies cst)) *)
(*              and target = *)
(*                [|get_vertex (get_target cst)|] *)
(*              in *)
(*              let edge = (!next_edge,cst) in *)
(*                incr next_edge; *)
(*                Graph.add_hedge graph edge () ~pred:dep ~succ:target) *)
(*           csts; *)
(*         List.fold_left *)
(*           (fun setv var -> T.SetV.add (get_vertex var) setv) *)
(*           T.SetV.empty *)
(*           entry_points *)
(*     in *)

(*     let _ = prerr_endline "initial graph computed"; print_times () in *)

(*     let manager : (State.abData,unit) Fixpoint.manager = *)
(*       let join : State.abData -> State.abData -> State.abData = *)
(*         fun abs1 abs2 -> match abs1,abs2 with *)
(*           | (`PP v1), `PP v2 -> `PP (State.PP.join v1 v2) *)
(*           | (`Method v1), `Method v2 -> `Method (State.Method.join v1 v2) *)
(*           | (`Field v1), `Field v2 -> `Field (State.Field.join v1 v2) *)
(*           | (`IOC v1), `IOC v2 -> `IOC (State.IOC.join v1 v2) *)
(*           | (`Global v1), `Global v2 -> `Global (State.Global.join v1 v2) *)
(*           | _, _ -> assert false        (\* incomparable *\) *)
(*       in { *)
(*           Fixpoint.bottom = *)
(*             (\* could be in State *\) *)
(*             begin function *)
(*               | _,`PP _ -> `PP State.PP.bot *)
(*               | _,`Method _ -> `Method State.Method.bot *)
(*               | _,`Field _ -> `Field State.Field.bot *)
(*               | _,`IOC _ -> `IOC State.IOC.bot *)
(*               | _,`Global _ -> `Global State.Global.bot *)
(*             end; *)
(*           Fixpoint.canonical = (fun _vertex _abs -> ()); *)
(*           Fixpoint.is_bottom = *)
(*             (\* should be in State and should really be isBot and not equal to bot *\) *)
(*             (fun _vertex -> function *)
(*                | `PP v -> State.PP.equal v State.PP.bot *)
(*                | `Field v -> State.Field.equal v State.Field.bot *)
(*                | `Method v -> State.Method.equal v State.Method.bot *)
(*                | `IOC v -> State.IOC.equal v State.IOC.bot *)
(*                | `Global v -> State.Global.equal v State.Global.bot); *)

(*           Fixpoint.is_leq = *)
(*             (\* should implemented in State / and Domain *\) *)
(*             begin fun _vertex abs1 abs2 -> match abs1,abs2 with *)
(*               | (`PP v1), `PP v2 -> *)
(*                   let modifies = ref false in *)
(*                     ignore (State.PP.join ~modifies v2 v1); *)
(*                     not !modifies *)
(*               | (`Field v1), `Field v2 -> *)
(*                   let modifies = ref false in *)
(*                     ignore (State.Field.join ~modifies v2 v1); *)
(*                     not !modifies *)
(*               | (`Method v1), `Method v2 -> *)
(*                   let modifies = ref false in *)
(*                     ignore (State.Method.join ~modifies v2 v1); *)
(*                     not !modifies *)
(*               | (`IOC v1), `IOC v2 -> *)
(*                   let modifies = ref false in *)
(*                     ignore (State.IOC.join ~modifies v2 v1); *)
(*                     not !modifies *)
(*               | (`Global v1), `Global v2 -> *)
(*                   let modifies = ref false in *)
(*                     ignore (State.Global.join ~modifies v2 v1); *)
(*                     not !modifies *)
(*               | _, _ -> assert false        (\* incomparable *\) *)
(*             end; *)

(*           Fixpoint.join = (fun _vertex abs1 abs2 -> join abs1 abs2); *)
(*           Fixpoint.join_list = *)
(*             (fun _vertex absl -> *)
(*                List.fold_left join (List.hd absl) (List.tl absl)); *)
(*           Fixpoint.widening = (fun _vertex abs1 abs2 -> join abs1 abs2); *)
(*           Fixpoint.apply = *)
(*             (fun (_,cst) tabs ->  *)
(*                match cst.transferFun tabs with *)
(*                  | `GlobalDomain _ as v -> *)
(*                      (),State.join_ad (`Global State.Global.bot) v *)
(*                  | `IOCDomain _ as v -> *)
(*                      (),State.join_ad (`IOC State.IOC.bot) v *)
(*                  | `FieldDomain _ as v -> *)
(*                      (),State.join_ad (`Field State.Field.bot) v *)
(*                  | `MethodDomain _ as v -> *)
(*                      (),State.join_ad (`Method State.Method.bot) v *)
(*                  | `PPDomain _ as v -> *)
(*                      (),State.join_ad (`PP State.PP.bot) v *)
(*             ); *)
(*           Fixpoint.arc_init = (fun _edge -> ()); *)
(*           Fixpoint.get_init = (fun (_,var) -> State.get state_init var); *)

(*           Fixpoint.print_abstract = (fun _fmt _ -> ()); *)
(*           Fixpoint.print_arc = (fun _fmt _ -> ()); *)
(*           Fixpoint.print_vertex = (fun _fmt _ -> ()); *)
(*           Fixpoint.print_hedge = (fun _fmt _ -> ()); *)

(*           Fixpoint.widening_first = false; *)
(*           Fixpoint.widening_start = max_int; *)
(*           Fixpoint.widening_freq = max_int; *)
(*           Fixpoint.widening_descend = 0; *)

(*           Fixpoint.print_analysis = false; *)
(*           Fixpoint.print_step = false; *)
(*           Fixpoint.print_state = false; *)
(*           Fixpoint.print_postpre = false; *)
(*         } *)
(*     in *)

(*     let _ = prerr_endline "manager declared/ computing strategy"; print_times () in *)

(*     let strat = Fixpoint.strategy_default graph vertex_entry_points in *)
(*     (\* let _ = Format.printf "strategy=%a@." (Fixpoint.print_strategy manager) strat in *\) *)
(*     let _ = prerr_endline "strategy found/iterating"; print_times () in *)

(*     let graph = Fixpoint.init manager graph vertex_entry_points in *)
(*     (\* let _ = Format.printf "initial graph=%a@." (Fixpoint.print_graph manager) graph in *\) *)
(*     let _ = prerr_endline "graph initialized"; print_times () in *)

(*     let _ = Fixpoint.fixpoint manager graph strat in *)
(*     let output = Fixpoint.output_of_graph graph in *)
(*     let _ = prerr_endline "fixpoint found"; print_times () in *)
(*     (\* let _ = Format.printf "output=%a@." (Fixpoint.print_output manager) output in *\) *)
(*       state_init *)


(* end *)

