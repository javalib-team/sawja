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
      let dico = ref Ptmap.empty in
      let get_var var =
        (* get the index of the node/var/target [var] *)
        try 
          let vari = HashVar.find var_indices var in
            assert (0 = Var.compare var (Ptmap.find vari !dico));
            (* if 0 <> Var.compare var var' *)
            (* then *)
            (*   Format.fprintf Format.std_formatter *)
            (*     "%i associated with %a in var_indices and %a in dico" *)
            (*     vari Var.pprint var Var.pprint var'; *)
            vari
        with Not_found ->
          let new_var = !var_current in
            incr var_current;
            HashVar.add var_indices var new_var;
            (* Format.fprintf Format.std_formatter "associates %i with %a@." *)
            (*   new_var Var.pprint var; *)
            assert (not (Ptmap.mem new_var !dico));
            (* using assert on the next line so to remove this code when
               compiling with -noassert *)
            assert (dico := Ptmap.add new_var var !dico; true);
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
            Format.pp_print_string Format.std_formatter "dictionnary :";
            Format.pp_print_newline Format.std_formatter ();
            HashVar.iter
              (fun var index ->
                 Format.pp_print_string Format.std_formatter (string_of_int index ^" <-| ");
                 Var.pprint Format.std_formatter var;
                 Format.pp_print_newline Format.std_formatter ())
              var_indices;
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
            (* if !debug_level > 1 then *)
            (*   Constraints.pprint Format.std_formatter cst.cst; *)
            Constraints.apply_cst ~modifies !abState cst.cst;
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
            if !modifies then
              ((* time_to_modify := *)
                (*   !time_to_modify +. ((Unix.times ()).Unix.tms_utime -. start); *)
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
              ((* time_to_idle := *)
                (*   !time_to_idle +. ((Unix.times ()).Unix.tms_utime -. start); *)
                (* if !debug_level > 1 then *)
                (*   Format.pp_print_string Format.std_formatter " did not modify\n"; *)
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
      (not_used_anymore:'a)
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

