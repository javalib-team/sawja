open Javalib_pack

module type S = sig

  module State:State.S

  type variable = State.Var.t

  type cst = {
    dependencies : variable list;
    target : variable;
    transferFun : State.t -> State.analysisDomain;
  }

  val get_dependencies : cst -> variable list
  val get_target : cst -> variable
  val pprint : Format.formatter -> cst -> unit
  val apply_cst : ?modifies:bool ref -> State.t -> cst -> unit
end

module Make (State:State.S) = struct

  module State = State

  type variable = State.Var.t

  type cst = {
    dependencies : variable list;
    target : variable;
    transferFun : State.t -> State.analysisDomain;
  }

  let get_dependencies cst = cst.dependencies
  let get_target cst = cst.target

  let apply_cst : ?modifies:bool ref -> State.t -> cst -> unit =
    fun ?(modifies=ref false) abst cst ->
      let target = get_target cst in
      let new_var = cst.transferFun abst in
	State.join ~modifies abst target new_var

  let pprint fmt cst =
    let pp_concat f pp_open pp_close pp_sep = function
      | [] -> ()
      | a::[] -> pp_open (); f a;pp_close ();
      | a::l ->
          pp_open ();
          f a;
          List.iter (fun a -> pp_sep ();f a) l;
          pp_close ()
    in
    let pprint_dependencies fmt dependencies =
      Format.pp_print_string fmt "[";
      pp_concat
	(State.Var.pprint fmt)
	ignore
	ignore
	(fun _ -> Format.fprintf fmt ",@,")
	dependencies;
      Format.pp_print_string fmt "]"
    in
      Format.fprintf fmt "{@[<hv>@[<hov 1>dependencies:%a@]@,@[<hov 1>target:%a@]@]}"
	pprint_dependencies cst.dependencies
	State.Var.pprint cst.target

end
