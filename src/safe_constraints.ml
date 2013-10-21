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
  val apply_cst : ?do_join:bool -> ?modifies:bool ref -> State.t -> cst -> unit
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



  let apply_cst : ?do_join:bool -> ?modifies:bool ref -> State.t -> cst -> unit =
    fun ?(do_join=true) ?(modifies=ref false) abst cst ->
            try
              let target = get_target cst in
              let new_var = cst.transferFun abst in
                State.join ~do_join ~modifies abst target new_var
            with Domain.DebugDom -> 
              Printf.eprintf "Error during following constraint: \n" ;
              pprint Format.err_formatter cst ;
              raise (State.DebugSt abst)
end
