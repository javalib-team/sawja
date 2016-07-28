(*
 * This file is part of SAWJA
 * Copyright (c)2010 Laurent Hubert (CNRS)
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

open Javalib_pack
open JBasics
open Javalib
open JProgram
open Safe

(** ReachabeMethods allows to compute the methods that are reachable from a set
    of entry points.  This can be used in order to update the field
    [parsed_method] of [!JProgram.program].  The implementation is not very
    efficient, but it is simple and rely on the [!Safe] framework. *)

(** Very simple Boolean domain.  A method is abstracted by [true] iff this
    method may be accessible.  *)
module Dom = struct
  type t = bool
  type analysisID = unit
  type analysisDomain = t
      
  let bot = false
  let isBot = (not)
  let join ?(modifies=ref false) v1 v2 =
    let res = v1 || v2 in
      if v1 != res then modifies := true;
      res
  let join_ad ?(do_join=true) =
    ignore do_join; (* so simple that do_join is useless *)
    join
  let equal = (==)
  let get_analysis () v = v
  let pprint fmt v = Format.pp_print_string fmt (string_of_bool v)
end

module Var = Var.Make(Var.EmptyContext)
module ED = Domain.Empty
module State = State.Make(Var)(ED)(ED)(ED)(Dom)(ED)
module Constraints = Constraints.Make(State)
module Solver = Solver.Make(Constraints)








let csts_of_cm program node cm =
  let successors = JCodePP.get_successors program node cm in
  let current_mn = `Method ((),get_name node, cm.cm_signature)
  in
    List.map
      (fun successor ->
         let (cn_succ,ms_succ) = cms_split successor in
           {Constraints.dependencies = [current_mn];
            Constraints.target = (`Method ((),cn_succ,ms_succ));
            Constraints.transferFun = (fun _ -> `MethodDomain true)
           }
      )
      (ClassMethodSet.elements successors)

let compute_csts program : Constraints.cst list =
  ClassMethodMap.fold
    (fun _ (c,cm) acc ->
       if cm.cm_implementation = Native
       then acc
       else List.rev_append (csts_of_cm program c cm) acc
    )
    program.parsed_methods
    []

let initial_state (entry_points:class_method_signature list) : State.t =
  (* TODO: calculate size on number of methods of program ? *)
  let state = State.bot (1,1,1,100000,1)
  in
    List.iter
      (function cms ->
         let (cn,ms) = cms_split cms
         in
           State.join
             state
             (`Method ((),cn,ms))
             (`MethodDomain true)
      )
      entry_points;
    state

(** [compute_reachable_methods p entry_points] will computes the methods that
    are reachable from the entry points [entry_points] in the program [p].  It
    assumes that [p.parsed_methods] is a correct over-approximation of reachable
    methods. *)
let compute_reachable_methods
    (program: JCode.jcode program)
    (entry_points : class_method_signature list)
    : (JCode.jcode node * JCode.jcode concrete_method) ClassMethodMap.t =
  let state = 
    let istate = initial_state entry_points
    and csts = compute_csts program
    and entry_var : Var.t list =
      List.map
        (fun cms -> let cn,ms = cms_split cms in `Method ((),cn,ms))
        entry_points
    in
      Solver.solve_constraints program csts istate entry_var
  in
    ClassMethodMap.fold
      (fun cms ccm set ->
         let cn,ms = cms_split cms in
           if State.get_method state (`Method ((),cn,ms))
           then ClassMethodMap.add cms ccm set
           else set
      )
      program.parsed_methods
      ClassMethodMap.empty
