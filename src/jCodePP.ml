(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 * Copyright (c)2013 Pierre Vittet (INRIA)
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
open JControlFlow
open JCode

type t = JCode.jcode PP.t
let get_class = PP.get_class
let get_meth = PP.get_meth
let get_pc = PP.get_pc
let get_pp = PP.get_pp
let get_first_pp =PP.get_first_pp
let get_first_pp_wp =PP.get_first_pp_wp
let goto_absolute =PP.goto_absolute
let goto_relative =PP.goto_relative
let equal =PP.equal
let compare =PP.compare
let hash =PP.hash
let next_instruction =PP.next_instruction
let static_pp_lookup =PP.static_pp_lookup
let to_string = PP.to_string
let pprint = PP.pprint
let get_ir = PP.get_ir

let get_code pp = 
  let meth = (get_meth pp) in
  let cn = get_name (get_class pp) in
  let ms = meth.cm_signature in
  let impl = meth.cm_implementation in
    match impl with 
      | Native -> raise (PP.NoCode (cn, ms)) (* If we are in a pp, we should have code available. *)
      | Java laz -> (Lazy.force laz).c_code

let get_opcode pp =
  (get_code pp).(get_pc pp)

let static_lookup program pp =
  match get_opcode pp with
    | OpInvoke (`Virtual obj, ms) ->
        Some (static_lookup_virtual program obj ms)
    | OpInvoke (`Static cs, ms) ->
        Some ([static_lookup_static program cs ms])
    | OpInvoke (`Special cs, ms) ->
        Some ([Class (static_lookup_special program (get_class pp) cs ms)])
    | OpInvoke (`Interface cs, ms) ->
        Some (static_lookup_interface program cs ms)
    | _ -> None


  (*Veut t'on une implémentation plus compliqué et rarement plus précise -> 
   voir code de Vincent.*)
let handlers pp =
  let open JCode in
  let pc = (get_pc pp) in
  let cn = get_name (get_class pp) in
  let meth = (get_meth pp) in
  let ms = meth.cm_signature in
  let impl = meth.cm_implementation in
  let excs = 
    match impl with 
      | Native -> raise (PP.NoCode (cn,ms))
      | Java laz-> (Lazy.force laz).c_exc_tbl
  in
    List.filter (fun exc -> 
    pc >= exc.e_start && pc < exc.e_end
    ) excs

let normal_successors pp =
  match get_opcode pp with
    | OpIf (_,l)
    | OpIfCmp (_,l) ->
        [next_instruction pp;goto_relative pp l]
    | OpJsr l
    | OpGoto l -> [goto_relative pp l]
    | OpRet _ -> (* all instruction following a jsr are returned. *)
        let code = get_code pp in
        let i = ref 0 in
        let l = ref [] in
          while !i < Array.length code do
            begin
              match code.(!i) with
                | OpJsr _ ->
                    l := next_instruction (goto_absolute pp !i)::!l
                | _ -> ()
            end;
            incr i;
          done;
          !l
    | OpTableSwitch (default,_,_,others) ->
        Array.fold_left
          (fun ppl jmp -> goto_relative pp jmp::ppl)
          [goto_relative pp default]
          others
    | OpLookupSwitch (default,others) ->
        List.fold_left
          (fun ppl (_,jmp) -> goto_relative pp jmp::ppl)
          [goto_relative pp default]
          others
    | OpThrow
    | OpReturn _ -> []
    | OpInvalid
    | OpBreakpoint ->
        raise (Class_structure_error "Instructions Invalid and Breakpoint are not authorized")
    | _ -> [next_instruction pp]


let exceptional_successors pp =
  List.map (fun e -> goto_absolute pp e.JCode.e_handler) (handlers pp)



let get_class_to_initialize caller = function
  | Interface _ as callee ->
      if defines_method callee clinit_signature
      then Some (make_cms (get_name callee) clinit_signature)
      else None
  | Class callee ->
      let caller =
        match caller with
          | Interface {i_super = caller}
          | Class caller
            -> caller
      in
      let rec find_first_cl_with_clinit callee :'a class_node option=
        if defines_method (Class callee) clinit_signature
        then Some callee
        else match callee.c_super with
          | None -> None
          | Some s -> find_first_cl_with_clinit s
      in
        match find_first_cl_with_clinit callee with
          | None -> None
          | Some callee ->
              let rec find_last_cl_with_clinit prev callee =
                match callee.c_super with
                  | Some s_callee when (extends_class caller s_callee) -> prev
                  | Some s_callee
                      when defines_method (Class s_callee) clinit_signature
                        -> find_last_cl_with_clinit (Some s_callee) s_callee
                  | Some s_callee -> find_last_cl_with_clinit prev s_callee
                  | None -> prev
              in
                match find_last_cl_with_clinit None callee with
                  | None -> None
                  | Some c ->
                      Some (make_cms c.c_info.c_name clinit_signature)

(** returns the possible methods that may be invoked from the current
    program point. For the static initialization, only the topmost
    class initializer is return, and the successors of a clinit
    methods includes the clinit methods that are beneath. *)
let get_successors program node m = 
  let successors = ref ClassMethodSet.empty in
    (*Check clinit*)
    if m.cm_signature = clinit_signature
    then
      begin
        let rec add_c_children_clinit class_node =
          try
            successors :=
            ClassMethodSet.add
              (get_class_method_signature
                 (get_method (Class class_node) clinit_signature))
              !successors
          with _ ->
            List.iter add_c_children_clinit class_node.c_children
        in
          match node with
            | Class class_node ->
                List.iter add_c_children_clinit class_node.c_children
            | Interface _ -> ()
      end;
    (*start looking at the implementation.*)
     (match m.cm_implementation with
        | Native -> ()
        | Java c ->
      let ppinit = get_pp node m 0 in
        Array.iteri
          (fun pc opcode -> match opcode with
       | JCode.OpNew cn' ->
           let c'= (get_node program cn') in
             begin
      	 match (get_class_to_initialize node c') with
      	   | None -> ()
      	   | Some c ->
      	       successors := ClassMethodSet.add c !successors
             end
       | JCode.OpGetStatic (cn',fs')
       | JCode.OpPutStatic (cn',fs') ->
           (* successeur : clinit du plus haut parant de cn'
      	n'ayant peut-être pas déjà été initialisé.
      	java.lang.Object.<clinit> est donc correct,
      	mais (TODO) on pourrait être plus précis
      	(entre autre, inutile d'initialisé une
      	super-classe de la classe courrante). *)
           let c'= (get_node program cn')
           in
             List.iter
      	 (fun c' ->
      	    match (get_class_to_initialize node c') with
      	      | None -> ()
      	      | Some c ->
      		  successors := ClassMethodSet.add c !successors
      	 )
      	 (JControlFlow.resolve_field fs' c')
       | JCode.OpInvoke (kind,ms) ->
           begin
             match kind with
      	 | `Static cn' ->
      	     let c' = match get_node program cn' with
      	       | Class c' -> c'
      	       | Interface _ -> raise IncompatibleClassChangeError
      	     in
      	     let c' = JControlFlow.resolve_method' ms c' in
      	       (match (get_class_to_initialize node (Class c')) with
      		  | None -> ()
      		  | Some c ->
      		      successors :=
      			ClassMethodSet.add c !successors)
      	 | _ -> ()
           end;
           let targets =
             let pp = goto_absolute ppinit pc
             in static_pp_lookup program pp
           in
             successors :=
      	 List.fold_left
      	   (fun successors pp ->
      	      let cms =
      		(get_meth pp).cm_class_method_signature
      	      in
      		ClassMethodSet.add cms successors)
      	   !successors
      	   targets
       | _ -> ()
          )
          (Lazy.force c).JCode.c_code);
     !successors

