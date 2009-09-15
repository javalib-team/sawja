(*
 * This file is part of Jaws
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see 
 * <http://www.gnu.org/licenses/>.
 *)

(* TODO : add cache memories in lookup functions *)

open JBasics
open JOpcodes
open Javalib

module ClassSet = Set.Make(
  struct
    type t = class_name
    let compare = compare_class_names
  end)

module MethodSet = Set.Make(
  struct
    type t = method_signature
    let compare = compare_method_signatures
  end)

module ClassMethSet = Set.Make(
  struct
    type t = class_name * method_signature
    let compare = compare
  end)

module ClassMethMap = Map.Make(
  struct
    type t = class_name * method_signature
    let compare = compare
  end)

type 'a class_file = {
  c_info : 'a jclass;
  c_super : 'a class_file option;
  c_interfaces : 'a interface_file ClassMap.t;
  get_c_children : unit -> 'a class_file list;
}
and 'a interface_file = {
  i_info : 'a jinterface;
  i_super : 'a class_file;
  (** must be java.lang.Object. But note that interfaces are not
      considered as children of java.lang.Object.*)
  i_interfaces : 'a interface_file ClassMap.t;
  get_i_children_interfaces : unit -> 'a interface_file list;
  get_i_children_classes : unit -> 'a class_file list
}
and 'a interface_or_class = [ `Interface of 'a interface_file | `Class of 'a class_file ]

let main_signature = make_method_signature "main"
  ([TObject (TArray (TObject
		       (TClass ["java";"lang";"String"])))], None)

let get_signature = function
  | `Interface i -> i.i_info.i_signature
  | `Class c -> c.c_info.c_signature

let get_name ioc = class_name2class_name (get_signature ioc)

let get_interfaces = function
  | `Interface i -> i.i_interfaces
  | `Class c -> c.c_interfaces

let get_consts = function
  | `Interface i -> i.i_consts
  | `Class c -> c.c_consts

let equal c1 c2 = match c1,c2 with
  | `Class c1, `Class c2 ->
      c1 == c2
      (* equal_class_names c1.c_info.c_signature c2.c_info.c_signature *)
  | `Interface i1, `Interface i2 ->
      i1 == i2
      (* equal_class_names i1.i_info.i_signature i2.i_info.i_signature *)
  | _, _ -> false

let rec get_all_children_classes c =
  let direct_children = c.get_c_children () in
    List.rev_append direct_children
      (List.fold_right
	 (fun c r ->
	    List.rev_append r (get_all_children_classes c)
	 ) direct_children [])

type 'a static_lookup_method = class_name -> method_signature -> int ->
  ('a class_file * 'a concrete_method) ClassMethodMap.t
type 'a program = { classes : 'a interface_or_class ClassMap.t;
		    parsed_methods : ('a interface_or_class *
					'a concrete_method) ClassMethodMap.t;
		    static_lookup_method : 'a static_lookup_method }
type 'a t = 'a program

let super = function
  | `Interface i -> Some i.i_super
  | `Class c -> c.c_super

exception IncompatibleClassChangeError
exception NoSuchMethodError
exception NoSuchFieldError
exception NoClassDefFoundError
exception AbstractMethodError
exception IllegalAccessError

let to_class ioc =
  match ioc with
    | `Interface i -> `Interface (i.i_info)
    | `Class c -> `Class (c.c_info)

let defines_method ms ioc = JClass.defines_method ms (to_class ioc)

let defines_field fs ioc = JClass.defines_field fs (to_class ioc)

let get_interface_or_class program cs =
  ClassMap.find cs program.classes

let super_class c : 'a class_file option = super c

let get_method ioc ms = JClass.get_method (to_class ioc) ms

let get_methods ioc = JClass.get_methods (to_class ioc)

let get_concrete_methods ioc = JClass.get_concrete_methods (to_class ioc)

let get_field ioc fs = JClass.get_field (to_class ioc) fs

let get_fields ioc = JClass.get_fields (to_class ioc)


let store_program filename program : unit =
  let ch = open_out_bin filename
  in
    Marshal.to_channel ch program [];
    close_out ch

let load_program filename : 'a program =
  let ch = open_in_bin filename
  in let p = (Marshal.from_channel ch : 'a program);
  in close_in ch; p


(* Iterators *)
let fold f s p = ClassMap.fold (fun _ c s -> f s c) p.classes s
let iter f p = ClassMap.iter (fun _ c -> f c) p.classes

(* Access to the hierarchy *)

let rec extends_class c1 c2 =
  if (equal (`Class c1) (`Class c2)) then true
  else
    match super (`Class c1) with
      | None -> false
      | Some sc -> extends_class sc c2

let rec extends_interface i1 i2 =
  if (equal (`Interface i1) (`Interface i2)) then true
  else
    ClassMap.fold
      (fun _ i3 b -> b || extends_interface i3 i2)
      i1.i_interfaces false

let extends ioc1 ioc2 =
  match (ioc1, ioc2) with
    | (`Class _, `Interface _) -> false
    | (`Class c1, `Class c2) -> extends_class c1 c2
    | (`Interface i1, `Interface i2) -> extends_interface i1 i2
    | (`Interface i, `Class c) ->
	equal_class_names i.i_super.c_info.c_signature c.c_info.c_signature

let rec implements (c1:'a class_file) (i2:'a interface_file) : bool =
  if
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface i3 i2)
	c1.c_interfaces
	false
    )
  then true
  else match super (`Class c1) with
    | None -> false
    | Some c3 -> implements c3 i2

let rec super_interfaces i =
  ClassMap.fold
    (fun _iname i ilist ->
      i::(List.rev_append (super_interfaces i) ilist))
    i.i_interfaces
    []

let rec implemented_interfaces' (c:'a class_file) : 'a interface_file list =
  let directly_implemented_interfaces =
    ClassMap.fold
      (fun _iname i ilist ->
	i::(List.rev_append (super_interfaces i) ilist))
      c.c_interfaces
      []
  in
    match super (`Class c) with
      | None -> directly_implemented_interfaces
      | Some c' ->
	  List.rev_append directly_implemented_interfaces (implemented_interfaces' c')

let rec rem_dbl = function
  | e::(_::_ as l) -> e:: (List.filter ((!=)e) (rem_dbl l))
  | l -> l

let implemented_interfaces c = rem_dbl (implemented_interfaces' c)

let rec firstCommonSuperClass (c1:'a class_file) (c2:'a class_file) : 'a class_file =
  if extends_class c1 c2
  then c2
  else match super_class (`Class c2) with
    | Some c3 -> firstCommonSuperClass c1 c3
    | None -> raise (Failure "firstCommonSuperClass: c1 and c2 has been found such that c1 does not extends c2 and c2 has no super class.")

let rec resolve_interface_method ?(acc=[]) msi (c:'a interface_or_class) : 'a interface_file list =
  ClassMap.fold
    (fun _ i acc ->
      if defines_method msi (`Interface i)
      then i::acc
      else resolve_interface_method ~acc msi (`Interface i))
    (get_interfaces c)
    acc

let rec resolve_implemented_method ?(acc=[]) msi (c:'a class_file)
    : ('a class_file option * 'a interface_file list) =
  match c.c_super with
    | None -> (None,resolve_interface_method ~acc msi (`Class c))
    | Some sc ->
	if defines_method msi (`Class sc)
	then (Some sc,resolve_interface_method ~acc msi (`Class c))
	else resolve_implemented_method ~acc:(resolve_interface_method ~acc msi (`Class c)) msi sc

exception Invoke_not_found of (class_name * method_signature
			       * class_name * method_signature)

let get_method_calls p cs m =
  let l = ref [] in
  let f_lookup = p.static_lookup_method in
  let method2callsite cs ms pp (ccs,cms) =
    ((cs,ms,pp),(ccs,cms))
  in
    begin
      match m with
	| ConcreteMethod ({cm_implementation = Java code} as cm)
	  when
	    ClassMethodMap.mem cm.cm_class_method_signature p.parsed_methods ->
	    let ms = cm.cm_signature in
	      Array.iteri
		(fun pp op ->
		   match op with
		     | OpInvoke _ ->
			 let callsites = (f_lookup cs ms pp) in
			 let callsites_list =
			   List.map split_class_method_signature
			     (ClassMethodMap.key_elements callsites)
			 in
			   l :=
			     List.rev_append
			       (List.map (method2callsite cs ms pp) callsites_list)
			       !l
		     | _ -> ())
		(Lazy.force code).c_code
	| _ -> ()
    end;
    !l

type callgraph = ((class_name * method_signature * int)
		  * (class_name * method_signature)) list

let get_callgraph p =
  let calls = ref [] in
    iter
      (fun ioc ->
	 match ioc with
	   | `Interface {i_info = {i_signature = cs; i_initializer = Some m}} ->
               calls :=
                 (get_method_calls p cs (ConcreteMethod m)) @ !calls
           | `Interface _ -> ()
	   | `Class c ->
	       MethodMap.iter
		 (fun _ m ->
		    calls := (get_method_calls p c.c_info.c_signature m) @ !calls)
                 c.c_info.c_methods
      ) p;
    !calls

let store_callgraph callgraph file =
  let out = IO.output_channel (open_out file) in
    List.iter
      (fun ((cs,ms,pp),(ccs,cms)) ->
	 IO.nwrite out
	   ((JDumpBasics.class_name (class_name2class_name cs)) ^ "."
	    ^ (method_signature2method_name ms)
	    ^ (JUnparseSignature.unparse_method_descriptor
		 (method_signature2method_descriptor ms)) ^ ","
	    ^ (string_of_int pp) ^ " -> "
	    ^ (JDumpBasics.class_name (class_name2class_name ccs)) ^ "."
	    ^ (method_signature2method_name cms)
	    ^ (JUnparseSignature.unparse_method_descriptor
		 (method_signature2method_descriptor cms)) ^ "\n")
      )
      callgraph;
    IO.close_out out
