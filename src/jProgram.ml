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
open JCode
open Javalib

module ClassMethMap = Map.Make(
  struct
    type t = class_name * method_signature
    let compare = compare
  end)

module ClassMethSet = Set.Make(
  struct
    type t = class_name * method_signature
    let compare = compare
  end)

type 'a class_node = {
  c_info : 'a jclass;
  c_super : 'a class_node option;
  c_interfaces : 'a interface_node ClassMap.t;
  mutable c_children : 'a class_node list;
}
and 'a interface_node = {
  i_info : 'a jinterface;
  i_super : 'a class_node;
  (** must be java.lang.Object. But note that interfaces are not
      considered as children of java.lang.Object.*)
  i_interfaces : 'a interface_node ClassMap.t;
  mutable i_children_interfaces : 'a interface_node list;
  mutable i_children_classes : 'a class_node list
}
and 'a node =
  | Interface of 'a interface_node
  | Class of 'a class_node

let main_signature =
  let java_lang_string = make_cn "java.lang.String" in
    make_ms "main"
      ([TObject (TArray (TObject
			   (TClass java_lang_string)))]) None

let get_name = function
  | Interface i -> i.i_info.i_name
  | Class c -> c.c_info.c_name

let get_interfaces = function
  | Interface i -> i.i_interfaces
  | Class c -> c.c_interfaces

let get_consts = function
  | Interface i -> i.i_info.i_consts
  | Class c -> c.c_info.c_consts

let equal c1 c2 = match c1,c2 with
  | Class c1, Class c2 ->
      c1 == c2
      (* equal_class_names c1.c_info.c_signature c2.c_info.c_signature *)
  | Interface i1, Interface i2 ->
      i1 == i2
      (* equal_class_names i1.i_info.i_signature i2.i_info.i_signature *)
  | _, _ -> false

let rec get_all_children_classes c =
  let direct_children = c.c_children in
    List.rev_append direct_children
      (List.fold_right
	 (fun c r ->
	    List.rev_append r (get_all_children_classes c)
	 ) direct_children [])

type 'a static_lookup_method = class_name -> method_signature -> int ->
  ('a class_node * 'a concrete_method) ClassMethodMap.t
type 'a program = { classes : 'a node ClassMap.t;
		    parsed_methods : ('a node *
					'a concrete_method) ClassMethodMap.t;
		    static_lookup_method : 'a static_lookup_method }
type 'a t = 'a program

let super = function
  | Interface i -> Some i.i_super
  | Class c -> c.c_super

exception IncompatibleClassChangeError
exception NoSuchMethodError
exception NoSuchFieldError
exception NoClassDefFoundError
exception AbstractMethodError
exception IllegalAccessError

let to_jclass ioc =
  match ioc with
    | Interface i -> JInterface (i.i_info)
    | Class c -> JClass (c.c_info)

let defines_method ioc ms = Javalib.defines_method (to_jclass ioc) ms

let defines_field ioc fs = Javalib.defines_field (to_jclass ioc) fs

let get_node program cs =
  ClassMap.find cs program.classes

let super_class c : 'a class_node option = super c

let get_method ioc ms =
  Javalib.get_method (to_jclass ioc) ms

let get_methods ioc = Javalib.get_methods (to_jclass ioc)

let get_concrete_methods ioc = Javalib.get_concrete_methods (to_jclass ioc)

let get_field ioc fs =
  Javalib.get_field (to_jclass ioc) fs

let get_fields ioc = Javalib.get_fields (to_jclass ioc)


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
  if (equal (Class c1) (Class c2)) then true
  else
    match super (Class c1) with
      | None -> false
      | Some sc -> extends_class sc c2

let rec extends_interface i1 i2 =
  if (equal (Interface i1) (Interface i2)) then true
  else
    ClassMap.fold
      (fun _ i3 b -> b || extends_interface i3 i2)
      i1.i_interfaces false

let extends ioc1 ioc2 =
  match (ioc1, ioc2) with
    | (Class _, Interface _) -> false
    | (Class c1, Class c2) -> extends_class c1 c2
    | (Interface i1, Interface i2) -> extends_interface i1 i2
    | (Interface i, Class c) ->
	cn_equal i.i_super.c_info.c_name c.c_info.c_name

let rec implements (c1:'a class_node) (i2:'a interface_node) : bool =
  if
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface i3 i2)
	c1.c_interfaces
	false
    )
  then true
  else match super (Class c1) with
    | None -> false
    | Some c3 -> implements c3 i2

let rec super_interfaces i =
  ClassMap.fold
    (fun _iname i ilist ->
      i::(List.rev_append (super_interfaces i) ilist))
    i.i_interfaces
    []

let rec implemented_interfaces' (c:'a class_node) : 'a interface_node list =
  let directly_implemented_interfaces =
    ClassMap.fold
      (fun _iname i ilist ->
	i::(List.rev_append (super_interfaces i) ilist))
      c.c_interfaces
      []
  in
    match super (Class c) with
      | None -> directly_implemented_interfaces
      | Some c' ->
	  List.rev_append directly_implemented_interfaces (implemented_interfaces' c')

let rec rem_dbl = function
  | e::(_::_ as l) -> e:: (List.filter ((!=)e) (rem_dbl l))
  | l -> l

let implemented_interfaces c = rem_dbl (implemented_interfaces' c)

let rec firstCommonSuperClass (c1:'a class_node) (c2:'a class_node) : 'a class_node =
  if extends_class c1 c2
  then c2
  else match super_class (Class c2) with
    | Some c3 -> firstCommonSuperClass c1 c3
    | None -> raise (Failure "firstCommonSuperClass: c1 and c2 has been found such that c1 does not extends c2 and c2 has no super class.")

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
			   List.map cms_split
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
	   | Interface {i_info = {i_name = cs; i_initializer = Some m}} ->
               calls :=
                 (get_method_calls p cs (ConcreteMethod m)) @ !calls
           | Interface _ -> ()
	   | Class c ->
	       MethodMap.iter
		 (fun _ m ->
		    calls := (get_method_calls p c.c_info.c_name m) @ !calls)
                 c.c_info.c_methods
      ) p;
    !calls

let store_callgraph callgraph file =
  let out = IO.output_channel (open_out file) in
    List.iter
      (fun ((cs,ms,pp),(ccs,cms)) ->
	 IO.nwrite out
	   (JPrint.method_signature
	      ~caller:(Some (TClass cs)) ms
	    ^ ","
	    ^ (string_of_int pp) ^ " -> "
	    ^ JPrint.method_signature
	      ~caller:(Some (TClass ccs)) cms
	    ^ "\n")
      )
      callgraph;
    IO.close_out out
