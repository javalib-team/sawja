(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 * Copyright (c)2012 Pierre Vittet (INRIA)
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
open JCode
open Javalib

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

let make_class_node c super interfaces =
  let node =
    {c_info = c;
     c_super = super;
     c_interfaces = interfaces;
     c_children = []} in
    ClassMap.iter
      (fun _ i ->
	 if not(List.exists
		  (fun chc -> cn_equal chc.c_info.c_name c.c_name)
		  i.i_children_classes) then
	   i.i_children_classes <- node :: i.i_children_classes
	 else failwith "Inconsistent make_class_node : child already present."
      ) interfaces;
    (match super with
       | None -> ()
       | Some sc ->
	   (match c.c_super_class with
	      | Some scn when cn_equal scn sc.c_info.c_name ->
		  if not(List.exists
			   (fun chc -> cn_equal chc.c_info.c_name c.c_name)
			   sc.c_children) then
		    sc.c_children <- node :: sc.c_children
		  else
		    failwith "Inconsistent make_class_node : child already present."
	      | _ ->
		  failwith "Inconsistent make_class_node : different super classes."
	   )
    );
    node

let make_interface_node i super interfaces =
  let node =
    {i_info = i;
     i_super = super;
     i_interfaces = interfaces;
     i_children_classes = [];
     i_children_interfaces = []} in
    ClassMap.iter
      (fun _ si ->
	 if not(List.exists
		  (fun chi -> cn_equal chi.i_info.i_name i.i_name)
		  si.i_children_interfaces) then
	   si.i_children_interfaces <- node :: si.i_children_interfaces
	 else
	   failwith "Inconsistent make_interface_node : child already present."
      ) interfaces;
    node

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

let c_equal = (==)
let i_equal = (==)

let node_equal c1 c2 = match c1,c2 with
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
  ClassMethodSet.t

type 'a program = { classes : 'a node ClassMap.t;
		    parsed_methods : ('a node *
					'a concrete_method) ClassMethodMap.t;
		    static_lookup_method : 'a static_lookup_method }

let super = function
  | Interface i -> Some i.i_super
  | Class c -> c.c_super

exception IncompatibleClassChangeError
exception NoSuchMethodError
exception NoSuchFieldError
exception NoClassDefFoundError
exception AbstractMethodError
exception IllegalAccessError

let to_ioc ioc =
  match ioc with
    | Interface i -> JInterface (i.i_info)
    | Class c -> JClass (c.c_info)

let defines_method ioc ms = Javalib.defines_method (to_ioc ioc) ms

let defines_field ioc fs = Javalib.defines_field (to_ioc ioc) fs

let get_node program cs =
  ClassMap.find cs program.classes

let super_class c : 'a class_node option = super c

let get_method ioc ms =
  Javalib.get_method (to_ioc ioc) ms

let get_concrete_method ioc ms =
  Javalib.get_concrete_method (to_ioc ioc) ms

let get_methods ioc = Javalib.get_methods (to_ioc ioc)

let get_concrete_methods ioc = Javalib.get_concrete_methods (to_ioc ioc)

let get_field ioc fs =
  Javalib.get_field (to_ioc ioc) fs

let get_fields ioc = Javalib.get_fields (to_ioc ioc)


(* Iterators *)
let fold f s p = ClassMap.fold (fun _ c s -> f s c) p.classes s
let iter f p = ClassMap.iter (fun _ c -> f c) p.classes


let cf_iter f node = Javalib.cf_iter f (to_ioc node)
let if_iter f node = Javalib.if_iter f (to_ioc node)
let f_iter f node = Javalib.f_iter f (to_ioc node)

let cf_fold f node = Javalib.cf_fold f (to_ioc node)
let if_fold f node = Javalib.if_fold f (to_ioc node)
let f_fold f node = Javalib.f_fold f (to_ioc node)

let am_iter f node = Javalib.am_iter f (to_ioc node)
let cm_iter f node = Javalib.cm_iter f (to_ioc node)
let m_iter f node = Javalib.m_iter f (to_ioc node)

let am_fold f node = Javalib.am_fold f (to_ioc node)
let cm_fold f node = Javalib.cm_fold f (to_ioc node)
let m_fold f node = Javalib.m_fold f (to_ioc node)



(* Access to the hierarchy *)

let rec extends_class c1 c2 =
  if c_equal c1 c2 then true
  else
    match c1.c_super with
      | Some sc -> extends_class sc c2
      | None -> false

let rec extends_interface i1 i2 =
  if i_equal i1 i2 then true
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
	c_equal i.i_super.c_info c.c_info

let rec implements (c1:'a class_node) (i2:'a interface_node) : bool =
  if
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface i3 i2)
	c1.c_interfaces
	false
    )
  then true
  else match c1.c_super with
    | None -> false
    | Some c3 -> implements c3 i2

let rec super_interfaces i =
  ClassMap.fold
    (fun _iname i ilist ->
      i::(List.rev_append (super_interfaces i) ilist))
    i.i_interfaces
    []


let directly_implemented_interfaces c =
  ClassMap.fold
    (fun _iname i ilist ->
       i::(List.rev_append (super_interfaces i) ilist))
    c.c_interfaces
    []

let rec implemented_interfaces' (c:'a class_node) : 'a interface_node list =
    match super (Class c) with
      | None -> (directly_implemented_interfaces c)
      | Some c' ->
	  List.rev_append (directly_implemented_interfaces c) (implemented_interfaces' c')

let rec rem_dbl = function
  | e::(_::_ as l) -> e:: (List.filter ((!=)e) (rem_dbl l))
  | l -> l

let implemented_interfaces c = rem_dbl (implemented_interfaces' c)


let rec first_common_super_class (c1:'a class_node) (c2:'a class_node) : 'a class_node =
  if extends_class c1 c2
  then c2
  else match super_class (Class c2) with
    | Some c3 -> first_common_super_class c1 c3
    | None -> failwith "first_common_super_class: c1 and c2 has been found such that c1 does not extends c2 and c2 has no super class."

let get_method_calls p cs cm =
  let l = ref Ptmap.empty in
  let f_lookup = p.static_lookup_method in
    begin
      match cm with
	| {cm_implementation = Java code}
	    when
	      ClassMethodMap.mem cm.cm_class_method_signature p.parsed_methods ->
	    let ms = cm.cm_signature in
	      Array.iteri
		(fun pp op ->
		   match op with
		     | OpInvoke _ ->
			 let lookup = (f_lookup cs ms pp) in
			   l := Ptmap.add pp lookup !l
		     | _ -> ())
		(Lazy.force code).c_code
	| _ -> ()
    end;
    !l

type callgraph = ((class_name * method_signature * int)
		  * (class_name * method_signature)) list


(*TODO : add edge from methods that trigger a <clinit> to corresponding <clinit> methods
  (for the moment <clinit> methods have no caller method in the callgraph)*)
let get_callgraph p =
  let methodcalls2callsite cs ms calls =
    let l = ref [] in
      Ptmap.iter
	(fun pp cmset ->
	   ClassMethodSet.iter
	     (fun ccms ->
		let (ccs,cms) = cms_split ccms in
		  l := ((cs,ms,pp),(ccs,cms)) :: !l
	     ) cmset
	) calls;
      !l in
  let calls = ref [] in
    iter
      (fun ioc ->
	 match ioc with
	   | Interface {i_info = {i_name = cs; i_initializer = Some cm}} ->
               calls :=
                 (methodcalls2callsite cs cm.cm_signature
		    (get_method_calls p cs cm)) @ !calls
           | Interface _ -> ()
	   | Class c ->
	       MethodMap.iter
		 (fun _ m ->
		    match m with
		      | ConcreteMethod cm ->
			  let cs = c.c_info.c_name in
			    calls :=
			      (methodcalls2callsite cs cm.cm_signature
				 (get_method_calls p cs cm))
			    @ !calls
		      | AbstractMethod _ -> ()
		 ) c.c_info.c_methods
      ) p;
    !calls

let get_callgraph_from_entries p entries = 
  let methodcalls2callsite cs ms calls =
    let l = ref [] in
      Ptmap.iter
	(fun pp cmset ->
	   ClassMethodSet.iter
	     (fun ccms ->
		let (ccs,cms) = cms_split ccms in
		  l := ((cs,ms,pp),(ccs,cms)) :: !l
	     ) cmset
	) calls;
      !l in
  let calls = ref [] in
  let history = ref (List.fold_left 
                       (fun map el -> ClassMethodSet.add el map) 
                       (ClassMethodSet.empty) entries) 
  in
  let workset = ref entries in
    while ((List.length !workset) > 0 ) do
      (
        let cur_cms = List.hd !workset in
          workset :=List.tl !workset;
          let (cur_cn, cur_ms) = cms_split cur_cms in
          let m = get_method (get_node p cur_cn) cur_ms in
            match m with
              | ConcreteMethod cm ->
                  let mcalls = (get_method_calls p cur_cn cm) in
                    Ptmap.iter
                      (fun _pp cmsSet ->
                         ClassMethodSet.iter 
                           (fun cms ->
                              if (ClassMethodSet.mem cms !history)
                              then ()
                              else 
                                (history := ClassMethodSet.add cms !history;
                                 workset := cms::!workset
                                )
                           )
                           cmsSet
                      )
                      mcalls;
                    calls := ((methodcalls2callsite cur_cn cm.cm_signature mcalls)
                              @ !calls)
              | AbstractMethod _ -> ()
      )
    done;
    !calls

let store_callgraph callgraph file =
  let out = IO.output_channel (open_out file) in
    List.iter
      (fun ((cs,ms,pp),(ccs,cms)) ->
	 IO.nwrite out
	   (JPrint.method_signature
	      ~callee:(TClass cs) ms
	    ^ ","
	    ^ (string_of_int pp) ^ " -> "
	    ^ JPrint.method_signature
	      ~callee:(TClass ccs) cms
	    ^ "\n")
      )
      callgraph;
    IO.close_out out

let to_class_node node =
  match node with
    | Class c -> c
    | Interface _ -> failwith "to_class_node applied on an interface node."

let _to_concrete_method m =
  match m with
    | ConcreteMethod cm -> cm
    | AbstractMethod _ -> failwith "to_concrete_method applied on an abstract method."

let to_interface_node node =
  match node with
    | Class _ -> failwith "to_interface_node applied on a class node."
    | Interface i -> i

let add_interface_or_class cmap c nodemap =
  let cobject = ClassMap.find JBasics.java_lang_object cmap in 
  let rec add_interface_or_class c (nodemap : 'a node ClassMap.t) =
    let cn = Javalib.get_name c in
      try
	let node = ClassMap.find cn nodemap in
	  (nodemap, node)
      with Not_found ->
	match c with
	  | JClass c ->
	      let (nodemap, super_node) =
		(match c.c_super_class with
		   | None -> (nodemap, None)
		   | Some sc ->
		       let super = ClassMap.find sc cmap in
		       let (nm, sc) = add_interface_or_class super nodemap in
			 (nm, Some (to_class_node sc))
		) in
	      let interfaces =
		List.map (fun iname ->
			    ClassMap.find iname cmap) c.Javalib.c_interfaces in
	      let (nodemap, interfaces_nodes) =
		List.fold_right
		  (fun i (nm,im) ->
		     let (nm,node) = add_interface_or_class i nm in
		       (nm,
			ClassMap.add (Javalib.get_name i)
			  (to_interface_node node) im)
		  ) interfaces (nodemap, ClassMap.empty) in
	      let node_info = make_class_node c super_node interfaces_nodes in
	      let node = Class node_info in
		(ClassMap.add c.c_name node nodemap, node)
	  | JInterface i ->
	      let (nodemap, super_node) =
		let (nm, o) =
		  add_interface_or_class cobject nodemap in
		  (nm, to_class_node o) in
	      let interfaces =
		List.map (fun iname ->
			    ClassMap.find iname cmap) i.Javalib.i_interfaces in
	      let (nodemap, interfaces_nodes) =
		List.fold_right
		  (fun i (nm,im) ->
		     let (nm,node) = add_interface_or_class i nm in
		       (nm,
			ClassMap.add (Javalib.get_name i)
			  (to_interface_node node) im)
		  ) interfaces (nodemap, ClassMap.empty) in
	      let node_info = make_interface_node i super_node interfaces_nodes in
	      let node = Interface node_info in
		(ClassMap.add i.i_name node nodemap, node)
  in fst (add_interface_or_class c nodemap)


let build_hierarchy 
    (cmap : 'a interface_or_class ClassMap.t) 
    : 'a node ClassMap.t =
  ClassMap.fold
    (fun _ c m ->
       add_interface_or_class cmap c m
    ) cmap ClassMap.empty

let map_program_classes map_iorc_context f classes =
  let jcmap = 
    ClassMap.map
      (fun node -> 
	 map_iorc_context (f node) (to_ioc node))
      classes
  in
    build_hierarchy jcmap

let map_program2' map_program_classes f fpp p =
  let classes = map_program_classes f p.classes in
  let slm = 
    match fpp with
	None -> p.static_lookup_method
      | Some fpp -> 
	  (fun cn ms pp -> 
	     try
	       match get_method (ClassMap.find cn classes) ms with
		   ConcreteMethod cm -> 
		     (match cm.cm_implementation with
			  Java laz -> 
			    p.static_lookup_method
			      cn
			      ms
			      (fpp (Lazy.force laz) pp)
			| Native -> ClassMethodSet.empty
		     )
		 | AbstractMethod _ -> ClassMethodSet.empty
		     
		     
	     with _ -> ClassMethodSet.empty
	  )
  in
    { classes = classes;
      parsed_methods =
	ClassMethodMap.map
	  (fun (node,cm) ->
	     let cn = get_name node in
	     let ms = cm.cm_signature in
	     let node = ClassMap.find cn classes in
	     let m = get_method node ms in
	       match m with
		 | AbstractMethod _ -> assert false
		 | ConcreteMethod cm -> (node,cm)
	  ) p.parsed_methods;
      static_lookup_method = slm	  
    }

let map_program2 f fpp p = 
  map_program2' (map_program_classes map_interface_or_class_context) f fpp p

let map_program f =
  map_program2
    (fun node cm -> f (get_name node) cm.cm_signature)

let map_program_with_native2 f fpp p = 
  map_program2' 
    (map_program_classes map_interface_or_class_with_native_context) f fpp p

let map_program_with_native f =
  map_program_with_native2
    (fun node cm -> f (get_name node) cm.cm_signature)
