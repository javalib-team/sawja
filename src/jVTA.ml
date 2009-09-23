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

open JBasics
open JOpcodes
open Javalib
open JProgram
  
type vta_concrete_method =
    { c_method : jvm_opcodes concrete_method;
      mutable c_has_been_parsed : bool;
      mutable vta_instantiated_classes : jvm_opcodes class_node ClassMap.t;
      mutable virtual_calls : (jvm_opcodes class_node * method_signature) Ptmap.t;
      mutable interface_calls : (jvm_opcodes interface_node * method_signature) Ptmap.t;
      mutable static_lookup : (jvm_opcodes class_node
			       * jvm_opcodes concrete_method) ClassMethodMap.t Ptmap.t;
      mutable vta_instantiated_subclasses : jvm_opcodes class_node ClassMap.t ClassMap.t;
      mutable vta_implemented_interfaces : jvm_opcodes class_node ClassMap.t ClassMap.t
    }
      
type vta_program =
    { p : jvm_opcodes program;
      rta_instantiated_classes : jvm_opcodes class_node ClassMap.t;
      mutable methods : vta_concrete_method ClassMethodMap.t;
      workset : (jvm_opcodes node * vta_concrete_method) Dllist.dllist;
      mutable pvta_parsed_methods : (jvm_opcodes node *
				       jvm_opcodes concrete_method) ClassMethodMap.t;
      mutable rta_instantiated_subclasses : jvm_opcodes class_node ClassMap.t ClassMap.t;
      mutable rta_implemented_interfaces : jvm_opcodes class_node ClassMap.t ClassMap.t
    }
      
let filter_subclasses sc classes =
  ClassMap.fold
    (fun cs c cmap ->
       if (extends_class c sc) then
	 ClassMap.add cs c cmap
       else cmap
    ) classes ClassMap.empty
    
let get_rta_instantiated_subclasses pvta c =
  let cs = c.c_info.c_name in
    try
      ClassMap.find cs pvta.rta_instantiated_subclasses
    with _ ->
      let classes =
	filter_subclasses c pvta.rta_instantiated_classes in
	pvta.rta_instantiated_subclasses <-
	  ClassMap.add cs classes pvta.rta_instantiated_subclasses;
	classes
	  
let get_vta_instantiated_subclasses m c =
  let cs = c.c_info.c_name in
    try
      ClassMap.find cs m.vta_instantiated_subclasses
    with _ ->
      let classes =
	filter_subclasses c m.vta_instantiated_classes in
	m.vta_instantiated_subclasses <-
	  ClassMap.add cs classes m.vta_instantiated_subclasses;
	classes
	  
let get_vta_method pvta cm =
  let cms = cm.cm_class_method_signature in
    try
      ClassMethodMap.find cms pvta.methods
    with _ ->
      let m = { c_method = cm;
		c_has_been_parsed = false;
		vta_instantiated_classes = ClassMap.empty;
		virtual_calls = Ptmap.empty;
		interface_calls = Ptmap.empty;
		static_lookup = Ptmap.empty;
		vta_instantiated_subclasses = ClassMap.empty;
		vta_implemented_interfaces = ClassMap.empty
	      } in
	pvta.methods <- ClassMethodMap.add cms m pvta.methods;
	m
	  
let filter_classes_implementing_interface i classes =
  ClassMap.fold
    (fun cs c cmap ->
       if (JControlFlow.implements_interface_or_subinterface_transitively c i)
       then
	 ClassMap.add cs c cmap
       else cmap
    ) classes ClassMap.empty
    
let get_rta_implemented_interfaces pvta i =
  let cs = i.i_info.i_name in
    try
      ClassMap.find cs pvta.rta_implemented_interfaces
    with _ ->
      let classes =
	filter_classes_implementing_interface i pvta.rta_instantiated_classes in
	pvta.rta_implemented_interfaces <-
	  ClassMap.add cs classes pvta.rta_implemented_interfaces;
	classes
	  
let get_vta_implemented_interfaces m i =
  let cs = i.i_info.i_name in
    try
      ClassMap.find cs m.vta_implemented_interfaces
    with _ ->
      let classes =
	filter_classes_implementing_interface i m.vta_instantiated_classes in
	m.vta_implemented_interfaces <-
	  ClassMap.add cs classes m.vta_implemented_interfaces;
	classes
	  
let update_invoke_virtual pvta m =
  Ptmap.iter
    (fun pp (cc,cms) ->
       let instantiated_classes =
	 get_vta_instantiated_subclasses m cc in
       let rmmap = JControlFlow.invoke_virtual_lookup ~c:(Some cc)
	 cms instantiated_classes in
	 ClassMethodMap.iter
	   (fun _ (c,cm) ->
	      let mvta = get_vta_method pvta cm in
		if not(mvta.c_has_been_parsed) then
		  (mvta.c_has_been_parsed <- true;
		   Dllist.add (Class c,mvta) pvta.workset)
	   ) rmmap;
	 let mmap =
	   try Ptmap.find pp m.static_lookup
	   with _ -> ClassMethodMap.empty in
	   m.static_lookup <-
	     Ptmap.add pp (ClassMethodMap.merge
			     (fun x _ -> x) mmap rmmap) m.static_lookup
    ) m.virtual_calls
    
let update_invoke_interface pvta m =
  Ptmap.iter
    (fun pp (ci,cms) ->
       let instantiated_classes =
	 get_vta_implemented_interfaces m ci in
     let rmmap = JControlFlow.invoke_interface_lookup ~i:(Some ci)
       cms instantiated_classes in
       ClassMethodMap.iter
	 (fun _ (c,cm) ->
	    let mvta = get_vta_method pvta cm in
	      if not(mvta.c_has_been_parsed) then
		(mvta.c_has_been_parsed <- true;
		 Dllist.add (Class c,mvta) pvta.workset)
	 ) rmmap;
       let mmap =
	 try Ptmap.find pp m.static_lookup
	 with _ -> ClassMethodMap.empty in
	 m.static_lookup <-
	   Ptmap.add pp (ClassMethodMap.merge
			   (fun x _ -> x) mmap rmmap) m.static_lookup
    ) m.interface_calls
    
let rec value_type2class_nodes pvta v =
  match v with
    | TBasic _ -> ClassMap.empty
    | TObject t ->
	match t with
	  | TArray a -> value_type2class_nodes pvta a
	  | TClass cn ->
	      try
		let ioc =
		  get_node pvta.p cn in
		  match ioc with
		    | Class c ->
			get_rta_instantiated_subclasses pvta c
		    | Interface i ->
			get_rta_implemented_interfaces pvta i
	      with Not_found ->
		(* Happen when an object is never instantiated and null
		   is always passed as parameter. *)
		ClassMap.empty
		    
let parse_invoke pvta m cms =
  let rt = ms_rtype cms in
  let instantiated_classes =
    match rt with
      | None -> ClassMap.empty
      | Some v -> value_type2class_nodes pvta v in
    m.vta_instantiated_classes <-
      ClassMap.merge
      (fun x _ -> x) instantiated_classes m.vta_instantiated_classes
      
let parse_invoke_virtual pvta m pp cc cms =
  parse_invoke pvta m cms;
  m.virtual_calls <-
    Ptmap.add pp (cc,cms) m.virtual_calls
    
let parse_invoke_interface pvta m pp ci cms =
  parse_invoke pvta m cms;
  m.interface_calls <-
    Ptmap.add pp (ci,cms) m.interface_calls
    
let add_clinit pvta ioc =
  try
    let clinit = get_method ioc clinit_signature in
      match clinit with
	| AbstractMethod _ -> assert false
	| ConcreteMethod cm ->
	    let mvta = get_vta_method pvta cm in
	      if not(mvta.c_has_been_parsed) then
		(mvta.c_has_been_parsed <- true;
		 Dllist.add (ioc,mvta) pvta.workset)
  with Not_found -> ()
    
let rec add_class_clinits pvta c =
  add_clinit pvta (Class c);
  match c.c_super with
    | None -> ()
    | Some sc -> add_class_clinits pvta sc
	
let parse_new m c =
  m.vta_instantiated_classes <-
    ClassMap.add c.c_info.c_name c m.vta_instantiated_classes
    
let parse_get pvta m fs =
  let instantiated_classes = value_type2class_nodes pvta fs in
    m.vta_instantiated_classes <-
      ClassMap.merge
      (fun x _ -> x) instantiated_classes m.vta_instantiated_classes
      
let parse_get_put_static pvta ioc fs =
  let rioc_list = JControlFlow.resolve_field fs ioc in
    List.iter
      (fun rioc ->
	 (match rioc with
	    | Class rc -> add_class_clinits pvta rc
	    | Interface _ -> add_clinit pvta rioc
	 )
      ) rioc_list
      
let parse_invoke_static pvta m pp cc cms =
  parse_invoke pvta m cms;
  let (rc,cm) = JControlFlow.invoke_static_lookup cc cms in
    m.static_lookup <-
      Ptmap.add pp (ClassMethodMap.add cm.cm_class_method_signature (rc,cm)
		      ClassMethodMap.empty) m.static_lookup;
    let mvta = get_vta_method pvta cm in
      if not(mvta.c_has_been_parsed) then
	(mvta.c_has_been_parsed <- true;
	 Dllist.add (Class rc, mvta) pvta.workset);
      rc
	
let parse_invoke_special pvta m pp ioc cc cms =
  parse_invoke pvta m cms;
  let (rc,cm) = JControlFlow.invoke_special_lookup ioc cc cms in
    m.static_lookup <-
      Ptmap.add pp (ClassMethodMap.add cm.cm_class_method_signature (rc,cm)
		      ClassMethodMap.empty) m.static_lookup;
    let mvta = get_vta_method pvta cm in
      if not(mvta.c_has_been_parsed) then
	(mvta.c_has_been_parsed <- true;
	 Dllist.add (Class rc, mvta) pvta.workset)
	  
let parse_method_parameters pvta m prms =
  List.iter
    (fun v ->
       let classes =
	 value_type2class_nodes pvta v in
	 m.vta_instantiated_classes <-
	   ClassMap.merge
	   (fun x _ -> x) classes m.vta_instantiated_classes
    ) prms
    
let parse_vta_method pvta ioc m =
  pvta.pvta_parsed_methods <-
    ClassMethodMap.add m.c_method.cm_class_method_signature
    (ioc, m.c_method) pvta.pvta_parsed_methods;
  parse_method_parameters pvta m (ms_args m.c_method.cm_signature);
  match m.c_method.cm_implementation with
    | Native -> ()
    | Java code ->
	let opcodes = (Lazy.force code).c_code in
	  Array.iteri
	    (fun pp op ->
	       match op with
		 | OpNew cs ->
		     let c =
		       let ioc =
			 ClassMap.find cs pvta.p.classes in
			 (match ioc with
			    | Interface _ -> failwith "Impossible New."
			    | Class c -> c
			 ) in
		       parse_new m c;
		       add_class_clinits pvta c
		 | OpConst (`Class _) ->
		     let cs = make_cn "java.lang.Class" in
		     let c =
		       let ioc =
			 ClassMap.find cs pvta.p.classes in
			 (match ioc with
			    | Interface _ -> failwith "Impossible ldc."
			    | Class c -> c
			 ) in
		       parse_new m c;
		       add_class_clinits pvta c		       
		 | OpGetField (_,fs) ->
		     parse_get pvta m (fs_type fs)
		 | OpGetStatic (cs,fs) ->
		     parse_get pvta m (fs_type fs);
		     let ioc =
		       ClassMap.find cs pvta.p.classes in
		       parse_get_put_static pvta ioc fs
		 | OpPutStatic (cs,fs) ->
		     let ioc =
		       ClassMap.find cs pvta.p.classes in
		       parse_get_put_static pvta ioc fs
		 | OpInvoke (`Static ccs, cms) ->
		     let cc =
		       let iocc =
			 ClassMap.find ccs pvta.p.classes in
			 (match iocc with
			    | Interface _ -> failwith "Impossible InvokeStatic."
			    | Class c -> c
			 ) in
		     let rc = parse_invoke_static pvta m pp cc cms in
		       add_class_clinits pvta rc
		 | OpInvoke (`Special ccs, cms) ->
		     let cc =
		       let iocc =
		 	 ClassMap.find ccs pvta.p.classes in
		 	 (match iocc with
		 	    | Interface _ -> failwith "Impossible InvokeSpecial."
		 	    | Class c -> c
		 	 ) in
		       parse_invoke_special pvta m pp ioc cc cms
		 | OpInvoke (`Virtual (TClass ccs), cms) ->
		     let cc =
		       let iocc =
			 ClassMap.find ccs pvta.p.classes in
			 (match iocc with
			    | Interface _ -> failwith "Impossible InvokeVirtual."
			    | Class c -> c
			 ) in
		       parse_invoke_virtual pvta m pp cc cms
		 | OpInvoke (`Virtual (TArray _), cms) ->
		     (* should only happen with clone(). *)
		     let cc =
		       let iocc =
			 ClassMap.find java_lang_object pvta.p.classes in
			 (match iocc with
			    | Interface _ -> failwith "Impossible InvokeVirtual."
			    | Class o -> o
			 ) in
		       parse_invoke_virtual pvta m pp cc cms
		 | OpInvoke (`Interface ccs, cms) ->
		     let ci =
		       let iocc =
			 ClassMap.find ccs pvta.p.classes in
			 (match iocc with
			    | Interface i -> i
			    | Class _ -> failwith "Impossible InvokeInterface."
			 ) in
		       parse_invoke_interface pvta m pp ci cms
		 | _ -> ()
	    ) opcodes;
	  update_invoke_virtual pvta m;
	  update_invoke_interface pvta m
	    
let iter_workset pvta =
  let tail = Dllist.tail pvta.workset in
    Dllist.iter_to_head
      (fun (ioc,mvta) -> parse_vta_method pvta ioc mvta) tail
      
let parse_program_from_rta prta instantiated_classes csms =
  let pvta = { p = prta;
	       rta_instantiated_classes = instantiated_classes;
	       methods = ClassMethodMap.empty;
	       workset = Dllist.create();
	       pvta_parsed_methods = ClassMethodMap.empty;
	       rta_instantiated_subclasses = ClassMap.empty;
	       rta_implemented_interfaces = ClassMap.empty
	     } in
  let (cs,ms) = csms in
  let ioc = get_node prta cs in
    (match ioc with
       | Interface _ -> failwith "An entry point can't be an interface method."
       | Class c ->
	   let m = get_method ioc ms in
	     (match m with
		| AbstractMethod _ -> failwith "An entry point can't be an abstract method."
		| ConcreteMethod cm ->
		    let mvta = get_vta_method pvta cm in
		      Dllist.add (Class c,mvta) pvta.workset
	     )
    );
    iter_workset pvta;
    let static_lookup_map =
      ClassMethodMap.map (fun mvta -> mvta.static_lookup) pvta.methods in
      { prta with parsed_methods = pvta.pvta_parsed_methods;
	  static_lookup_method =
	  (fun cs ms pp ->
       	     Ptmap.find pp
       	       (ClassMethodMap.find (make_cms cs ms)
       		  static_lookup_map))
      }
	
let default_entrypoints = JRTA.default_entrypoints

let parse_program ?(other_entrypoints=default_entrypoints) classpath csms =
  let (prta, instantiated_classes) =
    JRTA.parse_program ~other_entrypoints:other_entrypoints classpath csms in
  let pvta = parse_program_from_rta prta instantiated_classes csms in
    (pvta, instantiated_classes)
