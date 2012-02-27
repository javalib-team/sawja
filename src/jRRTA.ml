(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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
open JProgram

type vta_concrete_method =
    { c_method : jcode concrete_method;
      mutable c_has_been_parsed : bool;
      mutable vta_instantiated_classes : jcode class_node ClassMap.t;
      mutable virtual_calls : (jcode class_node * method_signature) ClassMethodMap.t;
      mutable interface_calls : (jcode interface_node * method_signature) ClassMethodMap.t;
      mutable static_lookup_virtual : ClassMethodSet.t ClassMethodMap.t;
      mutable static_lookup_interface : ClassMethodSet.t ClassMethodMap.t;
      mutable static_lookup_static : jcode concrete_method ClassMethodMap.t;
      mutable static_lookup_special : jcode concrete_method ClassMethodMap.t;
      mutable vta_instantiated_subclasses : jcode class_node ClassMap.t ClassMap.t;
      mutable vta_implemented_interfaces : jcode class_node ClassMap.t ClassMap.t
    }

type vta_program =
    { p : jcode program;
      rta_instantiated_classes : jcode class_node ClassMap.t;
      mutable methods : vta_concrete_method ClassMethodMap.t;
      workset : (jcode node * vta_concrete_method) Wlist.wlist;
      mutable pvta_parsed_methods : (jcode node *
				       jcode concrete_method) ClassMethodMap.t;
      mutable rta_instantiated_subclasses : jcode class_node ClassMap.t ClassMap.t;
      mutable rta_implemented_interfaces : jcode class_node ClassMap.t ClassMap.t
    }

let filter_subclasses sc classes =
  ClassMap.filter (fun c -> extends_class c sc) classes

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
		virtual_calls = ClassMethodMap.empty;
		interface_calls = ClassMethodMap.empty;
		static_lookup_virtual = ClassMethodMap.empty;
		static_lookup_interface = ClassMethodMap.empty;
		static_lookup_static = ClassMethodMap.empty;
		static_lookup_special = ClassMethodMap.empty;
		vta_instantiated_subclasses = ClassMap.empty;
		vta_implemented_interfaces = ClassMap.empty
	      } in
	pvta.methods <- ClassMethodMap.add cms m pvta.methods;
	m

let filter_classes_implementing_interface i classes =
  ClassMap.filter
    (fun c ->
       JControlFlow.implements_interface_or_subinterface_transitively c i
    ) classes

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
  ClassMethodMap.iter
    (fun ccms (cc,cms) ->
       let instantiated_classes =
	 get_vta_instantiated_subclasses m cc in
       let rmmap = JControlFlow.invoke_virtual_lookup ~c:(Some cc)
	 cms instantiated_classes in
	 ClassMethodMap.iter
	   (fun _ (c,cm) ->
	      let mvta = get_vta_method pvta cm in
		if not(mvta.c_has_been_parsed) then
		  (mvta.c_has_been_parsed <- true;
		   Wlist.add (Class c,mvta) pvta.workset)
	   ) rmmap;
	 let rset = ClassMethodMaptoSet.to_set rmmap in
	 let mset =
	   try ClassMethodMap.find ccms m.static_lookup_virtual
	   with _ -> ClassMethodSet.empty in
	   m.static_lookup_virtual <-
	     ClassMethodMap.add ccms
	     (ClassMethodSet.union mset rset) m.static_lookup_virtual
    ) m.virtual_calls

let update_invoke_interface pvta m =
  ClassMethodMap.iter
    (fun ccms (ci,cms) ->
       let instantiated_classes =
	 get_vta_implemented_interfaces m ci in
       let rmmap = JControlFlow.invoke_interface_lookup ~i:(Some ci)
	 cms instantiated_classes in
	 ClassMethodMap.iter
	   (fun _ (c,cm) ->
	      let mvta = get_vta_method pvta cm in
		if not(mvta.c_has_been_parsed) then
		  (mvta.c_has_been_parsed <- true;
		   Wlist.add (Class c,mvta) pvta.workset)
	   ) rmmap;
	 let rset = ClassMethodMaptoSet.to_set rmmap in
	 let mset =
	   try ClassMethodMap.find ccms m.static_lookup_interface
	   with _ -> ClassMethodSet.empty in
	   m.static_lookup_interface <-
	     ClassMethodMap.add ccms
	     (ClassMethodSet.union mset rset) m.static_lookup_interface
    ) m.interface_calls

let node2instanciated_class_nodes pvta ioc = 
  match ioc with
    | Class c ->
	get_rta_instantiated_subclasses pvta c
    | Interface i ->
	get_rta_implemented_interfaces pvta i

let rec value_type2class_nodes pvta v =
  match v with
    | TBasic _ -> ClassMap.empty
    | TObject t ->
	match t with
	  | TArray a -> 
	      let c_object =
		let iocc =
		  ClassMap.find java_lang_object pvta.p.classes in
		  (match iocc with
		     | Interface _ -> failwith "Impossible InvokeVirtual."
		     | Class o -> o
		  ) in
	      ClassMap.add 
		java_lang_object 
		c_object 
		(value_type2class_nodes pvta a)
	  | TClass cn ->
	      try
		let ioc = get_node pvta.p cn in
		  node2instanciated_class_nodes pvta ioc		  
	      with Not_found -> assert false

let parse_invoke pvta m cms =
  let rt = ms_rtype cms in
  let instantiated_classes =
    match rt with
      | None -> ClassMap.empty
      | Some v -> value_type2class_nodes pvta v in
    m.vta_instantiated_classes <-
      ClassMap.merge
      (fun x _ -> x) instantiated_classes m.vta_instantiated_classes

let parse_invoke_virtual pvta m cc cms =
  parse_invoke pvta m cms;
  let ccms = make_cms (cc.c_info.c_name) cms in
    m.virtual_calls <-
      ClassMethodMap.add ccms (cc,cms) m.virtual_calls

let parse_invoke_interface pvta m ci cms =
  parse_invoke pvta m cms;
  let ccms = make_cms (ci.i_info.i_name) cms in
    m.interface_calls <-
      ClassMethodMap.add ccms (ci,cms) m.interface_calls

let add_clinit pvta ioc =
  try
    let clinit = get_method ioc clinit_signature in
      match clinit with
	| AbstractMethod _ -> assert false
	| ConcreteMethod cm ->
	    let mvta = get_vta_method pvta cm in
	      if not(mvta.c_has_been_parsed) then
		(mvta.c_has_been_parsed <- true;
		 Wlist.add (ioc,mvta) pvta.workset)
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

let parse_invoke_static pvta m cc cms =
  parse_invoke pvta m cms;
  let ccms = make_cms cc.c_info.c_name cms in
  let (rc,cm) = JControlFlow.invoke_static_lookup cc cms in
    m.static_lookup_static <-
      ClassMethodMap.add ccms cm m.static_lookup_static;
    let mvta = get_vta_method pvta cm in
      if not(mvta.c_has_been_parsed) then
	(mvta.c_has_been_parsed <- true;
	 Wlist.add (Class rc, mvta) pvta.workset);
      rc

let parse_invoke_special pvta m ioc cc cms =
  parse_invoke pvta m cms;
  let ccms = make_cms cc.c_info.c_name cms in
  let (rc,cm) = JControlFlow.invoke_special_lookup ioc cc cms in
    m.static_lookup_special <-
      ClassMethodMap.add ccms cm m.static_lookup_special;
    let mvta = get_vta_method pvta cm in
      if not(mvta.c_has_been_parsed) then
	(mvta.c_has_been_parsed <- true;
	 Wlist.add (Class rc, mvta) pvta.workset)

let parse_method_parameters pvta m prms =
  List.iter
    (fun v ->
       let classes =
	 value_type2class_nodes pvta v in
	 m.vta_instantiated_classes <-
	   ClassMap.merge
	   (fun x _ -> x) classes m.vta_instantiated_classes
    ) prms

let parse_method_receiver pvta ioc m = 
  if m.c_method.cm_static = false
  then
    let classes = node2instanciated_class_nodes pvta ioc in
      m.vta_instantiated_classes <-
	ClassMap.merge
	(fun x _ -> x) classes m.vta_instantiated_classes

let parse_method_handlers pvta m = 
  match m.c_method.cm_implementation with
      Native -> ()
    | Java code -> 
	let exc_tbl = (Lazy.force code).c_exc_tbl in
	  List.iter
	    (fun e_h -> 
	       let classes = 
		 match e_h.e_catch_type with 
		     None -> 
		       value_type2class_nodes pvta 
			 (TObject (TClass (make_cn "java.lang.Throwable")))
		   | Some cn -> 
		       value_type2class_nodes pvta (TObject (TClass cn))
	       in     
		 m.vta_instantiated_classes <-
		   ClassMap.merge
		   (fun x _ -> x) classes m.vta_instantiated_classes)
	    exc_tbl


let parse_vta_method pvta ioc m =
  pvta.pvta_parsed_methods <-
    ClassMethodMap.add m.c_method.cm_class_method_signature
    (ioc, m.c_method) pvta.pvta_parsed_methods;
  parse_method_parameters pvta m (ms_args m.c_method.cm_signature);
  parse_method_receiver pvta ioc m;
  parse_method_handlers pvta m;
  match m.c_method.cm_implementation with
    | Native -> ()
    | Java code ->
	let opcodes = (Lazy.force code).c_code in
	  Array.iteri
	    (fun _ op ->
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
		 | OpConst (`String _) ->
		     let cs = make_cn "java.lang.String" in
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
		     let rc = parse_invoke_static pvta m cc cms in
		       add_class_clinits pvta rc
		 | OpInvoke (`Special ccs, cms) ->
		     let cc =
		       let iocc =
		 	 ClassMap.find ccs pvta.p.classes in
		 	 (match iocc with
		 	    | Interface _ -> failwith "Impossible InvokeSpecial."
		 	    | Class c -> c
		 	 ) in
		       parse_invoke_special pvta m ioc cc cms
		 | OpInvoke (`Virtual (TClass ccs), cms) ->
		     let cc =
		       let iocc =
			 ClassMap.find ccs pvta.p.classes in
			 (match iocc with
			    | Interface _ -> failwith "Impossible InvokeVirtual."
			    | Class c -> c
			 ) in
		       parse_invoke_virtual pvta m cc cms
		 | OpInvoke (`Virtual (TArray _), cms) ->
		     (* should only happen with clone(). *)
		     let cc =
		       let iocc =
			 ClassMap.find java_lang_object pvta.p.classes in
			 (match iocc with
			    | Interface _ -> failwith "Impossible InvokeVirtual."
			    | Class o -> o
			 ) in
		       parse_invoke_virtual pvta m cc cms
		 | OpInvoke (`Interface ccs, cms) ->
		     let ci =
		       let iocc =
			 ClassMap.find ccs pvta.p.classes in
			 (match iocc with
			    | Interface i -> i
			    | Class _ -> failwith "Impossible InvokeInterface."
			 ) in
		       parse_invoke_interface pvta m ci cms
		 | _ -> ()
	    ) opcodes;
	  update_invoke_virtual pvta m;
	  update_invoke_interface pvta m

let iter_workset pvta =
  let tail = Wlist.tail pvta.workset in
    Wlist.iter_to_head
      (fun (ioc,mvta) -> parse_vta_method pvta ioc mvta) tail

let static_lookup_method pvta =
  let virtual_lookup_map =
    ClassMethodMap.map
      (fun mvta -> mvta.static_lookup_virtual) pvta.methods in
  let interface_lookup_map =
    ClassMethodMap.map
      (fun mvta -> mvta.static_lookup_interface) pvta.methods in
  let special_lookup_map =
    ClassMethodMap.map
      (fun mvta -> mvta.static_lookup_special) pvta.methods in
  let static_lookup_map =
    ClassMethodMap.map
      (fun mvta -> mvta.static_lookup_static) pvta.methods in
  let concrete_methods =
    ClassMethodMap.map
      (fun mvta -> mvta.c_method) pvta.methods in
    fun cs ms pp ->
      let cm = ClassMethodMap.find (make_cms cs ms) concrete_methods in
	match cm.cm_implementation with
	  | Native -> failwith "Can't call static_lookup on Native methods"
	  | Java code ->
	      let opcode = (Lazy.force code).c_code.(pp) in
		(match opcode with
		   | OpInvoke (`Interface ccs,cms) ->
		       let cms = make_cms cs ms
		       and ccms = make_cms ccs cms in
		       let cmmap = ClassMethodMap.find cms interface_lookup_map in
			 ClassMethodMap.find ccms cmmap
		   | OpInvoke (`Virtual (TClass ccs),cms) ->
		       let cms = make_cms cs ms
		       and ccms = make_cms ccs cms in
		       let cmmap = ClassMethodMap.find cms virtual_lookup_map in
			 ClassMethodMap.find ccms cmmap
		   | OpInvoke (`Virtual (TArray _),cms) ->
		       (* should only happen with [clone()] *)
		       let cms = make_cms cs ms
		       and ccms = make_cms (JBasics.java_lang_object) cms in
		       let cmmap = ClassMethodMap.find cms virtual_lookup_map in
			 ClassMethodMap.find ccms cmmap
		   | OpInvoke (`Static ccs,cms) ->
		       let cms = make_cms cs ms
		       and ccms = make_cms ccs cms in
		       let cmmap = ClassMethodMap.find cms static_lookup_map in
		       let cm = ClassMethodMap.find ccms cmmap in
			 ClassMethodSet.add cm.cm_class_method_signature
			   ClassMethodSet.empty
		   | OpInvoke (`Special ccs,cms) ->
		       let cms = make_cms cs ms
		       and ccms = make_cms ccs cms in
		       let cmmap = ClassMethodMap.find cms special_lookup_map in
		       let cm = ClassMethodMap.find ccms cmmap in
			 ClassMethodSet.add cm.cm_class_method_signature
			   ClassMethodSet.empty
		   | _ -> raise Not_found
		)

(* TODO: an entry point can be in an interface if the method is a clinit method *)
let parse_program_from_rta prta instantiated_classes cmsl =
  let pvta = { p = prta;
	       rta_instantiated_classes = instantiated_classes;
	       methods = ClassMethodMap.empty;
	       workset = Wlist.create();
	       pvta_parsed_methods = ClassMethodMap.empty;
	       rta_instantiated_subclasses = ClassMap.empty;
	       rta_implemented_interfaces = ClassMap.empty
	     }
  in
    List.iter
      (fun cms ->
         let (cs,ms) = cms_split cms in
         let ioc = get_node prta cs in
           (match ioc with
              | Interface _ -> failwith "An entry point can't be an interface method."
              | Class c ->
                  try
	            let m =
                      get_method ioc ms
                    in
	              (match m with
		         | AbstractMethod _ ->
                             failwith "An entry point can't be an abstract method."
		         | ConcreteMethod cm ->
		             let mvta = get_vta_method pvta cm in
		               Wlist.add (Class c,mvta) pvta.workset
	              )
                  with Not_found ->
                    if ms_compare ms clinit_signature <> 0
                    then
                      failwith ("The method entry point "
                                ^ JPrint.method_signature ms
                                ^ " cannot be found in "
                                ^ JPrint.class_name cs)
           ))
      cmsl;
    iter_workset pvta;
    { prta with parsed_methods = pvta.pvta_parsed_methods;
	static_lookup_method = static_lookup_method pvta
    }

let default_entrypoints = JRTA.default_entrypoints

let cldc11_default_entrypoints = JRTA.cldc11_default_entrypoints

let default_native_throwable = JRTA.default_native_throwable

let default_instantiated = (make_cn "java.lang.Class")::(make_cn "java.lang.String")::default_native_throwable

let parse_program ?(instantiated=default_instantiated) ?(other_entrypoints=default_entrypoints) classpath cms =
  let (prta, instantiated_classes) =
    JRTA.parse_program
      ~instantiated:instantiated ~other_entrypoints:other_entrypoints classpath cms
  in
  let pvta =
    parse_program_from_rta prta instantiated_classes (cms::other_entrypoints) in
    (pvta, instantiated_classes)
