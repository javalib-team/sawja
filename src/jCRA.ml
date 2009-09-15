(*
 * This file is part of JavaLib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
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
open JClass
open JClassIndexation
open JProgram

type class_info = { class_data : JOpcodes.lazy_code JProgram.interface_or_class;
		    children_classes : (JOpcodes.lazy_code class_file list) ref;
		    children_interfaces : (JOpcodes.lazy_code interface_file list) ref
		  }

let rec update_interfaces classes_map ioc interfaces cs =
  match ioc with
    | `Class c ->
	ClassMap.fold
	  (fun i_sig i interfaces ->
	     let s =
	       try ClassMap.find i_sig interfaces
	       with _ -> ClassSet.empty in
	     let interfaces =
	       ClassMap.add i_sig (ClassSet.add cs s) interfaces in
	       update_interfaces classes_map (`Interface i) interfaces cs
	  )
	  c.c_interfaces interfaces
    | `Interface i ->
	ClassMap.fold
	  (fun i_sig i interfaces ->
	     let s =
	       try ClassMap.find i_sig interfaces
	       with _ -> ClassSet.empty in
	     let interfaces =
	       ClassMap.add i_sig (ClassSet.add cs s) interfaces in
	       update_interfaces classes_map (`Interface i) interfaces cs
	  )
	  i.i_interfaces interfaces

exception Class_not_found of class_signature

let add_classFile c classes_map interfaces =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let isig = make_class_signature iname in
	 let i =
	   try
	     match (ClassMap.find isig classes_map).class_data with
	       | `Interface i -> i
	       | `Class _ ->
		   raise (Class_structure_error
			    (JDumpBasics.class_name
			       (class_signature2class_name c.c_signature)
			     ^" is declared to implements "
			     ^JDumpBasics.class_name iname
			     ^", which is a class and not an interface."))
	   with Not_found -> raise (Class_not_found isig)
	 in ClassMap.add isig i imap
      ) ClassMap.empty c.JClass.c_interfaces in
  let c_super =
    match c.c_super_class with
      | None -> None
      | Some super ->
	  let super_signature = make_class_signature super in
	    try
	      let c_super_info = ClassMap.find super_signature classes_map in
		match c_super_info.class_data with
		  | `Class c -> Some c
		  | `Interface _ ->
		      raise (Class_structure_error
			       (JDumpBasics.class_name
				  (class_signature2class_name c.c_signature)
				^" is declared to extends "
				^JDumpBasics.class_name super
				^", which is an interface and not a class.")
			    )
	    with Not_found -> raise (Class_not_found super_signature)
  in
  let children_classes = ref [] in
  let cfile =
    {c_info = c;
     c_super = c_super;
     c_interfaces = imap;
     get_c_children = (fun () -> !children_classes)
    }
  in
    ClassMap.iter
      (fun _ i ->
	 let i_info = (ClassMap.find i.i_info.i_signature classes_map) in
	   i_info.children_classes := cfile :: !(i_info.children_classes)
      ) cfile.c_interfaces;
    begin
      match super_class (`Class cfile) with
	| None -> ();
	| Some parent ->
	    let c_super_info = ClassMap.find parent.c_info.c_signature
	      classes_map in
	      c_super_info.children_classes :=
		cfile :: !(c_super_info.children_classes)
    end;
    let rec c_info = { class_data = (`Class cfile);
		       children_classes = children_classes;
		       children_interfaces = ref [] } in
      interfaces := update_interfaces classes_map c_info.class_data
	!interfaces c.c_signature;
      ClassMap.add c.c_signature c_info classes_map

let add_interfaceFile c classes_map =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let isig = make_class_signature iname in
	 let i =
	   try
	     match (ClassMap.find isig classes_map).class_data with
	       | `Interface i -> i
	       | `Class c' ->
		   raise (Class_structure_error
			    ("Interface "^JDumpBasics.class_name
			       (class_signature2class_name c.i_signature)
			     ^" is declared to extends "
			     ^JDumpBasics.class_name
			       (class_signature2class_name c'.c_info.c_signature)
			     ^", which is an interface and not a class.")
			 )
	   with Not_found -> raise (Class_not_found isig)
	 in ClassMap.add isig i imap
      ) ClassMap.empty c.JClass.i_interfaces
  and super =
    try
      match (ClassMap.find java_lang_object_signature
	       classes_map).class_data with
	| `Class c -> c
	| `Interface _ ->
	    raise (Class_structure_error"java.lang.Object is declared as an interface.")
    with Not_found -> raise (Class_not_found java_lang_object_signature)
  in
  let children_classes = ref [] in
  let children_interfaces = ref [] in
  let cfile =
    {i_info = c;
     i_super = super;
     i_interfaces = imap;
     get_i_children_interfaces = (fun () -> !children_interfaces);
     get_i_children_classes = (fun () -> !children_classes)
    }
  in
    ClassMap.iter
      (fun _ i ->
	 let i_info = ClassMap.find i.i_info.i_signature classes_map in
	   i_info.children_interfaces := cfile :: !(i_info.children_interfaces))
      cfile.i_interfaces;
    let i_info = { class_data = (`Interface cfile);
		   children_classes = children_classes;
		   children_interfaces = children_interfaces } in
      ClassMap.add c.i_signature i_info classes_map

let add_one_file f classes_map interfaces =
  match f with
    | `Interface i -> add_interfaceFile i classes_map
    | `Class c -> add_classFile c classes_map interfaces

let add_class_referenced c classmap to_add =
  Array.iter
    (function
      | ConstMethod (TClass cn,_,_)
      | ConstInterfaceMethod (cn,_,_)
      | ConstField (cn,_,_)
      | ConstValue (ConstClass (TClass cn)) ->
	  let cs = make_class_signature cn in
	    if not (ClassMap.mem cs classmap) then to_add := cs::!to_add
      | _ -> ()) (JClass.get_consts c)

let get_class class_path jclasses_map cs =
  let cn = class_signature2class_name cs in
    try ClassMap.find cs !jclasses_map
    with Not_found ->
      try
	let c = JFile.get_class class_path (JDumpBasics.class_name cn)
	in
	  jclasses_map := ClassMap.add cs c !jclasses_map;
	  c;
      with No_class_found _ -> raise (Class_not_found cs)

let rec add_file class_path c classes_map interfaces =
  let jclasses_map = ref ClassMap.empty in
  let to_add = ref [] in
  let classes_map =
    try
      let c_signature = JClass.get_signature c in
	if not (ClassMap.mem c_signature classes_map)
	then
	  begin
	    add_class_referenced c !jclasses_map to_add;
	    add_one_file c classes_map interfaces
	  end
	else classes_map
    with Class_not_found cs ->
      let missing_class = get_class class_path jclasses_map cs in
	add_file class_path c
	  (add_file class_path missing_class classes_map interfaces) interfaces
  in begin
      let p_classes = ref classes_map in
	try while true do
	  let cs = List.hd !to_add in
	    to_add := List.tl !to_add;
	    if not (ClassMap.mem cs !p_classes) then
	      let c = get_class class_path jclasses_map cs
	      in p_classes :=
		   add_file class_path c !p_classes interfaces
	done;
	  !p_classes
	with Failure "hd" -> !p_classes
    end

let get_children_classes c children_classes =
  let cs = c.c_info.c_signature in
    try
      ClassMap.find cs !children_classes
    with _ ->
      let classes =
	List.fold_right
	  (fun c cmap ->
	     if not(c.c_info.c_abstract) then
	       ClassMap.add c.c_info.c_signature c cmap
	     else cmap
	  ) (c :: (get_all_children_classes c)) ClassMap.empty in
	    children_classes := ClassMap.add cs classes !children_classes;
	    classes

let static_virtual_lookup virtual_lookup_map children_classes c ms =
  let cs = c.c_info.c_signature in
    try
      ClassMethMap.find (cs,ms) !virtual_lookup_map
    with
      | _ ->
	  let instantiated_classes = get_children_classes c children_classes in
	  let cmmap =
	    JControlFlow.invoke_virtual_lookup ~c:(Some c) ms
	      instantiated_classes in
	    virtual_lookup_map :=
	      ClassMethMap.add (cs,ms) cmmap !virtual_lookup_map;
	    cmmap

let static_interface_lookup interface_lookup_map classes_map interfaces
    children_classes i ms =
  let cs = i.i_info.i_signature in
    try
      ClassMethMap.find (cs,ms) !interface_lookup_map
    with
      | _ ->
	  let equivalent_classes =
	    try ClassMap.find cs interfaces
	    with _ -> ClassSet.empty in
	  let instantiated_classes =
	    ClassSet.fold
	      (fun cs cmap ->
		 let ioc_info = ClassMap.find cs classes_map in
		 let ioc = ioc_info.class_data in
		   match ioc with
		     | `Interface _ -> assert false
		     | `Class c ->
			 let classes =
			   get_children_classes c children_classes in
			   ClassMap.merge (fun x _ -> x) cmap classes
	      ) equivalent_classes ClassMap.empty in
	  let cmmap =
	    JControlFlow.invoke_interface_lookup ~i:(Some i) ms
	      instantiated_classes in
	    interface_lookup_map :=
	      ClassMethMap.add (cs,ms) cmmap !interface_lookup_map;
	    cmmap
	      
let static_static_lookup static_lookup_map c ms =
  let cs = c.c_info.c_signature in
    try
      ClassMethMap.find (cs,ms) !static_lookup_map
    with
      | _ ->
	  let (rc,cm) = JControlFlow.invoke_static_lookup c ms in
	  let cmmap = ClassMethodMap.add cm.cm_class_method_signature (rc,cm)
	    ClassMethodMap.empty in
	    static_lookup_map :=
	      ClassMethMap.add (cs,ms) cmmap !static_lookup_map;
	    cmmap

let static_special_lookup special_lookup_map current_class c ms =
  let ccs = get_signature current_class in
  let cs = c.c_info.c_signature in
  let ccmmap =
    try
      ClassMap.find ccs !special_lookup_map
    with _ -> ClassMethMap.empty in
    try
      ClassMethMap.find (cs,ms) ccmmap
    with
      | _ ->
	  let (rc,cm) = JControlFlow.invoke_special_lookup current_class c ms in
	  let cmmap = ClassMethodMap.add cm.cm_class_method_signature (rc,cm)
	    ClassMethodMap.empty in
	    special_lookup_map := ClassMap.add ccs
	      (ClassMethMap.add (cs,ms) cmmap ccmmap) !special_lookup_map;
	    cmmap

let static_lookup_method classes_map interfaces virtual_lookup_map interface_lookup_map
    static_lookup_map special_lookup_map children_classes cs ms pp =
  let m = get_method (ClassMap.find cs classes_map).class_data ms in
    match m with
      | AbstractMethod _ -> failwith "Can't call static_lookup on Abstract Methods"
      | ConcreteMethod cm ->
	  (match cm.cm_implementation with
	     | Native -> failwith "Can't call static_lookup on Native methods"
	     | Java code ->
		 let c = (Lazy.force code).c_code in
		   try
		     let op = c.(pp) in
		       match op with
			 | OpInvoke(`Interface ccs, cms) ->
			     let cc =
			       let ioc =
			     	 (ClassMap.find ccs
			     	    classes_map).class_data in
			     	 match ioc with
			     	   | `Class _ ->
			     	       failwith "Impossible InvokeInterface"
			     	   | `Interface i -> i in
			       static_interface_lookup interface_lookup_map
			     	 classes_map interfaces children_classes cc cms
			 | OpInvoke (`Virtual (`TClass ccs), cms) ->
			     let cc =
			       let ioc =
			     	 (ClassMap.find ccs
			     	    classes_map).class_data in
			     	 match ioc with
			     	   | `Interface _ ->
			     	       failwith "Impossible InvokeVirtual"
			     	   | `Class c -> c in
			       static_virtual_lookup virtual_lookup_map
				 children_classes cc cms
			 | OpInvoke (`Virtual (`TArray _), cms) ->
			     (* should only happen with [clone()] *)
			     let cobj =
			       let ioc =
			     	 (ClassMap.find java_lang_object_signature
			     	    classes_map).class_data in
			     	 match ioc with
			     	   | `Interface _ ->
			     	       failwith "Impossible InvokeVirtual"
			     	   | `Class c -> c in
			       static_virtual_lookup virtual_lookup_map
				 children_classes cobj cms
			 | OpInvoke (`Static ccs, cms) ->
			     let cc =
			       let ioc =
				 (ClassMap.find ccs
				    classes_map).class_data in
				 match ioc with
				   | `Interface _ ->
				       failwith "Impossible InvokeStatic"
				   | `Class c -> c in
			       static_static_lookup static_lookup_map cc cms
			 | OpInvoke (`Special ccs, cms) ->
			     let cc =
			       let ioc =
				 (ClassMap.find ccs
				    classes_map).class_data in
				 match ioc with
				   | `Interface _ ->
				       failwith "Impossible InvokeSpecial"
				   | `Class c -> c in
			     let current_class =
			       (ClassMap.find cs classes_map).class_data in
			       static_special_lookup special_lookup_map
				 current_class cc cms
			 | _ ->
			     failwith "Invalid opcode found at specified program point"
		   with
		     | Not_found -> failwith "Invalid program point"
		     | e -> raise e
	  )

let default_classes =
  ["java.lang.Class"; "java.lang.System"; "java.lang.String"; "java.lang.Thread";
   "java.lang.ThreadGroup"; "java.lang.ref.Finalizer"; "java.lang.OutOfMemoryError";
   "java.lang.NullPointerException"; "java.lang.ArrayStoreException";
   "java.lang.ArithmeticException"; "java.lang.StackOverflowError";
   "java.lang.IllegalMonitorStateException"; "java.lang.Compiler";
   "java.lang.reflect.Method"; "java.lang.reflect.Field"]

let parse_program ?(other_classes=default_classes) class_path names =
  (* build a map of all the JClass.class_file that are going to be
     translated to build the new hierarchy.*)
  let (jars,others) =
    List.partition
      (fun f -> Filename.check_suffix f ".jar")
      (names @ other_classes) in
  let class_map =
    JFile.read
      class_path
      (fun cmap c ->
	 let c_signature = JClass.get_signature c in
	   ClassMap.add c_signature c cmap
      ) ClassMap.empty jars in
  let class_path = JFile.class_path class_path in
  let class_map = ref
    begin
      List.fold_left
	(fun clmap cn ->
	   let c = JFile.get_class class_path cn in
	   let c_signature = JClass.get_signature c in
	     ClassMap.add c_signature c clmap
	) class_map others
    end in
  let interfaces = ref ClassMap.empty in
  let p_classes =
    ClassMap.fold
      (fun _ c classes -> add_file class_path c classes interfaces)
      !class_map ClassMap.empty in
  let parsed_methods =
    ClassMap.fold
      (fun _ ioc_info cmmap ->
	 let ioc = ioc_info.class_data in
	   MethodMap.fold
	     (fun _ cm cmmap ->
		ClassMethodMap.add cm.cm_class_method_signature (ioc,cm) cmmap
	     ) (get_concrete_methods ioc) cmmap
      ) p_classes ClassMethodMap.empty in
    JFile.close_class_path class_path;
    let virtual_lookup_map = ref ClassMethMap.empty
    and interface_lookup_map = ref ClassMethMap.empty
    and static_lookup_map = ref ClassMethMap.empty
    and special_lookup_map = ref ClassMap.empty
    and children_classes = ref ClassMap.empty in
      { classes = ClassMap.map
	  (fun ioc_info ->
	     ioc_info.class_data
	  ) p_classes;
	parsed_methods = parsed_methods;
	static_lookup_method = static_lookup_method p_classes !interfaces
	  virtual_lookup_map interface_lookup_map static_lookup_map
	  special_lookup_map children_classes
      }

let parse_program_bench ?(other_classes=default_classes) class_path names =
  let time_start = Sys.time() in
    ignore(parse_program ~other_classes class_path names);
    let time_stop = Sys.time() in
      Printf.printf "program parsed in %fs.\n" (time_stop-.time_start)
