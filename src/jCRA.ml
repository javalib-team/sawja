(*
 * This file is part of SAWJA
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
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

let rec update_interfaces classes_map ioc interfaces cs =
  match ioc with
    | Class c ->
	ClassMap.fold
	  (fun i_sig i interfaces ->
	     let s =
	       try ClassMap.find i_sig interfaces
	       with _ -> ClassSet.empty in
	     let interfaces =
	       ClassMap.add i_sig (ClassSet.add cs s) interfaces in
	       update_interfaces classes_map (Interface i) interfaces cs
	  )
	  c.c_interfaces interfaces
    | Interface i ->
	ClassMap.fold
	  (fun i_sig i interfaces ->
	     let s =
	       try ClassMap.find i_sig interfaces
	       with _ -> ClassSet.empty in
	     let interfaces =
	       ClassMap.add i_sig (ClassSet.add cs s) interfaces in
	       update_interfaces classes_map (Interface i) interfaces cs
	  )
	  i.i_interfaces interfaces

exception Class_not_found of class_name

let add_classFile c classes_map interfaces =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let i =
	   try
	     match (ClassMap.find iname classes_map) with
	       | Interface i -> i
	       | Class _ ->
		   raise (Class_structure_error
			    (JDumpBasics.class_name c.c_name
			     ^" is declared to implements "
			     ^JDumpBasics.class_name iname
			     ^", which is a class and not an interface."))
	   with Not_found -> raise (Class_not_found iname)
	 in ClassMap.add iname i imap
      ) ClassMap.empty c.Javalib.c_interfaces in
  let c_super =
    match c.c_super_class with
      | None -> None
      | Some super ->
	  try
	    let c_super_info = ClassMap.find super classes_map in
	      match c_super_info with
		| Class c -> Some c
		| Interface _ ->
		    raise (Class_structure_error
			     (JDumpBasics.class_name c.c_name
			      ^" is declared to extends "
			      ^JDumpBasics.class_name super
			      ^", which is an interface and not a class.")
			  )
	  with Not_found -> raise (Class_not_found super)
  in
  let c_info = Class (make_class_node c c_super imap) in
    interfaces := update_interfaces classes_map c_info !interfaces c.c_name;
    ClassMap.add c.c_name c_info classes_map

let add_interfaceFile c classes_map =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let i =
	   try
	     match (ClassMap.find iname classes_map) with
	       | Interface i -> i
	       | Class c' ->
		   raise (Class_structure_error
			    ("Interface "^JDumpBasics.class_name c.i_name
			     ^" is declared to extends "
			     ^JDumpBasics.class_name c'.c_info.c_name
			     ^", which is an interface and not a class.")
			 )
	   with Not_found -> raise (Class_not_found iname)
	 in ClassMap.add iname i imap
      ) ClassMap.empty c.Javalib.i_interfaces
  and super =
    try
      match (ClassMap.find java_lang_object
	       classes_map) with
	| Class c -> c
	| Interface _ ->
	    raise (Class_structure_error"java.lang.Object is declared as an interface.")
    with Not_found -> raise (Class_not_found java_lang_object)
  in
  let i_info = Interface (make_interface_node c super imap) in
    ClassMap.add c.i_name i_info classes_map

let add_one_node f classes_map interfaces =
  match f with
    | JInterface i -> add_interfaceFile i classes_map
    | JClass c -> add_classFile c classes_map interfaces

let add_class_referenced c classmap to_add =
  Array.iter
    (function
      | ConstMethod (TClass cn,_)
      | ConstInterfaceMethod (cn,_)
      | ConstField (cn,_)
      | ConstValue (ConstClass (TClass cn)) ->
	  if not (ClassMap.mem cn classmap) then to_add := cn::!to_add
      | _ -> ()) (Javalib.get_consts c)

let get_class class_path jclasses_map cs =
  try ClassMap.find cs !jclasses_map
  with Not_found ->
    try
      let c = Javalib.get_class class_path cs
      in
	jclasses_map := ClassMap.add cs c !jclasses_map;
	c;
    with No_class_found _ -> raise (Class_not_found cs)

let rec add_node class_path c classes_map interfaces =
  let jclasses_map = ref ClassMap.empty in
  let to_add = ref [] in
  let classes_map =
    try
      let cname = Javalib.get_name c in
	if not (ClassMap.mem cname classes_map)
	then
	  begin
	    add_class_referenced c !jclasses_map to_add;
	    add_one_node c classes_map interfaces
	  end
	else classes_map
    with Class_not_found cs ->
      let missing_class = get_class class_path jclasses_map cs in
	add_node class_path c
	  (add_node class_path missing_class classes_map interfaces) interfaces
  in begin
      let p_classes = ref classes_map in
	try while true do
	  let cs = List.hd !to_add in
	    to_add := List.tl !to_add;
	    if not (ClassMap.mem cs !p_classes) then
	      let c = get_class class_path jclasses_map cs
	      in p_classes :=
		   add_node class_path c !p_classes interfaces
	done;
	  !p_classes
	with Failure "hd" -> !p_classes
    end

let get_children_classes c children_classes =
  let cs = c.c_info.c_name in
    try
      ClassMap.find cs !children_classes
    with _ ->
      let classes =
	List.fold_right
	  (fun c cmap ->
	     if not(c.c_info.c_abstract) then
	       ClassMap.add c.c_info.c_name c cmap
	     else cmap
	  ) (c :: (get_all_children_classes c)) ClassMap.empty in
	children_classes := ClassMap.add cs classes !children_classes;
	classes

let static_virtual_lookup virtual_lookup_map children_classes c ms =
  let cs = c.c_info.c_name in
    try
      ClassMethodMap.find (make_cms cs ms) !virtual_lookup_map
    with
      | _ ->
	  let instantiated_classes = get_children_classes c children_classes in
	  let cmmap =
	    JControlFlow.invoke_virtual_lookup ~c:(Some c) ms
	      instantiated_classes in
	  let cmset = ClassMethodMaptoSet.to_set cmmap in
	    virtual_lookup_map :=
	      ClassMethodMap.add (make_cms cs ms) cmset !virtual_lookup_map;
	    cmset

let static_interface_lookup interface_lookup_map classes_map interfaces
    children_classes i ms =
  let cs = i.i_info.i_name in
    try
      ClassMethodMap.find (make_cms cs ms) !interface_lookup_map
    with
      | _ ->
	  let equivalent_classes =
	    try ClassMap.find cs interfaces
	    with _ -> ClassSet.empty in
	  let instantiated_classes =
	    ClassSet.fold
	      (fun cs cmap ->
		 let ioc_info = ClassMap.find cs classes_map in
		 let ioc = ioc_info in
		   match ioc with
		     | Interface _ -> assert false
		     | Class c ->
			 let classes =
			   get_children_classes c children_classes in
			   ClassMap.merge (fun x _ -> x) cmap classes
	      ) equivalent_classes ClassMap.empty in
	  let cmmap =
	    JControlFlow.invoke_interface_lookup ~i:(Some i) ms
	      instantiated_classes in
	  let cmset = ClassMethodMaptoSet.to_set cmmap in
	    interface_lookup_map :=
	      ClassMethodMap.add (make_cms cs ms) cmset !interface_lookup_map;
	    cmset

let static_static_lookup static_lookup_map c ms =
  let cs = c.c_info.c_name in
    try
      ClassMethodMap.find (make_cms cs ms) !static_lookup_map
    with
      | _ ->
	  let (_,cm) = JControlFlow.invoke_static_lookup c ms in
	  let cmset = ClassMethodSet.add cm.cm_class_method_signature
	    ClassMethodSet.empty in
	    static_lookup_map :=
	      ClassMethodMap.add (make_cms cs ms) cmset !static_lookup_map;
	    cmset

let static_special_lookup special_lookup_map current_class c ms =
  let ccs = get_name current_class in
  let cs = c.c_info.c_name in
  let ccmmap =
    try
      ClassMap.find ccs !special_lookup_map
    with _ -> ClassMethodMap.empty in
    try
      ClassMethodMap.find (make_cms cs ms) ccmmap
    with
      | _ ->
	  let (_,cm) = JControlFlow.invoke_special_lookup current_class c ms in
	  let cmset = ClassMethodSet.add cm.cm_class_method_signature
	    ClassMethodSet.empty in
	    special_lookup_map := ClassMap.add ccs
	      (ClassMethodMap.add (make_cms cs ms) cmset ccmmap) !special_lookup_map;
	    cmset

let static_lookup_method =
  let virtual_lookup_map = ref ClassMethodMap.empty
  and interface_lookup_map = ref ClassMethodMap.empty
  and static_lookup_map = ref ClassMethodMap.empty
  and special_lookup_map = ref ClassMap.empty
  and children_classes = ref ClassMap.empty in
    fun classes_map interfaces cs ms pp ->
      let m = get_method (ClassMap.find cs classes_map) ms in
	match m with
	  | AbstractMethod _ -> failwith "Can't call static_lookup on Abstract Methods"
	  | ConcreteMethod cm ->
	      (match cm.cm_implementation with
		 | Native -> failwith "Can't call static_lookup on Native methods"
		 | Java code ->
		     let opcode = (Lazy.force code).c_code.(pp) in
		       (match opcode with
			  | OpInvoke (`Interface ccs, cms) ->
			      let cc =
				let ioc =
			     	  (ClassMap.find ccs classes_map) in
			     	  match ioc with
			     	    | Class _ ->
			     		failwith "Impossible InvokeInterface"
			     	    | Interface i -> i in
				static_interface_lookup interface_lookup_map
			     	  classes_map interfaces children_classes cc cms
			  | OpInvoke (`Virtual (TClass ccs), cms) ->
			      let cc =
				let ioc =
			     	  (ClassMap.find ccs classes_map) in
			     	  match ioc with
			     	    | Interface _ ->
			     		failwith "Impossible InvokeVirtual"
			     	    | Class c -> c in
				static_virtual_lookup virtual_lookup_map
				  children_classes cc cms
			  | OpInvoke (`Virtual (TArray _), cms) ->
			      (* should only happen with [clone()] *)
			      let cobj =
				let ioc =
			     	  (ClassMap.find java_lang_object classes_map) in
			     	  match ioc with
			     	    | Interface _ ->
			     		failwith "Impossible InvokeVirtual"
			     	    | Class c -> c in
				static_virtual_lookup virtual_lookup_map
				  children_classes cobj cms
			  | OpInvoke (`Static ccs, cms) ->
			      let cc =
				let ioc =
				  (ClassMap.find ccs classes_map) in
				  match ioc with
				    | Interface _ ->
					failwith "Impossible InvokeStatic"
				    | Class c -> c in
				static_static_lookup static_lookup_map cc cms
			  | OpInvoke (`Special ccs, cms) ->
			      let cc =
				let ioc =
				  (ClassMap.find ccs classes_map) in
				  match ioc with
				    | Interface _ ->
					failwith "Impossible InvokeSpecial"
				    | Class c -> c in
			      let current_class =
				(ClassMap.find cs classes_map) in
				static_special_lookup special_lookup_map
				  current_class cc cms
			  | _ -> raise Not_found
		       )
	      )

let default_classes = List.map make_cn
  ["java.lang.Class"; "java.lang.System"; "java.lang.String"; "java.lang.Thread";
   "java.lang.ThreadGroup"; "java.lang.ref.Finalizer"; "java.lang.OutOfMemoryError";
   "java.lang.NullPointerException"; "java.lang.ArrayStoreException";
   "java.lang.ArithmeticException"; "java.lang.StackOverflowError";
   "java.lang.IllegalMonitorStateException"; "java.lang.Compiler";
   "java.lang.reflect.Method"; "java.lang.reflect.Field"]

let parse_program ?(other_classes=default_classes) class_path names =
  let class_path = Javalib.class_path class_path in
    try
      let class_map = ref
        begin
          List.fold_left
	    (fun clmap cn ->
	       let c = Javalib.get_class class_path cn in
	       let c_name = Javalib.get_name c in
	         ClassMap.add c_name c clmap
	    ) ClassMap.empty (other_classes @ names)
        end in
      let interfaces = ref ClassMap.empty in
      let p_classes =
        ClassMap.fold
          (fun _ c classes -> add_node class_path c classes interfaces)
          !class_map ClassMap.empty in
      let parsed_methods =
        ClassMap.fold
          (fun _ ioc_info cmmap ->
	     let ioc = ioc_info in
	       MethodMap.fold
	         (fun _ cm cmmap ->
		    ClassMethodMap.add cm.cm_class_method_signature (ioc,cm) cmmap
	         ) (get_concrete_methods ioc) cmmap
          ) p_classes ClassMethodMap.empty in
        Javalib.close_class_path class_path;
        { classes = p_classes;
	  parsed_methods = parsed_methods;
	  static_lookup_method = static_lookup_method p_classes !interfaces
        }
    with e ->
      Javalib.close_class_path class_path;
      raise e

let parse_program_bench ?(other_classes=default_classes) class_path names =
  let time_start = Sys.time() in
    ignore(parse_program ~other_classes class_path names);
    let time_stop = Sys.time() in
      Printf.printf "program parsed in %fs.\n" (time_stop-.time_start)
