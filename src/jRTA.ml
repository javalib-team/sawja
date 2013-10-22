(*
 * This file is part of SAWJA
 * Copyright (c)2009 Laurent Hubert (CNRS)
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

module Program =
struct
  type rta_method = { mutable has_been_parsed : bool;
		      c_method : JCode.jcode jmethod }
  type class_info =
      { class_data : JCode.jcode node;
	mutable is_instantiated : bool;
	mutable instantiated_subclasses : JCode.jcode class_node ClassMap.t;
	super_classes : class_name list;
	super_interfaces : ClassSet.t;
	methods : rta_method MethodMap.t;
	mutable memorized_virtual_calls : MethodSet.t;
	mutable memorized_interface_calls : MethodSet.t }

  (*type class_method = JCode.jcode class_node * JCode.jcode concrete_method*)

  type program_cache =
      { mutable classes : class_info ClassMap.t;
	(* for each interface, interfaces maps a list of classes
	   that implements this interface or one of its subinterfaces *)
	mutable interfaces : ClassSet.t ClassMap.t;
	mutable static_virtual_lookup : ClassMethodSet.t ClassMethodMap.t;
	mutable static_static_lookup : ClassMethodSet.t ClassMethodMap.t;
	mutable static_special_lookup : (ClassMethodSet.t
					   ClassMethodMap.t) ClassMap.t;
	(* the clinits fields contains a set of class indexes whose clinit
	   methods have already been added to the workset *)
	mutable clinits : ClassSet.t;
	workset : (class_name * JCode.jcode concrete_method) Wlist.wlist;
	classpath : Javalib.class_path;
	mutable native_methods : ClassMethodSet.t;
	parse_natives : bool;
	native_methods_info : JNativeStubs.t }

  exception Method_not_found

  let methods2rta_methods ioc =
    let mmap =
      match ioc with
	| JClass c -> c.c_methods
	| JInterface i ->
	    let mmap =
	      MethodMap.map (fun am -> AbstractMethod am) i.i_methods in
	      (match i.i_initializer with
		 | None -> mmap
		 | Some cm ->
		     MethodMap.add clinit_signature (ConcreteMethod cm) mmap)
    in
      MethodMap.map (fun m -> { has_been_parsed = false; c_method = m }) mmap

  let rec value_type2class_name v =
    match v with
      | TBasic _ -> None
      | TObject o ->
	  match o with
	    | TClass cn -> Some cn
	    | TArray a -> value_type2class_name a

  let rec to_class_node ioc =
    match ioc with
      | Class c -> c
      | Interface _ -> failwith "to_class_node applied on interface !"

  and to_interface_node ioc =
    match ioc with
      | Class _ -> failwith "to_interface_node applied on class !"
      | Interface i -> i

  and load_value_type_class p v =
    match (value_type2class_name v) with
      | None -> ()
      | Some cn -> 
	  ignore(get_class_info p cn)
  
  and load_classes_of_fields p node = 
    let load_classes_of_fields' fm = 
      FieldMap.iter 
	( fun fs _ -> 
	    load_value_type_class p (fs_type fs))
	fm
    in 
      match node.class_data with
	  Class cnode -> load_classes_of_fields' cnode.c_info.c_fields
	| Interface inode -> load_classes_of_fields' inode.i_info.i_fields

  and get_class_info ?(check_name=true) p cs =
    let rec get_class_info' ?(check_name=true) p cs fields_of_nodes=
      try
	(ClassMap.find cs p.classes,fields_of_nodes)
      with
	| Not_found ->
	    let (node,new_fields_of_nodes) = 
	      add_class ~check_name p cs fields_of_nodes
	    in 
	      (node,ClassMap.add cs node new_fields_of_nodes)

    and add_class ?(check_name=true) p cs fields_of_nodes=
      (* We assume that a call to add_class is done only when a class
	 has never *)
      (* been loaded in the program. Loading a class implies loading
	 all its *)
      (* superclasses recursively. *)
      let fields_of_nodes' = 
	ref fields_of_nodes 
      in
      let ioc = Javalib.get_class p.classpath cs in
      let cs' = Javalib.get_name ioc in
	if check_name && cs' <> cs then
	  failwith "class name does not match the file name";
	let cs = cs' in
	let rta_methods = methods2rta_methods ioc in
	let ioc_info =
	  match ioc with
	    | JClass c ->
		let (super_class,super_classes) =
		  (match c.c_super_class with
		     | None -> (None, [])
		     | Some sc ->
			 let (sc_info,fields_nodes) = 
			   get_class_info' p sc !fields_of_nodes'
			 in
			   fields_of_nodes':=fields_nodes;
			   (Some (to_class_node sc_info.class_data),
			    sc :: sc_info.super_classes)
		  )
		and implemented_interfaces =
		  List.fold_right
		    (fun iname m ->
		       let (ii_info,fields_nodes) = 
			 get_class_info' p iname !fields_of_nodes'
		       in 
			 fields_of_nodes':=fields_nodes;	 
			 ClassMap.add iname
			   (to_interface_node ii_info.class_data) m
		    ) c.Javalib.c_interfaces ClassMap.empty
		in
		  (* For each implemented interface and its super
		     interfaces we add cni in the program interfaces
		     map *)
		let super_implemented_interfaces =
		  (ClassMap.fold
		     (fun i_sig _ s ->
			let (i_info,fields_nodes) = 
			  get_class_info' p i_sig !fields_of_nodes'
			in
			  fields_of_nodes':=fields_nodes;
			  ClassSet.add i_sig
			    (ClassSet.union s i_info.super_interfaces)
		     ) implemented_interfaces ClassSet.empty) in
		  ClassSet.iter
		    (fun i ->
		       if ( ClassMap.mem i p.interfaces ) then
			 p.interfaces <- ClassMap.add i
			   (ClassSet.add cs (ClassMap.find i p.interfaces))
			   p.interfaces
		       else
			 p.interfaces <- ClassMap.add i
			   (ClassSet.add cs ClassSet.empty) p.interfaces
		    ) super_implemented_interfaces;
		  let node = 
		    make_class_node c super_class implemented_interfaces 
		  in
		    { class_data = Class node;
		      is_instantiated = false;
		      instantiated_subclasses = ClassMap.empty;
		      super_classes = super_classes;
		      (* for a class super_interfaces contains
			 the transitively implemented interfaces *)
		      super_interfaces = super_implemented_interfaces;
		      methods = rta_methods;
		      memorized_virtual_calls = MethodSet.empty;
		      memorized_interface_calls = MethodSet.empty }
	    | JInterface i ->
		let (interfaces,super_interfaces) =
		  List.fold_right
		    (fun si (m,s)->
		       let (si_info,fields_nodes) = 
			 get_class_info' p si !fields_of_nodes'
		       in
			 fields_of_nodes':=fields_nodes;
			 let m = ClassMap.add si
			   (to_interface_node si_info.class_data) m in
			 let s = ClassSet.union si_info.super_interfaces
			   (ClassSet.add si s) in
			   (m,s)
		    ) i.Javalib.i_interfaces (ClassMap.empty, ClassSet.empty) in
		let (object_node) =
		  let (o_info,fields_nodes) = 
		    get_class_info' p java_lang_object !fields_of_nodes'
		  in 
		    fields_of_nodes':= fields_nodes;
		    to_class_node (o_info).class_data 
		in
		let node = make_interface_node i object_node interfaces in
		  { class_data = Interface node;
		    is_instantiated = false;
		    (* An interface will never be instantiated *)
		    instantiated_subclasses = ClassMap.empty;
		    super_classes = [];
		    super_interfaces = super_interfaces;
		    methods = rta_methods;
		    memorized_virtual_calls = MethodSet.empty;
		    memorized_interface_calls = MethodSet.empty }
	in
	  p.classes <- ClassMap.add cs ioc_info p.classes;
	  (ioc_info,!fields_of_nodes')
    in
      try
	ClassMap.find cs p.classes
      with
	| Not_found ->
	    let (node,nodes_to_add_fields) = 
	      get_class_info' ~check_name p cs ClassMap.empty
	    in
	      ClassMap.iter
		(fun _ class_node -> 
		   load_classes_of_fields p class_node;
		)
		nodes_to_add_fields;
	      node

  and add_clinit p cs =
    let ioc_info = get_class_info p cs in
      if ( not(ClassSet.mem cs p.clinits)
	   && defines_method ioc_info.class_data clinit_signature) then
	(
	  add_to_workset p (cs,clinit_signature);
	  p.clinits <- ClassSet.add cs p.clinits
	)

  and add_class_clinits p cs =
    let ioc_info = get_class_info p cs in
      List.iter
	(fun cs -> add_clinit p cs)
	(cs :: ioc_info.super_classes)

  and get_method p cs ms =
    let cl_info = get_class_info p cs in
      try
	MethodMap.find ms cl_info.methods
      with
	| Not_found -> raise Method_not_found

  and add_to_workset p (cs,ms) =
    let m = get_method p cs ms in
      match m with
	| { c_method = AbstractMethod _ } ->
	    failwith "Can't add an Abstract Method to the workset"
	| { c_method = ConcreteMethod cm } ->
	    (match cm.cm_implementation with
	       | Native ->
		   if not(m.has_been_parsed) then
		     (m.has_been_parsed <- true;
		      p.native_methods <-
			ClassMethodSet.add (make_cms cs ms) p.native_methods;
		      if (p.parse_natives) then Wlist.add (cs,cm) p.workset
		     )
	       | Java _ ->
		   if not(m.has_been_parsed) then
		     (m.has_been_parsed <- true;
		      Wlist.add (cs,cm) p.workset)
	    )

  let load_method_params_classes p ms =
    let args = ms_args ms in
    let rtype = ms_rtype ms in
    let prms = match rtype with
      | None -> args
      | Some r -> r :: args in
      List.iter (fun v -> load_value_type_class p v) prms

  let load_catch_types_classes p exc_tbl = 
    List.iter
      (fun exc_h -> 
	 match exc_h.e_catch_type with
	     Some cn -> ignore(get_class_info p cn)
	   | None -> ())
      exc_tbl

  let resolve_field p cs fs =
    let ioc = (get_class_info p cs).class_data in
    let rioc_list = JControlFlow.resolve_field fs ioc in
      List.map
	(fun rioc ->
	   match rioc with
	     | Class rc -> rc.c_info.c_name
	     | Interface ri -> ri.i_info.i_name) rioc_list

  let update_virtual_lookup_set p (c,ms) instantiated_subclasses =
    let cs = c.c_info.c_name in
    let virtual_lookup_map =
      JControlFlow.invoke_virtual_lookup ~c:(Some c) ms
	instantiated_subclasses in
      ClassMethodMap.iter
	(fun _ (rc,cm) ->
	   let rcs = rc.c_info.c_name in
	     add_to_workset p (rcs,ms);
	     let s = ClassMethodMap.find (make_cms cs ms) p.static_virtual_lookup in
	       p.static_virtual_lookup <-
		 ClassMethodMap.add (make_cms cs ms)
		 (ClassMethodSet.add cm.cm_class_method_signature s)
		 p.static_virtual_lookup
	) virtual_lookup_map

  let invoke_virtual_lookup p cs ms =
    (* If this virtual call site appears for the first time, *)
    (* we will update the static_lookup_virtual map, otherwise *)
    (* no work has to be done. *)
    let c_info = get_class_info p cs in
      if not( MethodSet.mem ms c_info.memorized_virtual_calls ) then
	(c_info.memorized_virtual_calls <-
	   MethodSet.add ms c_info.memorized_virtual_calls;
	 p.static_virtual_lookup <-
	   ClassMethodMap.add (make_cms cs ms)
	   ClassMethodSet.empty p.static_virtual_lookup;
	 let instantiated_classes =
	   if ( c_info.is_instantiated ) then
	     ClassMap.add cs (to_class_node (c_info.class_data))
	       c_info.instantiated_subclasses
	   else c_info.instantiated_subclasses in
	 let c = to_class_node c_info.class_data in
	   update_virtual_lookup_set p (c,ms) instantiated_classes
	)

  let interface_lookup_action interfaces cs f =
    if ( ClassMap.mem cs interfaces ) then
      (ClassSet.iter
	 f (ClassMap.find cs interfaces))
    else ()
      (* otherwise, the classes implementing the interface have not
	 been charged yet so we can't do anything *)

  let invoke_interface_lookup p cs ms =
    let i_info = get_class_info p cs in
      i_info.memorized_interface_calls <-
  	MethodSet.add ms i_info.memorized_interface_calls;
      interface_lookup_action p.interfaces cs
  	(fun x -> invoke_virtual_lookup p x ms)

  let update_interface_lookup_set p interfaces =
    ClassSet.iter
      (fun i ->
	 let i_info = get_class_info p i in
	   MethodSet.iter
	     (fun ms ->
		invoke_interface_lookup p i ms
	     )
	     i_info.memorized_interface_calls
      )
      interfaces
      (* transitivly implemented interfaces *)

  let add_instantiated_class p cs =
    let cl_info = get_class_info p cs in
      if not( cl_info.is_instantiated ) then
	(cl_info.is_instantiated <- true;
	 (* Now we need to update the static_lookup_virtual map *)
	 (* for each virtual call that already occurred on A and *)
	 (* its super classes. *)
	 (let calls = cl_info.memorized_virtual_calls in
	  let cl = to_class_node cl_info.class_data in
	  let subclass_map = ClassMap.add cs cl ClassMap.empty in
	    MethodSet.iter
	      (fun ms ->
		 update_virtual_lookup_set p (cl,ms) subclass_map
	      ) calls;
	    update_interface_lookup_set p cl_info.super_interfaces;
	    List.iter
	      (fun scs ->
	   	 let s_info = get_class_info p scs in
		 let sc = to_class_node s_info.class_data in
		   (* We complete the list of instantiated subclasses for cn
		      and its superclasses *)
		   s_info.instantiated_subclasses <-
		     ClassMap.add cs cl s_info.instantiated_subclasses;
	   	   (let calls = s_info.memorized_virtual_calls in
	   	      MethodSet.iter
	   		(fun ms ->
			   update_virtual_lookup_set p (sc,ms) subclass_map
	   		) calls
		   );
		   update_interface_lookup_set p s_info.super_interfaces
	      )
	      cl_info.super_classes);
	)

  let update_special_lookup_set p current_class_sig cs ms s =
    let cmmap =
      try ClassMap.find current_class_sig p.static_special_lookup
      with _ -> ClassMethodMap.empty in
    let rset =
      try ClassMethodMap.find (make_cms cs ms) cmmap
      with _ -> ClassMethodSet.empty in
      p.static_special_lookup <-
	(ClassMap.add current_class_sig
	   (ClassMethodMap.add (make_cms cs ms)
	      (ClassMethodSet.union rset s) cmmap)
	   p.static_special_lookup)

  let invoke_special_lookup p current_class_sig cs ms =
    let current_class = (get_class_info p current_class_sig).class_data in
    let called_class = to_class_node (get_class_info p cs).class_data in
    let (rc,cm) =
      JControlFlow.invoke_special_lookup current_class called_class ms in
    let rcs = rc.c_info.c_name in
    let s = ClassMethodSet.add cm.cm_class_method_signature
      ClassMethodSet.empty in
      update_special_lookup_set p current_class_sig cs ms s;
      (* we add (cs,ms) to the workset *)
      add_to_workset p (rcs,ms)

  let invoke_static_lookup p cs ms =
    let c = to_class_node (get_class_info p cs).class_data in
    let (rc,cm) = JControlFlow.invoke_static_lookup c ms in
    let rcs = rc.c_info.c_name in
      (if not( ClassMethodMap.mem (make_cms cs ms) p.static_static_lookup ) then
       	 let s = ClassMethodSet.add cm.cm_class_method_signature
	   ClassMethodSet.empty in
       	   p.static_static_lookup <-
	     ClassMethodMap.add (make_cms cs ms) s p.static_static_lookup;
	   add_to_workset p (rcs,ms)
      );
      rcs

  let parse_instruction p current_class_name op =
    match op with
      | OpNew cs ->
	  add_instantiated_class p cs;
	  add_class_clinits p cs
      | OpConst (`Class o) ->
	  load_value_type_class p (TObject o);
	  let jlc = make_cn "java.lang.Class" in
	    add_instantiated_class p jlc;
	    add_class_clinits p jlc
      | OpConst (`String _) ->
	  let jls = make_cn "java.lang.String" in
	    add_instantiated_class p jls;
	    add_class_clinits p jls	      
      | OpGetField (cs,fs)
      | OpPutField (cs,fs) ->
	  ignore (get_class_info p cs);
	  load_value_type_class p (fs_type fs)
      | OpGetStatic (cs,fs)
      | OpPutStatic (cs,fs) ->
	  load_value_type_class p (fs_type fs);
	  let rcs_list = resolve_field p cs fs in
	    List.iter
	      (fun rcs ->
		 let ioc_info = get_class_info p rcs in
		   (match ioc_info.class_data with
		      | Class _ -> add_class_clinits p rcs
		      | Interface _ -> add_clinit p rcs
		   )
	      ) rcs_list
      | OpInvoke(`Virtual (TClass cs),ms) ->
	  load_method_params_classes p ms;
	  invoke_virtual_lookup p cs ms
      | OpInvoke(`Virtual (TArray v),ms) ->
	  (* should only happen with [clone()] *)
	  load_value_type_class p v;
	  load_method_params_classes p ms;
	  invoke_virtual_lookup p java_lang_object ms
      | OpInvoke(`Interface cs,ms) ->
	  load_method_params_classes p ms;
	  invoke_interface_lookup p cs ms
      | OpInvoke(`Special cs,ms) ->
	  load_method_params_classes p ms;
      	  invoke_special_lookup p current_class_name cs ms
      | OpInvoke(`Static cs,ms) ->
	  load_method_params_classes p ms;
      	  let rcs = invoke_static_lookup p cs ms in
	    add_class_clinits p rcs
      | OpNewArray v ->
	  add_instantiated_class p java_lang_object;
	  add_class_clinits p java_lang_object;
	  load_value_type_class p v
      | OpAMultiNewArray (o,_) -> 
	  add_instantiated_class p java_lang_object;
	  add_class_clinits p java_lang_object;
	  load_value_type_class p (TObject o)
      | OpCheckCast o
      | OpInstanceOf o ->
	  load_value_type_class p (TObject o)
      | _ -> ()

  let parse_native_method p allocated_classes calls =
    let normalize_signature s =
      (* hack : why a class should not be encapsulated by L; ? *)
      let len = String.length s in
	if (len > 2) then
	  if (s.[len - 1] = ';') then
	    if (s.[0] = 'L') then
	      String.sub s 1 (len - 2)
	    else failwith "Bad class signature."
	  else s
	else s in
      List.iter
	(fun signature ->
	   match JParseSignature.parse_objectType
	     (normalize_signature signature) with
	       | TArray _ -> ()
	       | TClass cs ->
		   add_instantiated_class p cs;
		   add_class_clinits p cs
	) allocated_classes;
      List.iter
      	(fun (m_class,m_name,m_signature) ->
	   let cs =
	     match JParseSignature.parse_objectType
	       (normalize_signature m_class) with
		 | TArray _ -> failwith "Bad class"
		 | TClass cn -> cn in
	   let (parameters,rettype) =
	     JParseSignature.parse_method_descriptor m_signature in
	   let ms = make_ms m_name parameters rettype in
	     add_to_workset p (cs,ms)
	) calls

  let iter_workset p =
    let tail = Wlist.tail p.workset
    in
      Wlist.iter_to_head
	(fun (cs,cm) ->
	   match cm.cm_implementation with
	     | Native ->
		 if not(p.parse_natives) then
		   failwith "A Native Method shouldn't be found in the workset"
		 else
		   let ms = cm.cm_signature in
		   let m_class = JPrint.object_type ~jvm:true (TClass cs)
		   and m_name = ms_name ms
		   and m_signature =
		     JPrint.method_descriptor ~jvm:true (ms_args ms)
		       (ms_rtype ms) in
		   let m = (m_class,m_name,m_signature) in
		     (try
		   	let (m_alloc, m_calls) =
		   	  (JNativeStubs.get_native_method_allocations m
		   	     p.native_methods_info,
		   	   JNativeStubs.get_native_method_calls m
		   	     p.native_methods_info) in
		   	  parse_native_method p m_alloc m_calls
		      with _ ->
		   	prerr_endline ("warning : found native method " ^ m_class
		   		       ^ "." ^ m_name ^ ":" ^ m_signature
		   		       ^ " not present in the stub file.")
		     )
	     | Java t ->
		 let jcode = (Lazy.force t) in 
		 let code = jcode.c_code
		 in
		   load_catch_types_classes p jcode.c_exc_tbl;
		   Array.iter (parse_instruction p cs) code)
	tail

  let new_program_cache instantiated entrypoints native_stubs classpath =
    let (parse_natives,native_methods_info) =
      match native_stubs with
	| None -> (false, JNativeStubs.empty_info)
	| Some file -> (true,
			JNativeStubs.parse_native_info_file file) in
    let workset = Wlist.create () in
    let p =
      { classes = ClassMap.empty;
	interfaces = ClassMap.empty;
	static_virtual_lookup = ClassMethodMap.empty;
	static_static_lookup = ClassMethodMap.empty;
	static_special_lookup = ClassMap.empty;
	clinits = ClassSet.empty;
	workset = workset;
	classpath = classpath;
	native_methods = ClassMethodSet.empty;
	parse_natives = parse_natives;
	native_methods_info = native_methods_info }
    in
      List.iter (add_instantiated_class p) instantiated;
      List.iter (add_class_clinits p) instantiated;
      List.iter
	(fun cms ->
           let cs,ms = cms_split cms in
	   let c = get_class_info ~check_name:false p cs in
	   let cs = get_name c.class_data in
	   add_class_clinits p cs;
	   if defines_method c.class_data ms
	   then
	     match c.class_data with
	       | Interface _ ->
		   invoke_interface_lookup p cs ms
	       | Class _ -> add_to_workset p (cs,ms)
	   else 
	     if not (ms_equal ms clinit_signature)
	     then
	       invalid_arg 
		 (Printf.sprintf
		    "The entrypoint method '%s' does not exist in the class '%s'"
		    (JPrint.method_signature ms)
		    (JPrint.class_name cs))
	)
	entrypoints;
      p

  let parse_program instantiated entrypoints native_stubs classpath =
    let classpath = Javalib.class_path classpath in
      try
        let p = new_program_cache instantiated entrypoints native_stubs classpath in
          iter_workset p;
          if not (ClassMethodSet.is_empty p.native_methods)
          then prerr_endline "The program contains native method. Beware that native methods' side effects may invalidate the result of the analysis.";
          Javalib.close_class_path classpath;
          let instantiated_classes =
	    ClassMap.fold
	      (fun cs info cmap ->
	         match info.class_data with
	           | Interface _ -> cmap
	           | Class c ->
		       if (info.is_instantiated) then
		         ClassMap.add cs c cmap
		       else cmap) p.classes ClassMap.empty in
	    (p, instantiated_classes)
      with e ->
        Javalib.close_class_path classpath;
        raise e

  let _parse_program_bench instantiated entrypoints classpath =
    let time_start = Sys.time() in
    let (p,_) = parse_program instantiated entrypoints None classpath in
    let s = Wlist.size p.workset in
      Printf.printf "Workset of size %d\n" s;
      let time_stop = Sys.time() in
	Printf.printf "program parsed in %fs.\n" (time_stop-.time_start)
end

let static_virtual_lookup virtual_lookup_map cs ms =
  try
    ClassMethodMap.find (make_cms cs ms) virtual_lookup_map
  with _ ->
    (* probably dead code *)
    ClassMethodSet.empty

let static_static_lookup static_lookup_map cs ms =
  ClassMethodMap.find (make_cms cs ms) static_lookup_map

let static_interface_lookup virtual_lookup_map interfaces_map cs ms =
  let s = ref ClassMethodSet.empty in
  let f =
    (fun x ->
       let calls =
	 static_virtual_lookup virtual_lookup_map x ms in
	 s := ClassMethodSet.union !s calls) in
    Program.interface_lookup_action interfaces_map cs f;
    !s

let static_special_lookup special_lookup_map cs ccs cms =
  ClassMethodMap.find (make_cms ccs cms) (ClassMap.find cs special_lookup_map)

let static_lookup_method p :
    class_name -> method_signature -> int -> ClassMethodSet.t =
  let virtual_lookup_map = p.Program.static_virtual_lookup
  and special_lookup_map = p.Program.static_special_lookup
  and static_lookup_map = p.Program.static_static_lookup
  and interfaces_map = p.Program.interfaces
  and classes_map = p.Program.classes in
    fun cs ms pp ->
      let ioc = to_ioc (ClassMap.find cs classes_map).Program.class_data in
      let m = Javalib.get_method ioc ms in
	match m with
	  | AbstractMethod _ -> failwith "Can't call static_lookup on Abstract Methods"
	  | ConcreteMethod cm ->
	      (match cm.cm_implementation with
		 | Native -> failwith "Can't call static_lookup on Native methods"
		 | Java code ->
		     let opcode = (Lazy.force code).c_code.(pp) in
		       (match opcode with
			  | OpInvoke (`Interface ccs,cms) ->
			      static_interface_lookup virtual_lookup_map
				interfaces_map ccs cms
			  | OpInvoke (`Virtual (TClass ccs),cms) ->
			      static_virtual_lookup virtual_lookup_map ccs cms
			  | OpInvoke (`Virtual (TArray _),cms) ->
			      (* should only happen with [clone()] *)
			      static_virtual_lookup virtual_lookup_map
				java_lang_object cms
			  | OpInvoke (`Static ccs,cms) ->
			      static_static_lookup static_lookup_map ccs cms
			  | OpInvoke (`Special ccs,cms) ->
			      static_special_lookup special_lookup_map cs ccs cms
			  | _ -> raise Not_found
		       )
	      )

let pcache2jprogram p =
  { classes =
      ClassMap.mapi
	(fun i _ -> (Program.get_class_info p i).Program.class_data)
	p.Program.classes;
    parsed_methods =
      ClassMap.fold
	(fun _ ioc_info cmmap ->
	   MethodMap.fold
	     (fun _ m cmmap ->
		if (m.Program.has_been_parsed) then
		  match m.Program.c_method with
		    | AbstractMethod _ -> assert false
		    | ConcreteMethod cm ->
			ClassMethodMap.add cm.cm_class_method_signature
			  (ioc_info.Program.class_data,cm) cmmap
		else cmmap
	     ) ioc_info.Program.methods cmmap
	) p.Program.classes ClassMethodMap.empty;
    static_lookup_method = static_lookup_method p
  }

(**RuntimeException and Error that could be thrown by VM (cf. JVM Spec 1.5 §2.16.4).*)
let default_native_throwable = 
 [ (*RuntimeException that could be thrown by VM.*)
    make_cn "java.lang.ArithmeticException";
    make_cn "java.lang.ArrayStoreException";
    make_cn "java.lang.ClassCastException";
    make_cn "java.lang.IllegalMonitorStateException";
    make_cn "java.lang.IndexOutOfBoundsException";
    make_cn "java.lang.NegativeArraySizeException";
    make_cn "java.lang.NullPointerException";
    make_cn "java.lang.SecurityException";
    (*Error that could be thrown by VM.*)
    make_cn "java.lang.ClassFormatError";
    make_cn "java.lang.ClassCircularityError";
    make_cn "java.lang.NoClassDefFoundError";
    make_cn "java.lang.UnsupportedClassVersionError";
    make_cn "java.lang.NoSuchFieldError";
    make_cn "java.lang.NoSuchMethodError";
    make_cn "java.lang.InstantiationError";
    make_cn "java.lang.IllegalAccessError";
    make_cn "java.lang.VerifyError";
    make_cn "java.lang.ExceptionInInitializerError";
    make_cn "java.lang.AbstractMethodError";
    make_cn "java.lang.UnsatisfiedLinkError";
    make_cn "java.lang.InternalError";
    make_cn "java.lang.OutOfMemoryError";
    make_cn "java.lang.StackOverflowError";
    make_cn "java.lang.UnknownError";
 ]

(* cf. openjdk6/hotspot/src/share/vm/runtime/thread.cpp *)
let default_entrypoints =
  let initializeSystemClass =
    ("java.lang.System",
     make_ms "initializeSystemClass" [] None)
  in
    List.map
      (fun (cn,ms) -> make_cms (make_cn cn) ms)
      (("java.lang.Object",clinit_signature)::
	 ("java.lang.String",clinit_signature)::
	 ("java.lang.System",clinit_signature)::
	 initializeSystemClass::
	 ("java.lang.ThreadGroup",clinit_signature)::
	 ("java.lang.Thread",clinit_signature)::
	 ("java.lang.reflect.Method",clinit_signature)::
	 ("java.lang.ref.Finalizer",clinit_signature)::
	 ("java.lang.Class",clinit_signature)::
	 ("java.lang.OutOfMemoryError",clinit_signature)::
	 ("java.lang.NullPointerException",clinit_signature)::
	 ("java.lang.ClassCastException",clinit_signature)::
	 ("java.lang.ArrayStoreException",clinit_signature)::
	 ("java.lang.ArithmeticException",clinit_signature)::
	 ("java.lang.StackOverflowError",clinit_signature)::
	 ("java.lang.IllegalMonitorStateException",clinit_signature)::
	 ("java.lang.Compiler",clinit_signature)::
	 ("java.lang.reflect.Field",clinit_signature)::
	 []
      )

(* cf. j2me_cldc/kvm/VmCommon/src/StartJVM.c *)

let cldc11_default_entrypoints =
    List.map
      (fun (cn,ms) -> make_cms (make_cn cn) ms)
      [(* These classes are only loaded :
	 Primitive array classes :
	  #define T_BOOLEAN   4
	  #define T_CHAR      5
	  #define T_FLOAT     6
	  #define T_DOUBLE    7
	  #define T_BYTE      8
	  #define T_SHORT     9
	  #define T_INT       10
	  #define T_LONG      11
       ("java.lang.Object",clinit_signature);
       ("java.lang.Throwable",clinit_signature);
       ("java.lang.Error",clinit_signature);
       *)
       ("java.lang.Class",clinit_signature);
       ("java.lang.Thread",clinit_signature);
       ("java.lang.String",clinit_signature);
       ("java.lang.System",clinit_signature);
       ("java.lang.OutOfMemoryError",clinit_signature)
      ]

let default_instantiated = (make_cn "java.lang.Class")::(make_cn "java.lang.String")::default_native_throwable

let parse_program
    ?(instantiated=default_instantiated)
    ?(other_entrypoints=default_entrypoints) 
    ?(native_stubs=None)
    classpath csms =
  let (p_cache, instantiated_classes) =
    (Program.parse_program instantiated (csms::other_entrypoints) native_stubs classpath) in
    (pcache2jprogram p_cache, instantiated_classes)
