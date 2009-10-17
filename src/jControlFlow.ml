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
open JCode
open Javalib
open JProgram
  
module PP = struct
  type 'a t = {cl:'a node;
	       meth:'a concrete_method;
	       pc:int;}
      
  let eqc = JProgram.ioc_equal
  let eqm = (==)
  let eqi = (=)
    
  let equal pp1 pp2 =
    eqm pp1.meth pp2.meth
    && eqi pp1.pc pp2.pc
    && eqc pp1.cl pp2.cl
      
  let hash pp1 =
    Hashtbl.hash (get_name pp1.cl,pp1.meth.cm_signature,pp1.pc)
      
  let compare pp1 pp2 =
    if equal pp1 pp2
    then 0
    else
      match cn_compare
	(get_name pp1.cl) (get_name pp2.cl) with
	  | 0 ->
	      begin
		match ms_compare
		  pp1.meth.cm_signature pp2.meth.cm_signature with
		    | 0 -> compare pp1.pc pp2.pc
		    | n -> n
	      end
	  | n -> n
	      
  exception NoCode of (class_name * method_signature)
    
  let to_string (pp:'a t) : string =
    let s = pp.meth.cm_signature in
    let mname = ms_name s in
    let mparams = ms_args s in
      cn_name (get_name pp.cl)
      ^ "."^ mname ^"("
      ^ (String.concat ", "
	   (List.map JDumpBasics.value_signature mparams))
      ^ "): " ^ string_of_int pp.pc
	
  let pprint fmt pp : unit =
    Format.pp_print_string fmt (to_string pp)
      
  let get_class (pp:'a t) : 'a node =
    pp.cl
      
  let get_meth (pp:'a t) : 'a concrete_method =
    pp.meth
      
  let get_pc (pp:'a t) : int = pp.pc
    
  let get_pp cl' meth' pc' : 'a t = {cl=cl';meth=meth';pc=pc';}
    
  let get_first_pp prog cs ms : 'a t =
    match get_node prog cs with
      | Interface {i_info = {i_initializer = Some m}} as c
	  when ms_equal m.cm_signature ms ->
	  {cl=c;meth=m;pc=0;}
      | Class c as cl' when MethodMap.mem ms c.c_info.c_methods ->
	  begin
	    match MethodMap.find ms c.c_info.c_methods with
	      | ConcreteMethod m->
		  {cl=cl'; meth=m; pc=0;}
	      | _ -> raise (NoCode (cs,ms))
	  end
      | _ -> raise (NoCode (cs,ms))
	  
  let get_first_pp_wp c ms : 'a t =
    match get_method c ms with
      | ConcreteMethod m ->
	  {cl=c;meth=m;pc=0;}
      | AbstractMethod m ->
	  raise (NoCode (get_name c,m.am_signature))
	    
  let goto_absolute pp i : 'a t = {pp with pc=i;}
    
  let goto_relative pp jmp : 'a t ={pp with pc=pp.pc+jmp;}
    
end
  
open PP
type pp = JCode.jvm_code PP.t
    
let get_code (pp:pp) : jopcodes =
  match pp.meth.cm_implementation with
    | Java c -> (Lazy.force c).c_code
    | Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))
	
let get_opcode (pp:pp) : jopcode = (get_code pp).(pp.pc)
  
let next_instruction pp =
  let opcodes = get_code pp
  and i = ref (succ pp.pc)
  in
    while opcodes.(!i) = OpInvalid do
      incr i;
    done;
    goto_absolute pp !i
      
let normal_successors pp =
  match get_opcode pp with
    | OpIf (_,l)
    | OpIfCmp (_,l) ->
	[next_instruction pp; goto_relative pp l]
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
	
	
	
let resolve_class program cs =
  try get_node program cs
  with Not_found -> raise NoClassDefFoundError
    
let rec resolve_field' result fs c : unit =
  let get_interfaces = function
    | Interface i -> i.i_interfaces
    | Class c -> c.c_interfaces
  in
    if defines_field c fs
    then
      begin
	if not (List.exists (ioc_equal c) !result)
	then result := c::!result
      end
    else
      begin
	ClassMap.iter
	  (fun _ i -> resolve_field' result fs (Interface i))
	  (get_interfaces c);
	if !result = []
	then
	  begin
	    match super_class c with
	      | Some super -> resolve_field' result fs (Class super)
	      | None -> ()
	  end
      end
	
let resolve_field fs c : 'a node list =
  let result = ref [] in
    resolve_field' result fs c;
    !result
      
      
let rec resolve_method' ms (c:'a class_node) : 'a class_node =
  if defines_method (Class c) ms
  then c
  else
    match super_class (Class c) with
      | Some super -> resolve_method' ms super
      | None -> raise NoSuchMethodError
	  
let rec resolve_interface_method' ?(acc=[]) ms (c:'a node) : 'a interface_node list =
  ClassMap.fold
    (fun _ i acc ->
       if defines_method (Interface i) ms
       then i::acc
       else resolve_interface_method' ~acc ms (Interface i))
    (get_interfaces c)
    acc
    
(* TODO : like resolve_field, resolve_method should return a list in
   case the method is defined in several interfaces at the same time. *)
(* TODO : we could use c_resolve_methods or update it if there are no
   matches to see if it increases performance. According to some
   tests on the loading of soot.jar, it's not significant *)
let rec resolve_method ms (c:'a class_node) : 'a node =
  try Class (resolve_method' ms c)
  with NoSuchMethodError ->
    match resolve_interface_method' ms (Class c) with
      | resolved::_ -> Interface resolved
      | [] -> match super_class (Class c) with
	  | None -> raise NoSuchMethodError
	  | Some c' -> resolve_method ms c'
	      
	      
let resolve_interface_method ms (c:'a interface_node) : 'a node =
  if defines_method (Interface c) ms
  then (Interface c)
  else
    match resolve_interface_method' ms (Interface c) with
      | resolved::_ -> Interface resolved
      | [] -> Class (resolve_method' ms c.i_super) (* super = java.lang.object *)
	  
let resolve_all_interface_methods ms (i:'a interface_node) : 'a interface_node list =
  if defines_method (Interface i) ms
  then [i]
  else resolve_interface_method' ms (Interface i)
    
let lookup_virtual_method ms (c:'a class_node) : 'a class_node =
  let c' =
    try resolve_method' ms c
    with NoSuchMethodError -> raise AbstractMethodError
  in
    try
      match get_method (Class c') ms with
	| ConcreteMethod _ -> c'
	| AbstractMethod _ -> raise AbstractMethodError
    with Not_found -> raise AbstractMethodError
      
let lookup_interface_method = lookup_virtual_method
  
let overrides_methods ms c =
  let result = ref [] in
    match c.c_super with
      | None -> []
      | Some c ->
	  let sc = ref c in
	    try
	      while true do
		let c = resolve_method' ms c
		in
		  result := c::!result;
		  sc :=
		    (match c.c_super with
		       | Some c -> c
		       | None -> raise NoSuchMethodError);
	      done;
	      assert false
	    with NoSuchMethodError ->
	      !result
  
(* TODO : need to be accelerated (store intermediate result for future
   use) *)
let overridden_by_methods ms c : 'a class_node list=
  let result = ref ClassMap.empty
  and not_first = ref false in
  let rec overridden_by_methods' = function
    | Class cc as c ->
        if !not_first && defines_method c ms
        then result := ClassMap.add cc.c_info.c_name cc !result;
        not_first:=true;
        List.iter
          (fun c -> overridden_by_methods' (Class c))
          cc.c_children;
    | Interface i ->
        not_first:=true;
        List.iter
          (fun i -> overridden_by_methods' (Interface i))
          i.i_children_interfaces;
        List.iter
          (fun c -> overridden_by_methods' (Class c))
          i.i_children_classes
  in
    begin
      match get_method c ms with
	| AbstractMethod {am_signature = signature}
        | ConcreteMethod {cm_signature = signature} ->
	    let mname = ms_name signature in
              if (mname = "<clinit>" || mname = "<init>") then
		raise (Invalid_argument "overridden_by_methods")
    end;
    overridden_by_methods' c;
    ClassMap.fold (fun _ ioc l -> ioc::l) !result []
      
let implements_method c ms =
  try
    match MethodMap.find ms c.c_info.c_methods with
      | ConcreteMethod _ -> true
      | AbstractMethod _ -> false
  with Not_found -> false
    
let implements_methods ms c =
  ClassMap.fold
    (fun _ i l -> resolve_all_interface_methods ms i @ l)
    c.c_interfaces
    []
    
let static_lookup_interface prog cs ms : 'a node list =
  match resolve_class prog cs with
    | Class _ -> raise IncompatibleClassChangeError
    | Interface i ->
	let il =
	  List.map
	    (fun i -> Interface i)
	    (resolve_all_interface_methods ms i)
	in
	  try
	    let c = Class (resolve_method' ms i.i_super)
	    in c::il
	  with _ -> il
	    
let static_lookup_special prog pp cs cms ms =
  match resolve_class prog cs with
    | Interface _ -> raise IncompatibleClassChangeError
    | Class c ->
	let c' = resolve_method ms c in
	  match pp.cl,c' with
	    | _, Class c2 when (ms_name cms) = "<init>" ->
		c2
	    | Class c1, Class c2 when c1 == c2 || not (extends_class c1 c2) ->
		c2
	    | _ ->
		match super_class pp.cl with
		  | None -> raise AbstractMethodError
		  | Some c -> lookup_virtual_method ms c
		      
let static_lookup_virtual prog iobj ms =
  match iobj with
    | TArray _ ->
	begin
	  match resolve_class prog java_lang_object with
	    | Class c ->
		if implements_method c ms
		then [Class c]
		else
		  let ms = JPrint.method_signature ms
		  in
		    raise (Failure ("invokevirtual on an array : "^ms))
	    | Interface _ -> raise IncompatibleClassChangeError
	end
    | TClass cs ->
	match resolve_class prog cs with
	  | Interface _ -> raise IncompatibleClassChangeError
	  | Class c ->
	      try [Class (resolve_method' ms c)]
	      with NoSuchMethodError ->
		List.map
		  (fun i -> Interface i)
		  (resolve_interface_method' ms (Class c))

let static_lookup_static program cs ms =
  let c =
    match resolve_class program cs with
      | Class c -> resolve_method ms c
      | Interface _ -> raise IncompatibleClassChangeError
  in
    match c with
      | Class c' when implements_method c' ms -> c
      | _ -> raise AbstractMethodError

		    
let static_lookup program pp =
  match get_opcode pp with
    | OpInvoke (`Virtual obj, ms) ->
        Some (static_lookup_virtual program obj ms,ms)
    | OpInvoke (`Static cs, ms) ->
        Some ([static_lookup_static program cs ms],ms)
    | OpInvoke (`Special cs, ms) ->
        Some ([Class (static_lookup_special program pp cs ms ms)],ms)
    | OpInvoke (`Interface cs, ms) ->
	Some (static_lookup_interface program cs ms,ms)
    | _ -> None
	
let static_lookup' program pp =
  match get_opcode pp with
    | OpInvoke (a,b) ->
        let cs = get_name (get_class pp)
        and ms = (get_meth pp).cm_signature
        in
          List.map
            (fun (cs,ms) -> get_first_pp program cs ms)
            (List.map
               (fun (cs,ms) -> 
                  let c = get_node program cs
                  in match get_method c ms with
                    | AbstractMethod _ -> assert false;
                    | ConcreteMethod _ -> (cs,ms))
               (List.map cms_split
		  (ClassMethodSet.elements
                     (program.static_lookup_method cs ms (a,b)))))
    | _ -> []
	
let handlers program pp =
  let ioc2c = function
    | Class c -> c
    | Interface _ -> raise IncompatibleClassChangeError
  in
    match pp.meth.cm_implementation with
      | Java code ->
	  let is_prunable exn pp =
	    match exn.e_catch_type with
	      | None -> false
	      | Some exn_name ->
		  (* an exception handler can be pruned for an instruction if:
		     - the exception handler is a subtype of Exception and
		     - the exception handler is not a subtype nor a super-type of RuntimeException and
		     - the instruction is not a method call or if
                     the instruction is a method call which is not declared to throw
		     an exception of a subtype of the handler
		  *)
		  try
		    let exn_class =
		      ioc2c (JProgram.get_node program exn_name)
		    and javalangexception = 
                      let cs = make_cn "java.lang.Exception"
                      in
			ioc2c (JProgram.get_node program cs)
		    in
		      if not (JProgram.extends_class exn_class javalangexception)
		      then false
		      else
			let javalangruntimeexception =
                          let cs = make_cn "java.lang.RuntimeException"
			  in
                            ioc2c (JProgram.get_node program cs)
			in
			  if JProgram.extends_class exn_class javalangruntimeexception
			    || JProgram.extends_class javalangruntimeexception exn_class
			  then false
			  else
                            match get_opcode pp with
                              | OpInvoke (a,b) ->
                                  let cl =
                                    (* static_lookup program pp  (* safe, but using RTA is more precise *) *)
                                    let cs = get_name (get_class pp)
                                    and ms = (get_meth pp).cm_signature
                                    in
                                      List.map cms_split
					(ClassMethodSet.elements
                                           (program.static_lookup_method cs ms (a,b)))
				  and throws_instance_of m exn =
				    (* return true if the method m is
				       declared to throw exceptions of
				       a subtype of exn *)
				    List.exists
				      (fun e ->
				         let e = JProgram.get_node program e
				         in JProgram.extends_class (ioc2c e) exn)
				      (match m with
				         | AbstractMethod {am_exceptions=exn_list}
				         | ConcreteMethod {cm_exceptions=exn_list} -> exn_list)
				  in
				    not (List.exists
					   (fun (cs,ms) ->
					      let m =
                                                JProgram.get_method
                                                  (JProgram.get_node program cs)
                                                  ms
					      in throws_instance_of m exn_class)
					   cl)
		              | _ -> true
                  with Not_found -> false
                    (* false is safe, but it would be stange to end up
                       here as it would mean that some classes have not
                       been loaded.*)
	  in
	    List.filter
	      (fun e -> e.e_start <= pp.pc && pp.pc < e.e_end && not (is_prunable e pp))
	      (Lazy.force code).c_exc_tbl
      | Native ->
	  raise (NoCode (get_name pp.cl,pp.meth.cm_signature))
	    
let exceptional_successors program pp =
  List.map (fun e -> goto_absolute pp e.e_handler) (handlers program pp)

let invoke_virtual_lookup ?(c=None) ms instantiated_classes =
  ClassMap.fold
    (fun _ instantiated_class cmmap ->
       (match c with
	  | None -> ()
	  | Some c -> assert (extends_class instantiated_class c)
       );
       let rc = resolve_method' ms instantiated_class in
       let m = get_method (Class rc) ms in
       let cm = match m with
	 | AbstractMethod _ -> assert false
	 | ConcreteMethod cm -> cm in
       let rcmsig = make_cms (rc.c_info.c_name) ms in
	 ClassMethodMap.add rcmsig (rc,cm) cmmap
    ) instantiated_classes ClassMethodMap.empty

let rec implements_interface_or_subinterface ioc i =
  let interfaces = get_interfaces ioc in
    if (interfaces = ClassMap.empty) then false
    else if (ClassMap.mem i.i_info.i_name interfaces) then true
    else
      ClassMap.fold
	(fun _ si b ->
	   b || (implements_interface_or_subinterface (Interface si) i)
	) interfaces false

let rec implements_interface_or_subinterface_transitively c i =
  let e = implements_interface_or_subinterface (Class c) i in
    if e then true
    else
      match c.c_super with
	| None -> false
	| Some sc -> implements_interface_or_subinterface_transitively sc i

let invoke_interface_lookup ?(i=None) ms instantiated_classes =
  ClassMap.fold
    (fun _ instantiated_class cmmap ->
       (match i with
	  | None -> ()
	  | Some i -> assert (implements_interface_or_subinterface_transitively
				instantiated_class i)
       );
       let rc = resolve_method' ms instantiated_class in
       let m = get_method (Class rc) ms in
       let cm = match m with
	 | AbstractMethod _ -> assert false
	 | ConcreteMethod cm -> cm in
       let rcmsig = make_cms (rc.c_info.c_name) ms in
	 ClassMethodMap.add rcmsig (rc,cm) cmmap
    ) instantiated_classes ClassMethodMap.empty
    
let invoke_special_lookup current_class called_class ms =
  let ccs = get_name current_class in
  let rc = resolve_method' ms called_class in
  let rcs = rc.c_info.c_name in
  let mname = ms_name ms in
  let c =
    if ( mname = "<init>"
	|| cn_equal rcs ccs
	|| not (extends current_class (Class rc)) ) then rc
    else
      match super_class current_class with
	| None -> assert false
	| Some sc -> resolve_method' ms sc in
  let m = get_method (Class c) ms in
    match m with
      | AbstractMethod _ -> assert false
      | ConcreteMethod cm -> (c,cm)

let invoke_static_lookup c ms =
  let rc = resolve_method' ms c in
  let m = get_method (Class rc) ms in
    match m with
      | AbstractMethod _ -> assert false
      | ConcreteMethod cm -> (rc,cm)
