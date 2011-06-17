(*
 * This file is part of SAWJA
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

open Javalib_pack
open JBasics
open JCode
open Javalib
open JProgram


(*Usefull functions for program pointers access functions*)

let ioc2c = function
  | Class c -> c
  | Interface _ -> raise IncompatibleClassChangeError

(*PP modules*)

module type PPSig = sig
  type 'a t
  exception NoCode of (class_name * method_signature)
  val get_class : 'a t -> 'a node
  val get_meth : 'a t -> 'a concrete_method
  val get_pc : 'a t -> int

  val get_pp : 'a node -> 'a concrete_method -> int -> 'a t

  (** [get_first_pp p cn ms] gets a pointer to the first instruction
      of the method [ms] of the class [cn].

      @raise Not_found if [cn] is not a class of [p], or [ms] is not a
      method of [cn].

      @raise NoCode if the method [ms] has no associated code.*)
  val get_first_pp : 'a program -> class_name -> method_signature -> 'a t
  val get_first_pp_wp : 'a node -> method_signature -> 'a t
  val goto_absolute : 'a t -> int -> 'a t
  val goto_relative : 'a t -> int -> 'a t

  val to_string : 'a t -> string
  val pprint : Format.formatter -> 'a t -> unit

  val equal : 'a t -> 'a t -> bool
  val compare : 'a t -> 'a t -> int
  val hash : 'a t -> int
end

module PP = struct
  type 'a t = {cl:'a node;
	       meth:'a concrete_method;
	       pc:int;}

  let eqc = JProgram.node_equal
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

  let throws_instance_of program pp exn_class=
    let cl =
      (* static_lookup program pp  (* safe, but using RTA is more precise *) *)
      let cs = get_name (get_class pp)
      and ms = (get_meth pp).cm_signature
      in
	List.map cms_split
	  (ClassMethodSet.elements
             (program.static_lookup_method cs ms pp.pc))
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

end

(** Common signature of PP_* modules *)
module type GenericPPSig = sig
  type code 
  type instr'
  type exception_handler
  type t = code PP.t
  exception NoCode of (class_name * method_signature)

(** {2 Data access} *)

  val get_class : t -> code node
  val get_meth : t -> code concrete_method
  val get_pc : t -> int
  val get_opcode : t -> instr'

  val get_pp : code node -> code concrete_method -> int -> t


  (** [get_first_pp p cn ms] gets a pointer to the first instruction
      of the method [ms] of the class [cn].

      @raise Not_found if [cn] is not a class of [p], or [ms] is not a
      method of [cn].

      @raise NoCode if the method [ms] has no associated code.*)
  val get_first_pp : code program -> class_name -> method_signature -> t
  val get_first_pp_wp : code node -> method_signature -> t

  val to_string : t -> string
  val pprint : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

(** {2 Navigation} *)

  val goto_absolute : t -> int -> t
  val goto_relative : t -> int -> t

  (** returns the next instruction if there is one.  If there is
      not, the behavior is unspecified (specially if compiled with
      -unsafe...) *)
  val next_instruction : t -> t

  (** returns the normal intra-procedural successors of an
      instruction*)
  val normal_successors : t -> t list

  (** returns the handlers that could catch an exception thrown from
      the current instruction*)
  val handlers : code program -> t -> exception_handler list

  (** [exceptional_successors p pp] returns the list of program points
      that may be executed after [pp] if an exception (or error) occurs
      during the execution of [pp].  Note that its uses the [throws]
      annotation of the method, which is checked by the compiler but not
      by the bytecode verifier: for security analyses, the [throws]
      annotation should be checked by the analyses. *)
  (* TODO: implement a checker which checks if that the method declares
     all the exception it may throw (except subtypes of Error and
     RuntimeException). *)
  val exceptional_successors : code program -> t -> t list
    
  (** [static_lookup program pp] returns the highest methods in the
      hierarchy that may be called from program point [pp]. All
      methods that may be called at execution time are known to
      implement or extend one of the class that this function
      returns. *)
  val static_lookup : code program -> t
    -> (code node list * method_signature) option

  (** [get_successors program cl meth] returns the possible methods
      that may be invoked from the current program point (it uses
      [static_lookup'] function).  For the static initialization,
      only the topmost class initializer is returned (and the successors
      of a clinit methods includes the clinit methods that are
      beneath). *)
  val get_successors :
    code program ->
    code node -> code concrete_method -> ClassMethodSet.t

end

module GenericPP (S : sig type t end) = struct
  type code = S.t
  type t = code PP.t

  let equal (p1:t) (p2:t) = PP.equal p1 p2

  let hash = PP.hash

  let compare = PP.compare 

  exception NoCode of (class_name * method_signature)

  let to_string = PP.to_string

  let pprint = PP.pprint

  let get_class (p1:t) : S.t JProgram.node = PP.get_class p1

  let get_meth = PP.get_meth

  let get_pc = PP.get_pc

  let get_pp = PP.get_pp

  let get_first_pp = PP.get_first_pp

  let get_first_pp_wp = PP.get_first_pp_wp

  let goto_absolute = PP.goto_absolute

  let goto_relative = PP.goto_relative


end



open PP

 



(* Lookup and resolve procedure *)

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
	if not (List.exists (node_equal c) !result)
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
  let rec resolve_all_interface_methods acc ms i =
    ClassMap.fold
      (fun _ i acc ->
	 if defines_method (Interface i) ms
	 then resolve_all_interface_methods (i::acc) ms (Interface i)
	 else resolve_all_interface_methods acc ms (Interface i))
      (get_interfaces i)
      acc
  in
  let acc = 
    if defines_method (Interface i) ms
    then [i]
    else []
  in
    List.rev 
      (resolve_all_interface_methods acc ms (Interface i))
  

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
		let c = resolve_method' ms !sc
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
	    (if defines_method (Interface i) ms
	     then [i]
	     else resolve_interface_method' ms (Interface i))
	in
	  try
	    let c = Class (resolve_method' ms i.i_super)
	    in c::il
	  with _ -> il

let static_lookup_special prog cl cs ms =
  match resolve_class prog cs with
    | Interface _ -> raise IncompatibleClassChangeError
    | Class c ->
	let c' = resolve_method ms c in
	  match cl,c' with
	    | _, Class c2 when (ms_name ms) = "<init>" ->
		c2
	    | Class c1, Class c2 when c1 == c2 || not (extends_class c1 c2) ->
		c2
	    | _ ->
		match super_class cl with
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


(* Usefull functions for get_successors functions*)

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


(* Common code for get_successors functions *)

let get_successors
    (f: 'a program -> 'a node -> 'a concrete_method -> ClassMethodSet.t -> ClassMethodSet.t)
    (program:'a program)
    (node:'a node)
    (m:'a concrete_method)
    : ClassMethodSet.t =
  let successors = ref ClassMethodSet.empty in
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
    f program node m !successors




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


let static_lookup' program pp =
  try
    let cs = get_name (PP.get_class pp)
    and ms = (PP.get_meth pp).cm_signature
    in
      List.map
        (fun (cs,ms) -> PP.get_first_pp program cs ms)
        (List.map
	   (fun (cs,ms) ->
              let c = get_node program cs
              in match get_method c ms with
                | AbstractMethod _ -> assert false;
                | ConcreteMethod _ -> (cs,ms))
	   (List.map cms_split
	      (ClassMethodSet.elements
                 (program.static_lookup_method cs ms pp.pc))))
  with Not_found -> []




module PP_BC = struct 
  include GenericPP (struct type t = JCode.jcode end)

  type instr' = JCode.jopcode
  type exception_handler =  JCode.exception_handler

  let get_code pp : jopcodes =
    match pp.meth.cm_implementation with
      | Java c -> (Lazy.force c).c_code
      | Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

  let get_opcode pp : jopcode = 
    (get_code pp).(pp.pc)
      
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

  let handlers program pp =
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
				| OpInvoke _ ->
				    throws_instance_of program pp exn_class
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
      
      
  let static_lookup program pp =
    match get_opcode pp with
      | OpInvoke (`Virtual obj, ms) ->
          Some (static_lookup_virtual program obj ms,ms)
      | OpInvoke (`Static cs, ms) ->
          Some ([static_lookup_static program cs ms],ms)
      | OpInvoke (`Special cs, ms) ->
          Some ([Class (static_lookup_special program pp.cl cs ms)],ms)
      | OpInvoke (`Interface cs, ms) ->
	  Some (static_lookup_interface program cs ms,ms)
      | _ -> None




  (** returns the possible methods that may be invoked from the current
      program point. For the static initialization, only the topmost
      class initializer is return, and the successors of a clinit
      methods includes the clinit methods that are beneath. *)
  let get_successors = 
    get_successors 
      (fun program node m succ -> 
	 let successors = ref succ in
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
				 (resolve_field fs' c')
			 | JCode.OpInvoke (kind,ms) ->
			     begin
			       match kind with
				 | `Static cn' ->
				     let c' = match get_node program cn' with
				       | Class c' -> c'
				       | Interface _ -> raise IncompatibleClassChangeError
				     in
				     let c' = resolve_method' ms c' in
				       (match (get_class_to_initialize node (Class c')) with
					  | None -> ()
					  | Some c ->
					      successors :=
						ClassMethodSet.add c !successors)
				 | _ -> ()
			     end;
			     let targets =
			       let pp = goto_absolute ppinit pc
			       in static_lookup' program pp
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
      )

end



module PP_IRCodeLike (Code : Cmn.CodeSig) = struct

  include GenericPP (struct type t = Code.t end)
  type instr = Code.instr
  type exception_handler = Code.exception_handler

  let get_code pp = 
    match pp.meth.cm_implementation with
      | Java c -> Code.Internal.code (Lazy.force c)
      | Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

  let get_opcode (pp:Code.t PP.t) : Code.instr = 
    (get_code pp).(pp.pc)
      
  
  let next_instruction pp =
    goto_relative pp 1    

end

module PP_BirLike (IRBir : JBir.Internal.CodeInstrSig) = struct 
      
  include PP_IRCodeLike(IRBir)

  let normal_successors pp =
    match get_opcode pp with
	   | IRBir.Goto l -> 
	       [goto_absolute pp l]
	   | IRBir.Ifd (_,l) -> 
	       [next_instruction pp; goto_absolute pp l]
	   | IRBir.Throw _ 
	   | IRBir.Return _ -> []
	   | _ -> [next_instruction pp]

  let handlers program pp =
    let ioc2c = function
      | Class c -> c
      | Interface _ -> raise IncompatibleClassChangeError
    in
      match pp.meth.cm_implementation with
	| Java code ->
	    let is_prunable exn pp =
	      match exn.IRBir.e_catch_type with
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
			if not (JProgram.extends_class 
				  exn_class javalangexception)
			then false
			else
			  let javalangruntimeexception =
                            let cs = make_cn "java.lang.RuntimeException"
			    in
                              ioc2c (JProgram.get_node program cs)
			  in
			    if JProgram.extends_class 
			      exn_class javalangruntimeexception
			      || JProgram.extends_class 
			      javalangruntimeexception exn_class
			    then false
			    else
                              match get_opcode pp with
				| IRBir.InvokeStatic _
				| IRBir.InvokeVirtual _ 
				| IRBir.InvokeNonVirtual _ ->
                                    throws_instance_of program pp exn_class
				| _ -> true
                    with Not_found -> false
                      (* false is safe, but it would be stange to end up
			 here as it would mean that some classes have not
			 been loaded.*)
	    in
	      List.filter
		(fun e -> e.IRBir.e_start <= pp.pc 
		   && pp.pc < e.IRBir.e_end 
		   && not (is_prunable e pp))
		(IRBir.Internal.exc_tbl (Lazy.force code))
	| Native ->
	    raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

  let exceptional_successors program pp =
    List.map 
      (fun e -> goto_absolute pp e.IRBir.e_handler) (handlers program pp)
      
  let static_lookup program pp =
    match get_opcode pp with
      | IRBir.InvokeVirtual (_,_,kind, ms,_) ->
	  (match kind with
	       JBir.VirtualCall obj -> 
		 Some (static_lookup_virtual program obj ms,ms)
	     | JBir.InterfaceCall cs -> 
		 Some (static_lookup_interface program cs ms,ms)
	  )
      | IRBir.InvokeStatic (_, cs, ms,_) ->
          Some ([static_lookup_static program cs ms],ms)
      | IRBir.InvokeNonVirtual (_, _, cs, ms, _) ->
          Some ([Class (static_lookup_special program pp.cl cs ms)],ms)
      | _ -> None

  let get_successors = 
    get_successors 
      (fun program node m succ -> 
	 let ppinit = get_pp node m 0 
	 and successors = ref succ in
	 let add_invoke pc =
	   let targets =
	     let pp = goto_absolute ppinit pc
	     in static_lookup' program pp
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
	 in
	   (match m.cm_implementation with
              | Native -> ()
              | Java c ->
		  Array.iteri
		    (fun pc opcode -> match opcode with
                       | IRBir.MayInit cn' ->
			   let c'= (get_node program cn') in
			     begin
                               match (get_class_to_initialize node c') with
				 | None -> ()
				 | Some c ->
				     successors := ClassMethodSet.add c !successors
			     end
                       | IRBir.InvokeVirtual _
		       | IRBir.InvokeNonVirtual _ -> 
			   add_invoke pc
		       | IRBir.InvokeStatic (_,cn',_,_) -> 
			   (match get_node program cn' with
			     | Class _ -> ()
			     | Interface _ -> 
				 raise IncompatibleClassChangeError);
			   add_invoke pc
		       | _ -> ()
		    )
		    (IRBir.Internal.code (Lazy.force c)));
	   !successors
      )

end


module PP_Bir = struct
  include PP_BirLike(JBir)
  type instr' = instr
end


module PP_A3BirLike (IRA3Bir : A3Bir.Internal.CodeInstrSig) = struct 

  include PP_IRCodeLike (IRA3Bir)

  let normal_successors pp =
    match get_opcode pp with
	   | IRA3Bir.Goto l -> 
	       [goto_absolute pp l]
	   | IRA3Bir.Ifd (_,l) -> 
	       [next_instruction pp; goto_absolute pp l]
	   | IRA3Bir.Throw _ 
	   | IRA3Bir.Return _ -> []
	   | _ -> [next_instruction pp]

  let handlers program pp =
    let ioc2c = function
      | Class c -> c
      | Interface _ -> raise IncompatibleClassChangeError
    in
      match pp.meth.cm_implementation with
	| Java code ->
	    let is_prunable exn pp =
	      match exn.IRA3Bir.e_catch_type with
		| None -> false
		| Some exn_name ->
		    (* an exception handler can be pruned for an
		       instruction if: - the exception handler is a
		       subtype of Exception and - the exception
		       handler is not a subtype nor a super-type of
		       RuntimeException and - the instruction is not a
		       method call or if the instruction is a method
		       call which is not declared to throw an
		       exception of a subtype of the handler *)
		    try
		      let exn_class =
			ioc2c (JProgram.get_node program exn_name)
		      and javalangexception =
			let cs = make_cn "java.lang.Exception"
			in
			  ioc2c (JProgram.get_node program cs)
		      in
			if not (JProgram.extends_class 
				  exn_class javalangexception)
			then false
			else
			  let javalangruntimeexception =
                            let cs = make_cn "java.lang.RuntimeException"
			    in
                              ioc2c (JProgram.get_node program cs)
			  in
			    if JProgram.extends_class 
			      exn_class javalangruntimeexception
			      || JProgram.extends_class 
			      javalangruntimeexception exn_class
			    then false
			    else
                              match get_opcode pp with
				| IRA3Bir.InvokeStatic _
				| IRA3Bir.InvokeVirtual _ 
				| IRA3Bir.InvokeNonVirtual _ ->
                                    throws_instance_of program pp exn_class
				| _ -> true
                    with Not_found -> false
                      (* false is safe, but it would be stange to end up
			 here as it would mean that some classes have not
			 been loaded.*)
	    in
	      List.filter
		(fun e -> e.IRA3Bir.e_start <= pp.pc 
		   && pp.pc < e.IRA3Bir.e_end 
		   && not (is_prunable e pp))
		(IRA3Bir.Internal.exc_tbl (Lazy.force code))
	| Native ->
	    raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

  let exceptional_successors program pp =
    List.map 
      (fun e -> goto_absolute pp e.IRA3Bir.e_handler) (handlers program pp)
      
  let static_lookup program pp =
    match get_opcode pp with
      | IRA3Bir.InvokeVirtual (_,_,kind, ms,_) ->
	  (match kind with
	       A3Bir.VirtualCall obj -> 
		 Some (static_lookup_virtual program obj ms,ms)
	     | A3Bir.InterfaceCall cs -> 
		 Some (static_lookup_interface program cs ms,ms)
	  )
      | IRA3Bir.InvokeStatic (_, cs, ms,_) ->
          Some ([static_lookup_static program cs ms],ms)
      | IRA3Bir.InvokeNonVirtual (_, _, cs, ms, _) ->
          Some ([Class (static_lookup_special program pp.cl cs ms)],ms)
      | _ -> None


  let get_successors = 
    get_successors 
      (fun program node m succ -> 
	 let ppinit = get_pp node m 0 
	 and successors = ref succ in
	 let add_invoke pc =
	   let targets =
	     let pp = goto_absolute ppinit pc
	     in static_lookup' program pp
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
	 in
	   (match m.cm_implementation with
              | Native -> ()
              | Java c ->
		  Array.iteri
		    (fun pc opcode -> match opcode with
		       | IRA3Bir.MayInit cn' -> 
			   let c'= (get_node program cn') in
			     begin
                               match (get_class_to_initialize node c') with
				 | None -> ()
				 | Some c ->
				     successors := ClassMethodSet.add c !successors
			     end
                       | IRA3Bir.InvokeVirtual _
		       | IRA3Bir.InvokeNonVirtual _ -> 
			   add_invoke pc
		       | IRA3Bir.InvokeStatic (_,cn',_,_) -> 
			   (match get_node program cn' with
			     | Class _ -> ()
			     | Interface _ -> 
				 raise IncompatibleClassChangeError);
			   add_invoke pc
                       | _ -> ()
		    )
		    (IRA3Bir.Internal.code (Lazy.force c)));
	   !successors
      )


end

module PP_A3Bir = struct 
  include PP_A3BirLike (A3Bir)
  type instr' = instr
end

module PP_BirSSA = 
  struct 
    include PP_BirLike (JBirSSA)
   
    type instr' = JBirSSA.phi_node list * instr
      
    let get_code (pp:JBirSSA.t PP.t) : JBirSSA.t =
      match pp.meth.cm_implementation with
	| Java c -> (Lazy.force c)
	| Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

    let get_opcode pp 
      : (JBirSSA.phi_node list * JBirSSA.instr) = 
    let code = (get_code pp) in
      (code.JBirSSA.phi_nodes.(pp.pc),code.JBirSSA.code.(pp.pc))
  end

module PP_A3BirSSA = 
struct 
  include PP_A3BirLike(A3BirSSA)

  type instr' = A3BirSSA.phi_node list * A3BirSSA.instr
  
  let get_code (pp:A3BirSSA.t PP.t) : A3BirSSA.t =
    match pp.meth.cm_implementation with
      | Java c -> (Lazy.force c)
      | Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

  let get_opcode (pp:A3BirSSA.t PP.t) 
      : (A3BirSSA.phi_node list * A3BirSSA.instr) = 
    let code = (get_code pp) in
      (code.A3BirSSA.phi_nodes.(pp.pc),code.A3BirSSA.code.(pp.pc))
end
