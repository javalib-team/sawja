(*
 * This file is part of SAWJA
 * Copyright (c)2010 Laurent Hubert (CNRS)
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
open Javalib
open JProgram
open Safe


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

let get_XTA_program
    ?(native_throwable=default_native_throwable)
    (field_analysis:[< `Class | `Field | `Global ])
    (program: JCode.jcode program)
    (entry_points:class_method_signature list)
    : JCode.jcode program =
  (** [get_relevant_operations program m] returns the sets of instance
      fields that are read, instance field that are written, static
      fields that are read, static fields that are written, classes
      that are instantiated and handled exceptions types in that order. *)
  let get_relevant_operations program m =
    match m.cm_implementation with
      | Native ->
          let ecfs = ClassFieldSet.empty
          in (ecfs,ecfs,ecfs,ecfs,ClassSet.empty,[],false,false)
      | Java c ->
          let ecfs = ClassFieldSet.empty in
          let instance_fields_read = ref ecfs
          and instance_fields_written = ref ecfs
          and static_fields_read = ref ecfs
          and static_fields_written = ref ecfs
          and classes_instantiated = ref ClassSet.empty
	  and handled_exceptions = 
	    let rec type_list = 
	      function
		  [] -> []
		| exc_h::r -> 
		    (match exc_h.JCode.e_catch_type with 
			 None -> [TObject
				    (TClass 
				       (make_cn "java.lang.Throwable"))]
		       | Some cn -> (TObject(TClass cn))::(type_list r))
	    in
	      type_list (Lazy.force c).JCode.c_exc_tbl
          and resolve_cfs initial_set cn fs =
            try
              let c = JControlFlow.resolve_class program cn in
              let cl = JControlFlow.resolve_field fs c in
                List.fold_left
                  (fun set c ->
                     ClassFieldSet.add
                       (get_class_field_signature (get_field c fs))
                       set)
                  initial_set
                  cl
            with NoClassDefFoundError ->
              (* if an exception occurs in resolve_class, it should be
                 because RTA has already detected that no method on
                 that class may be called and that there is no
                 instance of this class (so the get/put field must be
                 some dead code, provided that native methods have
                 been correctly analyzed). *)
              initial_set
          in 
	  let fs_of_object_type fs =
            match fs_type fs with
              | TObject _ -> true
              | TBasic _ -> false
          in
	  let array_load = ref false 
	  and array_store = ref true 
	  in
            Array.iter
              (function
                 | JCode.OpGetStatic (cn,fs) when fs_of_object_type fs ->
                     static_fields_read :=
                       resolve_cfs !static_fields_read cn fs
                 | JCode.OpPutStatic (cn,fs) when fs_of_object_type fs ->
                     static_fields_written :=
                       resolve_cfs !static_fields_written cn fs
                 | JCode.OpGetField (cn,fs) when fs_of_object_type fs ->
                     instance_fields_read :=
                       resolve_cfs !instance_fields_read cn fs
                 | JCode.OpPutField (cn,fs) when fs_of_object_type fs ->
                     instance_fields_written :=
                       resolve_cfs !instance_fields_written cn fs
                 | JCode.OpNew cn ->
                     classes_instantiated :=
                       ClassSet.add cn !classes_instantiated
		 | JCode.OpNewArray _
		 | JCode.OpAMultiNewArray _ ->  
		     classes_instantiated :=
                       ClassSet.add java_lang_object !classes_instantiated
		 | JCode.OpConst (`Class _ ) -> 
		     let cn = make_cn "java.lang.Class" in
		       classes_instantiated :=
			 ClassSet.add cn !classes_instantiated
		 | JCode.OpConst (`String _ ) -> 
		     let cn = make_cn "java.lang.String" in
		       classes_instantiated :=
			 ClassSet.add cn !classes_instantiated
		 | JCode.OpArrayLoad (`Object) -> array_load:=true
		 | JCode.OpArrayStore (`Object) -> array_store:=true
                 | _ -> ()
              )
              (Lazy.force c).JCode.c_code;
            (!instance_fields_read,!instance_fields_written,
             !static_fields_read,!static_fields_written,
             !classes_instantiated,handled_exceptions,!array_load,
	     !array_store)
  in


  (** returns a set with all the classes that extends (or implements) the class or
      interface given as parameter. *)
  let cone : 'a node -> ClassSet.t =
    let ccones = ref ClassMap.empty
    and icones = ref ClassMap.empty in
    let rec cone_class c =
      try ClassMap.find c.c_info.c_name !ccones
      with Not_found ->
        let cone_c =
          List.fold_left
            (fun acc class_node ->
               ClassSet.union acc (cone_class class_node)
            )
            (ClassSet.singleton c.c_info.c_name)
            c.c_children
        in
          ccones := ClassMap.add c.c_info.c_name cone_c !ccones;
          cone_c
    in
    let rec cone_interface i =
      try ClassMap.find i.i_info.i_name !icones
      with Not_found ->
        let cone_i =
          List.fold_left
            (fun acc inode ->
               ClassSet.union acc (cone_interface inode)
            )
            ClassSet.empty
            i.i_children_interfaces
        in
        let cone_i =
          List.fold_left
            (fun acc cnode ->
               ClassSet.union acc (cone_class cnode))
            cone_i
            i.i_children_classes
        in
          icones := ClassMap.add i.i_info.i_name cone_i !icones;
          cone_i
    in function
      | Interface i -> cone_interface i
      | Class c -> cone_class c
  in


  (* let (output,outchannel) = *)
  (*   let outchannel = open_out "XTA.pl" in *)
  (*   let out = Format.formatter_of_out_channel outchannel in *)
  (*     Format.pp_set_margin out max_int; *)
  (*     (out,outchannel) *)
  (* in *)

  let nb_bits =
    int_of_float (ceil (log
                          (float_of_int
                             (ClassMap.fold
                                (fun _ node count ->
                                   max count (cn_hash (get_name node)))
                                program.classes
                                0))
                        /. log 2.))
  in
  let module XTADom = ClassDomain.Make(struct let nb_bits = nb_bits end) in
  let module XTAGlobalDom =  struct
    (*Global domain for field (if selected)* Global domain for
      instance of Throwble * Global domain for array content*)
    type t = XTADom.t * XTADom.t * XTADom.t
    type analysisID = unit
    type analysisDomain = t
	
    let bot = (XTADom.bot,XTADom.bot,XTADom.bot)
    let bot_except_field fd =
      (fd,XTADom.bot,XTADom.bot)
    let bot_except_exception ed =
      (XTADom.bot,ed,XTADom.bot)
    let bot_except_arraycont acd =
      (XTADom.bot,XTADom.bot,acd)
    let get_field_domain ((d1,_,_):t) =
      d1
    let get_exception_domain ((_,d2,_):t) =
      d2
    let get_arraycont_domain ((_,_,d3):t) =
      d3
    let isBot (d1,d2,d3) = XTADom.isBot d1 && XTADom.isBot d2 && XTADom.isBot d3
    let join ?(modifies=ref false) (d1,d2,d3) (d1',d2',d3') =
      let m = ref false
      and m' = ref false 
      and m'' = ref false in
      let d11 = XTADom.join ~modifies:m d1 d1'
      and d22 = XTADom.join ~modifies:m' d2 d2'
      and d33 = XTADom.join ~modifies:m'' d3 d3'
      in
	if !m || !m' || !m''
	then (
	  modifies := true;
	  (d11,d22,d33))
	else
	  (d1,d2,d3)

    let join_ad ?(do_join=true) =
      ignore do_join;
      (* TODO: operations on this domain are not monotonic (because of the use
         of bet_except_* functions).  Therefore, avoiding to join with the
         previous value would be unsafe... *)
      join

    let equal ((d1,d2,d3) as v1) ((d1',d2',d3') as v2) = 
      if v1 == v2
      then true
      else (XTADom.equal d1 d1') && (XTADom.equal d2 d2') && (XTADom.equal d3 d3')
    let get_analysis () v = v
    let pprint fmt (d1,d2,d3) = 
      Format.pp_print_string fmt "Global domain for fields: ";
      XTADom.pprint fmt d1;
      Format.pp_print_string fmt "Global domain for exceptions: ";
      XTADom.pprint fmt d2;
      Format.pp_print_string fmt "Global domain for array content: ";
      XTADom.pprint fmt d3;
  end 
  in
  let module ED = Domain.Empty in
  let module XTAVar = Var.Make(Var.EmptyContext) in
  let module XTAState = State.Make(XTAVar)(XTAGlobalDom)(XTADom)(XTADom)(XTADom)(Domain.Empty) in
  let module XTAConstraints = Constraints.Make(XTAState) in

  let module XTASolver = Solver.Make(XTAConstraints) in

  let compute_csts field_analysis program node m : XTAConstraints.cst list =
    (** [refine_with_type program type_list abm] returns the some abstract value
        [abm] where the classes in [abm] that cannot be of a type of [type_list]
        have been removed. Partial applications with a program and a type_list
        are encourage as it should speed up the computation.

        As an optimization, [refine_with_type program type_list] may return
        [None] to encode a function that would otherwise always returns
        [XTADom.bot] *)
    let refine_with_type program (typs:value_type list) : (XTADom.t -> XTADom.t) option =
      let object_found = ref false in
      let return_None = ref true in
      let typs =
        List.fold_left
          (fun acc -> function
             | TBasic _ ->
                 acc
             | TObject (TArray _) ->
                 return_None := false;
		 lazy (XTADom.join
                         (Lazy.force acc)
                         (XTADom.of_set
                            (ClassSet.singleton java_lang_object)))
             | TObject (TClass cn) when cn_equal cn java_lang_object ->
                 object_found := true;
                 return_None := false;
                 acc
             | TObject (TClass cn) ->
                 return_None := false;
                 try
                   lazy (XTADom.join
                           (Lazy.force acc)
                           (XTADom.of_set
                              (cone (JControlFlow.resolve_class program cn))))
                 with NoClassDefFoundError ->
                   (* There is no such class in the program, this means that there
                      can be no instance of it (provided RTA is correct). *)
                   acc)
          (lazy XTADom.bot)
          typs
      in
        if !object_found
        then Some (function abm -> abm)
        else
          if !return_None then None
          else
            Some
              (function abm ->
                 XTADom.meet (Lazy.force typs) abm)
    in
    let refine_for_exc = 
      match refine_with_type 
	program [TObject (TClass (make_cn "java.lang.Throwable"))] with
	    None ->  assert false
	  | Some refine -> refine
    in
    let cn = get_name node
    and ms = m.cm_signature
    and successors = JCodePP.get_successors program node m in
      (* TODO : remove (node,m) from the successors as simple recursions
         (constraints from m to m) are useless *)
    let (instance_fields_read,instance_fields_written,
         static_fields_read,static_fields_written,
         classes_instantiated,handled_exceptions_vt,array_load,array_store) = get_relevant_operations program m
    in
    let current_method = `Method ((),cn,ms) in
    let cst_field_read (cn,fs) : XTAConstraints.cst =
      match field_analysis with
        | `Field ->
            let field_var = `Field ((),cn,fs)
            in
              (* prolog> *)
              (* XTA(current_method,S):-XTA(field_var,S). *)
              (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S).@]@]@." *)
              (*   XTAVar.pprint current_method XTAVar.pprint field_var; *)
              (* <prolog *)
              {XTAConstraints.dependencies = [field_var];
               XTAConstraints.target = current_method;
               XTAConstraints.transferFun =
                  (fun state ->
                     `MethodDomain (XTAState.get_field state field_var))}
        | `Class ->
            (* TODO : use refinetype to be more precise *)
            let class_var = `IOC ((),cn)
            in
              (* prolog> *)
              (* XTA(current_method,S):-XTA(class_var,S). *)
              (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S).@]@]@." *)
              (*   XTAVar.pprint current_method XTAVar.pprint class_var; *)
              (* <prolog *)
              {XTAConstraints.dependencies = [class_var];
               XTAConstraints.target = current_method;
               XTAConstraints.transferFun =
                  (fun state ->
                     `MethodDomain (XTAState.get_IOC state class_var))}
        | `Global ->
            (* TODO : use refinetype to be more precise *)
            let global_var = `Global ()
            in
              (* prolog> *)
              (* XTA(current_method,S):-XTA(global,S). *)
              (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S).@]@]@." *)
              (*   XTAVar.pprint current_method XTAVar.pprint global_var; *)
              (* <prolog *)
              {XTAConstraints.dependencies = [global_var];
               XTAConstraints.target = current_method;
               XTAConstraints.transferFun =
                  (fun state ->
		     `MethodDomain 
		       (XTAGlobalDom.get_field_domain 
			  (XTAState.get_global state global_var))
		  )}
    and cst_field_written (cn,fs) : XTAConstraints.cst option =
      match refine_with_type program [fs_type fs] with
        | None -> None
        | Some refine ->
            Some
              (match field_analysis with
                 | `Field ->
                     let field_var = `Field ((),cn,fs)
                     in
                       (* prolog> *)
                       (* XTA(field_var,S):-XTA(current_method,S),refine([fs_type fs],S). *)
                       (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S),refine([%s],S).@]@]@." *)
                       (*   XTAVar.pprint field_var *)
                       (*   XTAVar.pprint current_method *)
                       (*   (JPrint.value_type (fs_type fs)); *)
                       (* <prolog *)
                       {XTAConstraints.dependencies = [current_method];
                        XTAConstraints.target = field_var;
                        XTAConstraints.transferFun =
                           (fun state ->
                              let abm = XTAState.get_method state current_method in
                              let abf = refine abm in `FieldDomain abf)}
                 | `Class ->
                     let class_var = `IOC ((),cn)
                     in
                       (* prolog> *)
                       (* XTA(class_var,S):-XTA(current_method,S),refine([fs_type fs],S). *)
                       (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S),refine([%s],S).@]@]@." *)
                       (*   XTAVar.pprint class_var *)
                       (*   XTAVar.pprint current_method *)
                       (*   (JPrint.value_type (fs_type fs)); *)
                       (* <prolog *)
                       {XTAConstraints.dependencies = [current_method];
                        XTAConstraints.target = class_var;
                        XTAConstraints.transferFun =
                           (fun state ->
                              let abm = XTAState.get_method state current_method in
                              let abf = refine abm in `IOCDomain abf)}
                 | `Global ->
                     let global_var = `Global (())
                     in
                       (* prolog> *)
                       (* XTA(global,S):-XTA(current_method,S),refine([fs_type fs],S). *)
                       (* Format.fprintf output "@[XTA(global,S):- @[XTA(%a,S),refine([%s],S).@]@]@." *)
                       (*   XTAVar.pprint current_method *)
                       (*   (JPrint.value_type (fs_type fs)); *)
                       (* <prolog *)
                       {XTAConstraints.dependencies = [current_method];
                        XTAConstraints.target = global_var;
                        XTAConstraints.transferFun =
                           (fun state ->
                              let abm = XTAState.get_method state current_method in
                              let abf = refine abm in 
				`GlobalDomain 
				  (XTAGlobalDom.bot_except_field abf))})
    and csts_new cn : (XTAConstraints.cst*XTAConstraints.cst option) =
      (* prolog> *)
      (* XTA(current_method,cn) *)
      (* Format.fprintf output "@[XTA(%a,%s):-@[XTA(%a,_).@]@]@." *)
      (*   XTAVar.pprint current_method *)
      (*   (JPrint.class_name cn) *)
      (*   XTAVar.pprint current_method; *)
      (* <prolog *)
      let dom_of_new = XTADom.singleton cn in
      let dom_global_exc = 
	refine_for_exc dom_of_new 
      in
      let local = {XTAConstraints.dependencies = [current_method];
		   XTAConstraints.target = current_method;
		   XTAConstraints.transferFun = (fun _ -> `MethodDomain dom_of_new)}
      in
	if XTADom.isBot dom_global_exc
	then
	  (local,None)
	else
	  let cst_global = {XTAConstraints.dependencies = [current_method];
			    XTAConstraints.target = `Global ();
			    XTAConstraints.transferFun = 
	      (fun _ -> 
		 `GlobalDomain 
		   (XTAGlobalDom.bot_except_exception dom_global_exc))}
	  in
	    (local,Some cst_global)	  
    and csts_method_calls (cn,ms) : XTAConstraints.cst list =
      let callee_var = `Method ((),cn,ms)
      and callee =
        try
          match get_method (get_node program cn) ms with
            | ConcreteMethod cm -> cm
            | AbstractMethod _ -> assert false
        with Not_found -> assert false
      and caller_var = current_method
      and refine_this = refine_with_type program [TObject (TClass cn)]
      and refine_parameters = refine_with_type program (ms_args ms)
      and refine_return =
        match ms_rtype ms with
          | None
          | Some (TBasic _)
            -> None
          | Some rtype -> 
              refine_with_type program [rtype]
      in
      let cst_call refine_this refine_parameters =
        (* prolog> *)
        (* XTA(callee_var,S):-isNotStatic(callee_var),XTA(caller_var,S2),refineType([callee_class],S2),
           XTA(caller_var,S),refineType([callee_class;ms_parameters],S). *)
        (* XTA(callee_var,S):-isStatic(callee_var),XTA(caller_var,S),refineType([ms_parameters],S). *)
        (* Format.fprintf output "@[XTA(%a,S):- @[isNotStatic(%a),@ XTA(%a,S2),@ refineType([%s],S2),@ XTA(%a,S),@ refineType([%s],S).@]@]@." *)
        (*   XTAVar.pprint callee_var *)
        (*   XTAVar.pprint callee_var *)
        (*   XTAVar.pprint caller_var *)
        (*   (JPrint.class_name cn) *)
        (*   XTAVar.pprint caller_var *)
        (*   (String.concat ";" *)
        (*      ((JPrint.class_name cn)::(List.map JPrint.value_type (ms_args ms)))); *)
        (* Format.fprintf output "@[XTA(%a,S):- @[isStatic(%a),@ XTA(%a,S),@ refineType([%s],S).@]@]@." *)
        (*   XTAVar.pprint callee_var *)
        (*   XTAVar.pprint callee_var *)
        (*   XTAVar.pprint caller_var *)
        (*   (String.concat ";" (List.map JPrint.value_type (ms_args ms))); *)
        (* <prolog *)
        {XTAConstraints.dependencies = [caller_var];
         XTAConstraints.target = callee_var;
         XTAConstraints.transferFun =
            (fun state ->
               (* if callee can be called from caller, we propagate
                  the abstract value of caller to callee, while
                  refining by the type of arguments (including this)
                  that callee may take. *)
               let abm = XTAState.get_method state caller_var in
               let this =
                 (* TODO: this can be improved because if callee_var is called
                    with a virtual call and it is redefined in a sub-class, then
                    we can removed sub-classes of the class that redefine callee
                    from [this]. *)
                 if callee.cm_static
                 then XTADom.bot
                 else refine_this abm
               in
                 if (not callee.cm_static) && XTADom.is_empty this
                 then
                   (* callee is unreachable *)
                   `MethodDomain XTADom.bot
                 else
                   let params =
                     (* TODO: it is also possible to refine the
                        parameters with the corresponding stack
                        map. *)
                     refine_parameters abm
                   in
                     `MethodDomain (XTADom.join params this))}
      and cst_ret refine_this refine_return =
        (* prolog> *)
        (* XTA(caller_var,S):-XTA(callee_var,S),refineType([rtype],S). *)
        (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S),@
           refineType([%s],S).@]@]@." *)
        (*   XTAVar.pprint caller_var *)
        (*   XTAVar.pprint callee_var *)
        (* (match ms_rtype ms with None -> "V" | Some rtype ->
           JPrint.value_type rtype); *)
        (* <prolog *)
        {XTAConstraints.dependencies = [caller_var;callee_var];
         XTAConstraints.target = caller_var;
         XTAConstraints.transferFun =
            (fun state ->
               (* if callee can be called form caller, we propagate
                  the abstract value of callee to caller, while refining
                  by the return type of callee. *)
               let callee_unreachable () =
                 let ab_caller = XTAState.get_method state caller_var
                 in XTADom.is_empty (refine_this ab_caller)
               in
                 if (not callee.cm_static) && callee_unreachable ()
                 then `MethodDomain XTADom.bot
                 else
                   let ab_callee = XTAState.get_method state callee_var in
                     `MethodDomain (refine_return ab_callee))}
      in
      let refine_this =
        match refine_this with
          | None -> (fun _ -> XTADom.bot) (* should never *)
          | Some _ when callee.cm_static -> (fun _ -> XTADom.bot)
          | Some refine_this -> refine_this
      in
        match refine_parameters, refine_return with
          | None, None ->
              if callee.cm_static
              then []
              else [cst_call refine_this (fun _ -> XTADom.bot)]
          | None, Some refine_return ->
              if callee.cm_static
              then [cst_ret refine_this refine_return]
              else [cst_call refine_this (fun _ -> XTADom.bot);
                    cst_ret refine_this refine_return]
          | Some refine_parameters, None ->
              [cst_call refine_this refine_parameters]
          | Some refine_parameters, Some refine_return ->
              [cst_call refine_this refine_parameters;
               cst_ret refine_this refine_return]
    and cst_handlers handled_exceptions : XTAConstraints.cst =
      let global_var = `Global () in
      let refine = refine_with_type program handled_exceptions in
	match refine with 
	    None -> assert false
	  | Some refine -> 
	      {XTAConstraints.dependencies = [global_var];
	       XTAConstraints.target = current_method;
	       XTAConstraints.transferFun = 
		  (fun state -> 
		     let exc_dom = 
		       XTAGlobalDom.get_exception_domain 
			 (XTAState.get_global state global_var) 
		     in
			   `MethodDomain (refine exc_dom))}

    and cst_array_load () : XTAConstraints.cst =
      let global_var = `Global () in
	{XTAConstraints.dependencies = [global_var];
	 XTAConstraints.target = current_method;
	 XTAConstraints.transferFun = 
	    (fun state -> 
	       let ac_dom = 
		       XTAGlobalDom.get_arraycont_domain
			 (XTAState.get_global state global_var) 
	       in
		 `MethodDomain ac_dom)}
	  
    and cst_array_store () : XTAConstraints.cst =
      {XTAConstraints.dependencies = [current_method];
       XTAConstraints.target = `Global ();
       XTAConstraints.transferFun = 
	  (fun state -> 
	     let abm = XTAState.get_method state current_method in
	       `GlobalDomain (XTAGlobalDom.bot_except_arraycont abm))}
    in let csts = []
    in let csts =
        ClassSet.fold
          (fun cn csts -> 
	     match csts_new cn with
		 (local,None) -> local::csts
	       | (local,Some global) -> local::global::csts)
          classes_instantiated
          csts
    in let csts =
        ClassFieldSet.fold
          (fun cfs csts -> (cst_field_read (cfs_split cfs))::csts)
          instance_fields_read
          csts
    in let csts =
        ClassFieldSet.fold
          (fun cfs csts -> (cst_field_read (cfs_split cfs))::csts)
          static_fields_read
          csts
    in let csts =
        ClassFieldSet.fold
          (fun cfs csts ->
             Option.map_default
               (fun e -> e::csts)
               csts
               (cst_field_written (cfs_split cfs)))
          instance_fields_written
          csts
    in let csts =
        ClassFieldSet.fold
          (fun cfs csts ->
             Option.map_default
               (fun e -> e::csts)
               csts
               (cst_field_written (cfs_split cfs)))
          static_fields_written
          csts
    in let csts =
        ClassMethodSet.fold
          (fun cms csts -> (csts_method_calls (cms_split cms))@csts)
          successors
          csts
    in let csts =
	if List.length handled_exceptions_vt = 0
	then csts
	else
	  (cst_handlers handled_exceptions_vt)::csts
    in let csts =
	if array_load
	then (cst_array_load ())::csts
	else
	  csts
    in let csts =
	if array_store
	then (cst_array_store ())::csts
	else
	  csts
    in
      (* Format.pp_print_flush output (); *)
      (* close_out outchannel; *)
      csts
  in
    
  let get_csts field_analysis program =
    ClassMethodMap.fold
      (fun _cms (node,m) csts ->
         (compute_csts field_analysis program node m)@csts)
      program.parsed_methods
      []
  in

  let initial_state program entry_points native_throwable: XTAState.t=
    (* TODO: calculate init size on number of fields or methods of program ? *)
    let state = XTAState.bot (1,1,10000,100000,1)
    in
      List.iter
        (function `Method ((),cn,ms) ->
           XTAState.join
             state
             (`Method ((),cn,ms))
             (`MethodDomain (XTADom.empty))
        )
        entry_points;
      let init_exc_set = 
	let default_native_throwable = 
	  List.fold_left
	    (fun set cn -> 
	       ClassSet.add cn set)
	    ClassSet.empty
	    native_throwable
	in
	  ClassMap.fold 
	    ( fun cn _ set -> 
		if ClassSet.mem cn default_native_throwable
		then ClassSet.add cn set
		else set)
	    program.classes
	    ClassSet.empty
      in
	XTAState.join
	  state
	  (`Global ())
	  (`GlobalDomain (XTAGlobalDom.bot_except_exception (XTADom.of_set init_exc_set)));
	state
  in

  (* type static_lookup_invoke = class_name -> method_signature -> invoke -> ClassMethodSet.t *)
  (** [static_lookup state classmap cn ms invoke] returns the set of
      methods that may be called for a call to [invoke] from [(cn,ms)]
      with the [classmap] as [program.classes].  Note: partial
      applications with the state and the classmap are greatly
      encourage, as partial application with the caller ([cn] and
      [ms]) has it speed up the resolution. *)
  let static_lookup (* : *)
      (* static_lookup_invoke -> XTAState.t -> 'a node ClassMap.t -> static_lookup_invoke *)
      = fun _old_sli state classes ->
        let get_classes : class_name -> method_signature -> 'a class_node ClassMap.t =
          (* [get_classes cn ms] returns a the map of classes for
             which we may find instances in [(cn,ms)] (i.e. the result
             of XTA where the classes are directly accessible). This
             methods uses a cache to bring down the cost of several
             calls with the same arguments. *)
          let cache : 'a class_node ClassMap.t ClassMethodMap.t ref =
            ref ClassMethodMap.empty
          in
            fun cn ms ->
              let cms = make_cms cn ms in
                try ClassMethodMap.find cms !cache
                with Not_found ->
                  let res =
                    let abm = XTAState.get_method state (`Method ((),cn,ms))
                    in
                      if XTADom.isBot abm
                      then ClassMap.empty
                      else
                        ClassSet.fold
                          (fun cn res ->
                             match ClassMap.find cn classes with
                               | Class c -> ClassMap.add cn c res
                               | Interface _ -> raise IncompatibleClassChangeError)
                          (XTADom.to_set abm)
                          ClassMap.empty
                  in
                    cache := ClassMethodMap.add cms res !cache;
                    res
        in
          fun cn ms pp ->
            let instances = lazy (get_classes cn ms)
            and caller_c = lazy (ClassMap.find cn classes) in
	    let m = get_method (Lazy.force caller_c) ms in
	      match m with
		| AbstractMethod _ -> failwith "Can't call static_lookup on Abstract Methods"
		| ConcreteMethod cm ->
		    (match cm.cm_implementation with
		       | Native -> failwith "Can't call static_lookup on Native methods"
		       | Java code ->
			   let opcode = (Lazy.force code).JCode.c_code.(pp) in
			     (match opcode with
				| JCode.OpInvoke (`Virtual (TClass callee_cn),callee_ms) ->
				    let callee = match ClassMap.find callee_cn classes with
				      | Class c -> c
				      | Interface _ -> raise IncompatibleClassChangeError
				    in
				    let instances =
				      ClassMap.filter
					(fun c -> extends_class c callee)
					(Lazy.force instances)
				    in
				      ClassMethodMaptoSet.to_set
					(JControlFlow.invoke_virtual_lookup
					   ~c:(Some callee) callee_ms instances)
				| JCode.OpInvoke (`Virtual (TArray _),ms) ->
				    ClassMethodSet.singleton (make_cms java_lang_object ms)
				| JCode.OpInvoke (`Interface iname, callee_ms) ->
				    let callee = match ClassMap.find iname classes with
				      | Interface i -> i
				      | Class _ -> raise IncompatibleClassChangeError
				    in
				    let instances =
				      ClassMap.filter
					(fun c -> implements c callee)
					(Lazy.force instances)
				    in
				      ClassMethodMaptoSet.to_set
					(JControlFlow.invoke_interface_lookup
					   ~i:(Some callee) callee_ms instances)
				| JCode.OpInvoke (`Special cn, ms) ->
				    let callee = match ClassMap.find cn classes with
				      | Class c -> c
				      | Interface _ -> raise IncompatibleClassChangeError
				    in let (_c,cm) =
					JControlFlow.invoke_special_lookup
					  (Lazy.force caller_c) callee ms
				    in
				      ClassMethodSet.singleton cm.cm_class_method_signature
				| JCode.OpInvoke (`Static cn, ms) ->
				    let callee = match ClassMap.find cn classes with
				      | Class c -> c
				      | Interface _ -> raise IncompatibleClassChangeError
				    in let (_c,cm) =
					JControlFlow.invoke_static_lookup callee ms
				    in
				      ClassMethodSet.singleton cm.cm_class_method_signature
				| _ -> raise Not_found
			     )
		    )
  in

  let get_XTA_program xtastate program =
    let program = {
      program with
        static_lookup_method =
        static_lookup
	  program.static_lookup_method
	  xtastate
	  program.classes}
    in {program with
          parsed_methods =
          ReachableMethods.compute_reachable_methods program entry_points}
  in

  let entry_points =
    List.map
      (fun cms -> let cn,ms =cms_split cms in `Method ((),cn,ms))
      entry_points
  in
  let csts = get_csts field_analysis program
  and state = initial_state program entry_points native_throwable in
  let state =
    (* XTASolver.debug_level := 4; *)
    XTASolver.solve_constraints ~optimize_join:true program csts state (entry_points:>XTAVar.t list)
  in
    get_XTA_program state program

      
