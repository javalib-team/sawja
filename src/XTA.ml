
open JBasics
open Javalib
open JProgram
open Safe

let get_XTA_program
    (field_analysis:[< `Class | `Field | `Global ])
    (program: JCode.jcode program)
    (entry_points:class_method_signature list)
    : JCode.jcode program =

  (** [get_relevant_operations program m] returns the sets of instance fields that
      are read, instance field that are written, static fields that are read,
      static fields that are written and classes that are instantiated, in that
      order. *)
  let get_relevant_operations program m =
    match m.cm_implementation with
      | Native ->
          let ecfs = ClassFieldSet.empty
          in (ecfs,ecfs,ecfs,ecfs,ClassSet.empty)
      | Java c ->
          let ecfs = ClassFieldSet.empty in
          let instance_fields_read = ref ecfs
          and instance_fields_written = ref ecfs
          and static_fields_read = ref ecfs
          and static_fields_written = ref ecfs
          and classes_instantiated = ref ClassSet.empty
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
              (* if an exception occurs in resolve_class, it should be because RTA
                 has already detected that no method on that class may be called
                 and that there is no instance of this class (so the get/put field
                 must be some dead code, provided that native methods have been
                 correctly analyzed). *)
              initial_set
          and fs_of_object_type fs =
            match fs_type fs with
              | TObject _ -> true
              | TBasic _ -> false
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
                 | _ -> ()
              )
              (Lazy.force c).JCode.c_code;
            (!instance_fields_read,!instance_fields_written,
             !static_fields_read,!static_fields_written,
             !classes_instantiated)
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
  let module ED = Domain.Empty in
  let module XTAVar = Var.Make(Var.EmptyContext) in
  let module XTAState = State.Make(XTAVar)(XTADom)(XTADom)(XTADom)(XTADom)(Domain.Empty) in
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
    (* TODO : also return a boolean so no constraint is built when
       refine_with_type always return Bot *)
    let refine_with_type program (typs:value_type list) : (XTADom.t -> XTADom.t) option =
      let object_found = ref false in
      let return_None = ref true in
      let typs =
        List.fold_left
          (fun acc -> function
             | TBasic _ ->
                 acc
             | TObject (TArray _) ->
                 object_found := true;
                 return_None := false;
                 acc
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

    let cn = get_name node
    and ms = m.cm_signature
    and successors = JControlFlow.get_successors program node m in
    let (instance_fields_read,instance_fields_written,
         static_fields_read,static_fields_written,
         classes_instantiated) = get_relevant_operations program m
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
                     `MethodDomain (XTAState.get_global state global_var))}
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
                              let abf = refine abm in `GlobalDomain abf)})
    and cst_new cn : XTAConstraints.cst =
      (* prolog> *)
      (* XTA(current_method,cn) *)
      (* Format.fprintf output "@[XTA(%a,%s):-@[XTA(%a,_).@]@]@." *)
      (*   XTAVar.pprint current_method *)
      (*   (JPrint.class_name cn) *)
      (*   XTAVar.pprint current_method; *)
      (* <prolog *)
      {XTAConstraints.dependencies = [current_method];
       XTAConstraints.target = current_method;
       XTAConstraints.transferFun = (fun _ -> `MethodDomain (XTADom.singleton cn))}
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
               (* if callee can be called from caller, we propagate the abstract
                  value of caller to callee, while refining by the type of
                  arguments (including this) that callee may take. *)
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
                     (* TODO: it is also possible to refine the parameters with
                        the corresponding stack map. *)
                     refine_parameters abm
                   in
                     `MethodDomain (XTADom.join params this))}
      and cst_ret refine_this refine_return =
        (* prolog> *)
        (* XTA(caller_var,S):-XTA(callee_var,S),refineType([rtype],S). *)
        (* Format.fprintf output "@[XTA(%a,S):- @[XTA(%a,S),@ refineType([%s],S).@]@]@." *)
        (*   XTAVar.pprint caller_var *)
        (*   XTAVar.pprint callee_var *)
        (*   (JPrint.value_type rtype); *)
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
    in let csts = []
    in let csts =
        ClassSet.fold
          (fun cn csts -> (cst_new cn)::csts)
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

  let initial_state _program entry_points : XTAState.t=
    let state = XTAState.bot ()
    in
      List.iter
        (function `Method ((),cn,ms) ->
           XTAState.join
             state
             (`Method ((),cn,ms))
             (`MethodDomain (XTADom.empty))
        )
        entry_points;
      state
  in

  (* type static_lookup_invoke = class_name -> method_signature -> invoke -> ClassMethodSet.t *)
  (** [static_lookup state classmap cn ms invoke] returns the set of methods that
      may be called for a call to [invoke] from [(cn,ms)] with the [classmap] as
      [program.classes].  Note: partial applications with the state and the
      classmap are greatly encourage, as partial application with the caller ([cn]
      and [ms]) has it speed up the resolution. *)
  let static_lookup (* : *)
      (* static_lookup_invoke -> XTAState.t -> 'a node ClassMap.t -> static_lookup_invoke *)
      = fun _old_sli state classes ->
        let get_classes : class_name -> method_signature -> 'a class_node ClassMap.t =
          (* [get_classes cn ms] returns a the map of classes for which we may find
             instances in [(cn,ms)] (i.e. the result of XTA where the classes are
             directly accessible). This methods uses a cache to bring down the cost of
             several calls with the same arguments. *)
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

  let get_XTA_program xtastate program = {
    program with
      static_lookup_method =
      static_lookup
	program.static_lookup_method
	xtastate
	program.classes}
  in

  let entry_points =
    List.map
      (fun cms -> let cn,ms =cms_split cms in `Method ((),cn,ms))
      entry_points
  in
  let csts = get_csts field_analysis program
  and state = initial_state program entry_points in
  let state =
    XTASolver.solve_constraints program csts state (entry_points:>XTAVar.t list)
  in get_XTA_program state program

