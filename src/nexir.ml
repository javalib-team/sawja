open Javalib_pack
(*open Sawja_pack *)
open JBasics
open A3Bir

(* debut du code provenant de asite.ml *)
type non_virtual_method_call = 
  | CallStatic of class_method_signature
  | CallSpecial of class_method_signature

type virtual_method_call = 
  | CallVirtual of class_method_signature
  | CallInterface of class_method_signature    


(* fin du code provenant de asite.ml *)

exception IncompatibleClassChangeError
	  
module Formula=
  struct
      
    type condition = 
      | BinCond of [ `Eq | `Le | `Lt | `Ne | `Ge | `Gt] *  expr *  expr
      | IsInstanceOf of expr * object_type  (* object type from Javalib_pack.JBasics. *)
      | IsNotInstanceOf of expr * object_type (* TODO: add comments and take care of null *)
      | IsArrayElementInstanceOf of expr * expr (** (element variable, array variable) *)
      | IsNotArrayElementInstanceOf of expr * expr (** (element variable, array variable) *)

    type formula = 
    | Atom of condition
    | BoolVar of tvar
    | And of formula * formula
    | Or of formula * formula

          
    type check = 
      | CheckNullPointer of A3Bir.tvar
      | CheckArrayStore of A3Bir.tvar * A3Bir.tvar 
      | CheckNegativeArraySize of A3Bir.tvar
      | CheckCast of A3Bir.tvar * JBasics.object_type 
      | CheckArithmetic of A3Bir.tvar 
      | CheckArrayLowerBound of A3Bir.tvar * A3Bir.tvar
      | CheckArrayUpperBound of A3Bir.tvar * A3Bir.tvar
						

    type should_be_verified = 
      | False
      | ToBeChecked of check
      | MethodAssertion of class_method_signature
      
end
(* end of formula *)      

    

type return_kind =
  | NormalReturn
  | ExceptionalReturn		     

type instrCfg =
  | Nop
  (*r skip *)
  | AssignVar of A3Bir.var * A3Bir.expr 
      (*r [AssignVar (x,e)] assigns var [x] with the result of 
	  the evaluation of expression [e] *) 
  | AffectField of tvar * Javalib_pack.JBasics.class_name * Javalib_pack.JBasics.field_signature * tvar
  | AffectStaticField of Javalib_pack.JBasics.class_name * Javalib_pack.JBasics.field_signature * tvar
  | ArrayStore of A3Bir.tvar * A3Bir.tvar * A3Bir.tvar 
         (*r ArrayStore(t,i,v) == t[i] = v *)
  | Assume of Formula.formula  * Formula.should_be_verified
      (*r [Assume (cond,verif?)] may block the execution. It only
	  allow progress if condition [cond] returns true when evaluated.
	  If [verif?] equals [false] then the verifier just assume the property
	  holds otherwise it will try to prove it. *)
  | AllocVar of A3Bir.var * JBasics.class_name * JBasics.value_type list * A3Bir.tvar list
  | AllocArray of A3Bir.var * JBasics.value_type * A3Bir.tvar list
  | CheckLink of Javalib_pack.JCode.jopcode
  | MayInit of Javalib_pack.JBasics.class_name

  | MonitorEnter of A3Bir.tvar
  | MonitorExit of A3Bir.tvar
  | NonVirtualCall of A3Bir.var * A3Bir.var * non_virtual_method_call * A3Bir.tvar list
   (* [NonVirtualCall e  r cms args] performs a method call to
	the method found in [cms], using arguments [args]. 
        If themethod ends with an exception, this exception is put in [e], 
        otherwise if it ends normally, the result is put in [r].  
        If the methods does not return any value
	because its return type is simply [void] then we assign
	[r] to [null].  *)
  | VirtualCall of A3Bir.var * A3Bir.var * A3Bir.tvar * virtual_method_call * A3Bir.tvar list
      (*r [VirtualCall e r x cn ms args] performs a virtual
	method call using the class of the reference in [x] and the
	method signature [(cn,ms)].  The method lookup may fail
	because of interfaces.  *)
  | UncaughtExceptionAfterMethodCall
      (* r [UncaughtExceptionAfterMethodCall x] special trick for exceptions thrown
	 by a method call *)
  | NormalReturnAfterMethodCall
      (* r [NormalReturnAfterMethodCall x] special trick for exceptions thrown
	   by a method call *)


       
type pc=int 
type t = {
  params : A3Bir.tvar list;
  code : (instrCfg*pc) list array; (*the pc represent the instruction following
                                    the current instruction.*)
  start_pc : pc;
  normal_return : var ;   
  normal_end_pc : pc;      (* where each edge that model normal returns should go *)
  exceptional_return : var;
  exceptional_end_pc : pc; (* where each edge that model uncaught exceptions should go *)
  error_pc : pc;           (* where each edge that model errors should go *)
  cfg2Bir : pc Ptmap.t; (** Map which for a cfg pc give its original pc in JcA3Bir. *) 
  (* Some properties:
      -- [ code.(normal_end_pc) = [] ]
      -- for every [pc1], [pc2] such that [code.(pc)] contains [(Return NormalReturn,pc2)],
         [pc2] must be equal to [normal_end_pc]
   *)
}

open Formula
       
let ifcond2cond (c,v1,v2) = 
  match c with
  | `Eq -> BinCond (`Eq,Var v1,Var v2)
  (* v1 >= v2 <=> v2 <= v1 *)
  | `Ge -> BinCond (`Le,Var v2,Var v1)
  (* v1 > v2 <=> v2 < v1 *)
  | `Gt -> BinCond (`Lt,Var v2,Var v1)
  | `Le -> BinCond (`Le,Var v1,Var v2)
  | `Lt -> BinCond (`Lt,Var v1,Var v2)
  | `Ne -> BinCond (`Ne,Var v1,Var v2)
		     
(* Generation of fresh pcs *)

let fresh_pc = ref (-1)

let init_fresh_pc (pc:int) :unit =
  fresh_pc := pc

let clear_fresh_pc _ = fresh_pc := -1

let get_fresh_pc unit : int = 
  let cur_fresh_pc = !fresh_pc in
    incr fresh_pc;
    cur_fresh_pc

let negate_condition = function
  | BinCond (cond,e1,e2) ->
     begin
       match cond with
       | `Eq -> BinCond(`Ne,e1,e2)
       | `Ne -> BinCond(`Eq,e1,e2)
       | `Le -> BinCond(`Lt,e2,e1)
       | `Lt -> BinCond(`Le,e2,e1)
       | `Gt -> BinCond(`Le,e1,e2)
       | `Ge -> BinCond(`Lt,e1,e2)
     end
  | IsInstanceOf (v,ot) -> IsNotInstanceOf (v,ot)
  | IsNotInstanceOf (v,ot) -> IsInstanceOf (v,ot)
  | IsArrayElementInstanceOf (v_elt,v_array) -> 
     IsNotArrayElementInstanceOf (v_elt,v_array)
  | IsNotArrayElementInstanceOf (v_elt,v_array) -> 
     IsArrayElementInstanceOf (v_elt,v_array)

let rec transform_formula = function
  | A3Bir.Atom(cmp,tv1,tv2) -> Atom(BinCond(cmp,Var tv1,Var tv2))
  | A3Bir.BoolVar tv -> BoolVar tv
  | A3Bir.And(f1,f2) -> And(transform_formula f1,transform_formula f2)
  | A3Bir.Or(f1,f2) -> Or(transform_formula f1,transform_formula f2)

       
	   
	   
let transform_handlers tcode old_pc current_pc exceptional_return exceptional_end_pc (var:A3Bir.tvar) = 
  (* [new_current_pc] represent the pc from which an uncaught exception
     could be thrown if could_be_uncaught is false.*)
  let (new_current_pc,new_instrs,could_be_uncaught,_) = 
    List.fold_left
      (fun (current_pc,new_instrs,could_be_uncaught,finally_done) eh -> 
         (* check if the handler covers our old instruction at old pc
            and if a finally block was not used before (in this later
            case it will catch every possible exception and no other
            handler will be used) *)
         if not finally_done && eh.A3Bir.e_start <= old_pc && eh.A3Bir.e_end > old_pc
         then
           (* check if it catch the exception contained in var *)
           match eh.A3Bir.e_catch_type with
               None -> (* Finally block *) 
                 (-1 (* will not be used anymore *),
                  (current_pc,
                   [(AssignVar (eh.A3Bir.e_catch_var,Var var), eh.A3Bir.e_handler)])::new_instrs,
                   false, true)
                   (* TODO: we must check if it it the
                      java.lang.Throwable class since it is equivalent
                      to a finally block !? *)
             | Some cn -> 
                 (* condition to enter in the handler *)
                 let condition = Formula.IsInstanceOf (Var var,TClass cn) in
                   (* a fresh pc to assign the catch variable of the
                      handler with the variable containing the exception
                      thrown before going to the handler code *)
                 let before_handler_pc = get_fresh_pc ()
                   (* a fresh pc to go in the case the exception is not
                      caught by the handler, it is the new current pc
                      for our generated instructions (since we can go in
                      next handlers only if we don't go in this one) *)
                 and new_current_pc = get_fresh_pc () in
                   (new_current_pc, 
                    (current_pc,
                     [(Assume (Atom condition,False), before_handler_pc);
                      (Assume (Atom (negate_condition condition),False), new_current_pc)])::
                      (before_handler_pc,
                       [AssignVar (eh.A3Bir.e_catch_var, Var var), eh.A3Bir.e_handler])
                    ::new_instrs,could_be_uncaught,false)
         else current_pc,new_instrs,could_be_uncaught,finally_done
      )
      (current_pc,[],true,false)
      (A3Bir.exc_tbl tcode)
  in
    if could_be_uncaught
    then
      (* If the exception could be uncaught after trying to use all
         handlers, we must have an exceptionnal return going from the
         current pc after handlers assumptions (=old_pc if no handlers
         were available) *)
      (assert (new_current_pc <> -1);
       let new_instrs = 
         (new_current_pc,
          [AssignVar(exceptional_return,Var var),exceptional_end_pc])
         ::new_instrs
       in
         new_instrs)
    else
      new_instrs


	
let transform_invoke_static old_pc exc_return exc_end_pc tcode origin_retvar cms args 
    =
  (* generate fresh var if it was a void return: this var
     will never be used but simplify further treatments *)
  (* Position of the next pc to be added. Currently an instruction
     of the JcA3Bir representation at program point pp will be
     translated at the same program point pp in the cfg
     representation. 

     CAUTION: if modified here please modify it to in
     transform_code !!!  *)
  let cur_pc = old_pc in
  let ret_var = 
    match origin_retvar with
        None -> A3Bir.make_fresh_var tcode 
      | Some x -> x
  in
    (* exceptional return var *)
  let exc_var = A3Bir.make_fresh_var tcode in
  let return_pc = get_fresh_pc ()
  and exc_return_pc = get_fresh_pc () in
  let handlers_on_exceptional_return =
    transform_handlers tcode old_pc  exc_return_pc exc_return exc_end_pc ((TObject (TClass (make_cn "java.lang.Throwable"))) ,exc_var)
		  
  in
    (cur_pc, 
     [ NonVirtualCall (exc_var,ret_var,CallStatic cms,args), 
     return_pc ])::
      (return_pc, 
       [ (NormalReturnAfterMethodCall, cur_pc+1);
         (UncaughtExceptionAfterMethodCall,
          exc_return_pc)])
    :: handlers_on_exceptional_return

	
	
let rec transform_code (code:A3Bir.t) (normal_ret:A3Bir.var) (normal_end_pc:int) (exc_ret:A3Bir.var) (exc_end_pc:int) (instrs:A3Bir.instr array) (last_old_pc:int) (old_pc:int) new_code cfg2Bir =
  let cur_pc = old_pc in
  (*Printf.printf "last_old_pc = %d, pc = %d, instr = %s\n" last_old_pc cur_pc (A3Bir.print_instr instrs.(old_pc));*)
  
  let new_instrs =
    match instrs.(old_pc) with
    | A3Bir.Nop -> [(cur_pc, [(Nop, cur_pc+1)])]
    | A3Bir.AffectVar (v,e) -> [(cur_pc, [AssignVar (v, e), cur_pc+1])]
    | A3Bir.AffectArray (x,i,v) -> [(cur_pc, [ArrayStore(x,i, v), cur_pc+1])]
    | A3Bir.AffectField ((xt,x),c,fs,(yt,y)) ->
       [(cur_pc, [AffectField ((xt,x),c,fs,(yt,y)), cur_pc+1])]
    | A3Bir.AffectStaticField (c,fs,(yt, y)) ->
       [(cur_pc, [AffectStaticField (c, fs, (yt,y)), cur_pc+1])]
    | A3Bir.Goto pc -> 
       [(cur_pc, [Nop, pc])]
    | A3Bir.Ifd (g,pc) ->
       let condition = ifcond2cond g in
       [(cur_pc, 
         [(Assume (Atom condition,False), pc);
          (Assume (Atom (negate_condition condition),False),cur_pc+1)])]
    | A3Bir.Throw var -> 
       transform_handlers code old_pc cur_pc exc_ret exc_end_pc var
    | A3Bir.Return v_opt -> 
       begin
	 match v_opt with
         | None -> 
          (* For void return we return the 0 constant (in a  fresh var) *)
	    [(cur_pc, [AssignVar (normal_ret,Const (`Int (Int32.of_int 0))), normal_end_pc])]
         | Some var -> 
            [(cur_pc, [AssignVar (normal_ret, Var var), normal_end_pc])]
       end
    | A3Bir.MayInit (c) -> [(cur_pc, [MayInit (c), cur_pc+1])]

    | MonitorEnter (tv) -> [(cur_pc, [MonitorEnter (tv), cur_pc+1])] 
    | MonitorExit (tv)  -> [(cur_pc, [MonitorExit (tv), cur_pc+1])] 			     

    | A3Bir.New (x, ct, l, args) ->
       [(cur_pc, [AllocVar (x, ct, l, args), cur_pc+1])] 
    | A3Bir.NewArray (x,t,el) -> 
       [(cur_pc, [AllocArray (x, t, el), cur_pc+1])] 
	 
    | A3Bir.InvokeStatic (x,c,ms,args) ->
       let cms = make_cms c ms in 
       transform_invoke_static  old_pc exc_ret exc_end_pc code x cms args
				
    | A3Bir.InvokeVirtual(x,y,k,ms,args) ->
       let virtual_method_call = 
         match k with
         | A3Bir.VirtualCall(ot) ->
	    begin
	      match ot with
	      | TClass cn -> CallVirtual (make_cms cn ms) 
	      | _ -> assert(false)
	    end
         | A3Bir.InterfaceCall(cn)  -> CallInterface (make_cms cn ms)
       in
       (* generate fresh var if it was a void return: this var
               will never be used but simplify further treatments *)
       let ret_var = 
         match x with
           None -> A3Bir.make_fresh_var code 
         | Some x -> x
       in
       (* exceptional return var *)
       let exc_var = A3Bir.make_fresh_var code in
       let return_pc = get_fresh_pc ()
       and exc_return_pc = get_fresh_pc () in
       let handlers_on_exceptional_return =
         transform_handlers code old_pc exc_return_pc exc_ret exc_end_pc  ((TObject (TClass (make_cn "java.lang.Throwable"))) ,exc_var)
       in
       (cur_pc, 
        [ VirtualCall (exc_var,ret_var, y,virtual_method_call,args), return_pc ])::
         (return_pc, 
          [ (NormalReturnAfterMethodCall, cur_pc+1);
            (UncaughtExceptionAfterMethodCall, exc_return_pc)])::
           handlers_on_exceptional_return             
             
             
    | A3Bir.InvokeNonVirtual (x, y, c, ms, args) ->
       let cms = make_cms c ms in 
       (* add receiver in standard arguments for invokespecial *)
       let args = y::args in
       (* generate fresh var if it was a void return: this var
               will never be used but simplify further treatments *)
       let ret_var = 
         match x with
           None -> A3Bir.make_fresh_var code 
         | Some x -> x
       in
       (* exceptional return var *)
       let exc_var = A3Bir.make_fresh_var code in
       let return_pc = get_fresh_pc ()
       and exc_return_pc = get_fresh_pc () in
       let handlers_on_exceptional_return =
	 transform_handlers code old_pc exc_return_pc exc_ret exc_end_pc ((TObject (TClass (make_cn "java.lang.Throwable"))) ,exc_var)
	 
       in
       (cur_pc, 
        [ NonVirtualCall (exc_var,ret_var, (CallSpecial cms),args), 
          return_pc ])::
         (return_pc, 
          [ (NormalReturnAfterMethodCall, cur_pc+1);
            (UncaughtExceptionAfterMethodCall, exc_return_pc)])::
           handlers_on_exceptional_return             
             
| A3Bir.Check c -> 
       begin
	 match c with
	 | A3Bir.CheckNullPointer e -> 
            [(cur_pc,[Assume (Atom (BinCond (`Ne,Var e,Const `ANull)),
                              ToBeChecked (CheckNullPointer e)),cur_pc+1])]
	 | A3Bir.CheckArrayBound (a,i) -> 
            let assume_pc = 
              get_fresh_pc ()
            in
            [(cur_pc,
              [Assume (Atom (BinCond (`Le,Const (`Int (Int32.of_int 0)),Var i)),
                       ToBeChecked (CheckArrayLowerBound (a, i))), assume_pc]);
             (assume_pc,
              [Assume (Atom (BinCond (`Lt,Var i, (Unop(ArrayLength, a)))),
                       ToBeChecked (CheckArrayUpperBound(a, i))), cur_pc+1])
            ]
	 | A3Bir.CheckArrayStore (a,v) -> 
            [cur_pc,[Assume (Atom (IsArrayElementInstanceOf (Var v,Var a)), 
                             ToBeChecked (CheckArrayStore( a,v))), cur_pc+1]]
	 | A3Bir.CheckNegativeArraySize e -> 
            [(cur_pc,
              [Assume (Atom (BinCond (`Le,Const (`Int (Int32.of_int 0)),Var e)),
                       ToBeChecked (CheckNegativeArraySize e)), cur_pc+1])]
	 | A3Bir.CheckCast (e,t) -> 
            let notnull_pc = get_fresh_pc () in
            (* succeeds if e is null or if e is not null &&
                       e instanceof t: we only need to prove that e
                       instanceof t if e maybe not null *)
            [(cur_pc,[Assume (Atom (BinCond (`Eq,Var e,Const `ANull)),False),cur_pc+1; 
                      Assume (Atom (BinCond (`Ne,Var e,Const `ANull)),False),notnull_pc]);
             (notnull_pc,[Assume (Atom (IsInstanceOf (Var e,t)),
                                  ToBeChecked (CheckCast(e,t))), cur_pc+1])]
	 | A3Bir.CheckArithmetic e -> 
            [(cur_pc,[Assume (Atom (BinCond (`Ne,Const (`Int (Int32.of_int 0)),Var e)),
                              ToBeChecked (CheckArithmetic e)), cur_pc+1])]
         | A3Bir.CheckLink opcode ->
            [(cur_pc,[ CheckLink (opcode), cur_pc+1])]
       end

| Formula (cms, f) ->
  [(cur_pc, [Assume(transform_formula f,MethodAssertion cms),cur_pc+1])]
  
  in
  let (new_code, cfg2Bir) =
    List.fold_left
      (fun (code, cfg2Bir) (new_pc, instrs) ->
       (* It must be the first time we add code at this pc *)
       assert (not (Ptmap.mem new_pc code)); 
         (Ptmap.add new_pc instrs code,
          Ptmap.add new_pc old_pc cfg2Bir)
      )
      (new_code, cfg2Bir)
      new_instrs
  in
    if old_pc == last_old_pc
    then
      (new_code, cfg2Bir)
    else
	transform_code  code normal_ret normal_end_pc exc_ret exc_end_pc instrs last_old_pc (old_pc+1) new_code cfg2Bir
        (*function transform_code end *)


let transform (code:A3Bir.t ): t =
  (* get the array of instructions *)
  let instrs = A3Bir.code code in
  
  (* get the list of parameters [(type, param) ...] *)
  let params = A3Bir.params code in

  let last_old_pc = Array.length instrs -1 in (* ?? *)
  let new_pc = last_old_pc +1 in
  let start_pc = new_pc
  and normal_end_pc = new_pc + 1
  and normal_return = A3Bir.make_fresh_var code 
  and exceptional_end_pc = new_pc + 2
  and error_pc = new_pc + 3 in
  let _init_fresh_pc = init_fresh_pc (new_pc + 4) in
  let exceptional_return = A3Bir.make_fresh_var code in

  (*  Printf.printf "last_old_pc = %d\nnew_cp = %d\n normal_end_pc = %d\n exceptional_end_pc = %d\nerror_pc = %d\n" last_old_pc start_pc normal_end_pc exceptional_end_pc error_pc;*)

  let (code_map, cfg2Bir) =     (* LG: ??? *)
    if Array.length instrs <> 0
    then
      let code_map_init = 
        Ptmap.add start_pc [Nop, 0] 
          (Ptmap.add normal_end_pc [] 
             (Ptmap.add exceptional_end_pc []
                (Ptmap.add error_pc [] Ptmap.empty)))
      in
      transform_code code normal_return normal_end_pc exceptional_return exceptional_end_pc instrs last_old_pc 0 code_map_init Ptmap.empty
    else
      (Ptmap.empty, Ptmap.empty)
  in
  (*Printf.printf "Ptmap.cardinal = %d\n"       (Ptmap.cardinal code_map);*)
  let array_code =
    Array.init
      (Ptmap.cardinal code_map)
      (fun pc ->
       (*Printf.printf "pc = %d\n" pc;*)
       Ptmap.find pc code_map)
  in
  {
    params = params;
    code = array_code;
    start_pc = start_pc;
    normal_return =  normal_return;
    normal_end_pc = 0;
    exceptional_return = exceptional_return ;
    exceptional_end_pc = exceptional_end_pc;
    error_pc = error_pc; 
    cfg2Bir = cfg2Bir
  }

 let string_of_cmp = function
    | `Eq -> " = "
    | `Le -> " <= "
    | `Lt -> " < "
    | `Ne -> " != "
    | `Gt -> " > "
    | `Ge -> " >= "
      
 let rec valuetype2string (ot:object_type) : string =
   match ot with
	| TClass cs -> cn_name cs
	| TArray vt ->
	   begin
	     match vt with
	     | TBasic _ -> JPrint.value_type vt
	     | TObject ot' -> let res = valuetype2string ot' in
			      res ^ "[]"
					       
	   end

	       
 let string_of_condition (c:Formula.condition) : string =
   match c with
   | BinCond (cmp, e1, e2) -> print_expr e1 ^ string_of_cmp cmp ^ print_expr e2
   | IsInstanceOf (e, ot) -> print_expr e ^ " instance of " ^ valuetype2string ot
   | IsNotInstanceOf(e, ot) -> print_expr e ^ " not instance of " ^ valuetype2string ot
   | IsArrayElementInstanceOf(v, a) -> print_expr v ^ " (array)instance of " ^ print_expr a ^ "[]"
   | IsNotArrayElementInstanceOf(v, a) -> print_expr v ^ " not (array)instance of " ^ print_expr a ^ "[]"

 let rec string_of_formula (f:Formula.formula) : string =
   match f with 
   | Atom (condition) -> string_of_condition condition
   | BoolVar(tv)->  print_tvar tv
   | And (f1, f2) -> "(" ^ string_of_formula f1 ^ ") and (" ^ string_of_formula f2 ^ ")"
   | Or (f1, f2) -> "(" ^ string_of_formula f1 ^ ") or (" ^ string_of_formula f2  ^ ")"

     

let string_of_check   = function
    CheckNullPointer e -> Printf.sprintf "CheckNotNull (%s != null);" (print_tvar e)
  | CheckArrayLowerBound (a,i) -> Printf.sprintf "CheckArrayLowerBound (%s[%s]);"  (print_tvar a) (print_tvar i)
  | CheckArrayUpperBound (a,i) -> Printf.sprintf "CheckArrayUpperBound (%s[%s]);"  (print_tvar a) (print_tvar i)
  | CheckArrayStore (a,v) -> Printf.sprintf "CheckArrayStore (%s[] <- %s);"  (print_tvar a) (print_tvar v)
  | CheckNegativeArraySize e -> Printf.sprintf "CheckArraySize (%s >= 0);" (print_tvar e)
  | CheckCast (e,t) -> 
     Printf.sprintf "CheckCast ((type?)%s);" (*(JBasics.string_of_object_type t)*) (print_tvar e) 
  | CheckArithmetic e -> Printf.sprintf "CheckArithmetic (%s != 0);" (print_tvar e)
	    
   

let string_of_instr (instr:instrCfg) : string =
  match instr with
  | Nop -> "Nop"
  | AssignVar (v, e) -> var_name v^" = "^ print_expr e
  | AffectField (x, c, fs, y) -> print_tvar  x^".< "^cn_name c^":"^fs_name fs^"> := "^print_tvar y
  | AffectStaticField (c, fs, e) -> "<"^ cn_name c ^":"^fs_name fs^"> := " ^ print_tvar e 
  | ArrayStore (t, i, v) -> print_tvar t^"[ "^ print_tvar i ^"] = "^print_tvar v
  | AllocVar (x, c, tl, args) -> assert(List.length tl == List.length args);
				 var_name x
				 ^" = new "
				 ^cn_name c 
				 ^"(" ^
				   (if List.length args = 0
				    then ""
				    else
				      let idx = ref 0 in
				      (JUtil.print_list_sep ", "
							    (fun tv ->
							     let typeArg = JPrint.value_type (List.nth tl !idx) in
							     incr(idx);
							     let (t, v ) = tv in
							     Printf.sprintf "%s:%s"  (var_name v) typeArg)
							    args)
				   )
				   ^")"
  (* LG: les types des arguments contenus dans tl et dans les tvar ne sont pas identiques...*)
					
  | AllocArray (v, vt, vl) -> var_name v
			      ^" = new "
			      ^(JPrint.value_type vt)
			      ^"["
			      ^(JUtil.print_list_sep "]["
						     (fun tv ->
						      Printf.sprintf "%s" (print_tvar tv))
						     vl)
			      ^"]"
  | Assume (c, sbv) ->
     let condition = string_of_formula c in
     begin
       match sbv with
       | False -> "assume("^condition^")"
       | ToBeChecked check -> 
          "assert("^condition^", Check("^string_of_check check^"))"
       | MethodAssertion cms ->
         let (cn,ms) = cms_split cms in 
         "assert("^condition^", Check("^cn_name cn^"."^ms_name ms^"))"
     end
  | CheckLink(opcode) -> "CheckLink(??)"
  | MayInit(c) -> "MayInit("^cn_name c ^")"
  | MonitorEnter(v) -> "MonitorEnter("^print_tvar v ^ ")"
  | MonitorExit (v) -> "MonitorExit("^print_tvar v ^ ")"
  | NonVirtualCall (exc_var,ret_var, nvc,args) ->
     begin
       match nvc with
       | CallStatic(cms)
       | CallSpecial(cms) ->
	  let c, ms = cms_split cms in
	  Printf.sprintf "(%s,%s) = (%s).%s(%s)"
			 (var_name exc_var)
			 (var_name ret_var)
			 (cn_name c)
			 (ms_name ms)
			 (if List.length args = 0
			  then ""
			  else
			    (JUtil.print_list_sep ", "
						  (fun tv -> Printf.sprintf "%s"  (print_tvar tv))
						  args)
			 )
     end
  | NormalReturnAfterMethodCall -> "NormalReturnAfterMethodCall"
  | UncaughtExceptionAfterMethodCall -> "UncaughtExceptionAfterMethodCall"
  | VirtualCall (exc_var, ret_var, v,  vmc, args) ->
     begin
       match vmc with
       | CallVirtual (cms) 
       | CallInterface (cms) ->
	  let c, ms = cms_split cms in
	  Printf.sprintf "(%s,%s) = (%s).%s(%s)"
			 (var_name exc_var)
			 (var_name ret_var)
			 (cn_name c)
			 (ms_name ms)
			 (if List.length args = 0
			  then ""
			  else
			    (JUtil.print_list_sep ", "
						  (fun tv -> Printf.sprintf "%s"  (print_tvar tv))
						  args)
			 )
     end
     
  
let cfg2dot (filename:string) (cms:JBasics.class_method_signature) t : unit=
  let outchan  = open_out (filename) in
  output_string outchan ("digraph "^ms_name (snd (cms_split cms))^"{\n");
  Array.iteri
    (fun i l ->
     List.iter
       (fun (instr, pc) ->
	output_string outchan (Printf.sprintf "%d -> %d [label=\"%s\"];\n" i pc (string_of_instr instr) );
       (*Printf.printf "%d : %s : %d\n" i (string_of_instr instr) pc*)
       )
       l;
    )

   t.code;
  
  output_string outchan "}\n";
  close_out outchan
