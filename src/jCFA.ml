(*
 * This file is part of SAWJA
 * Copyright (c)2013 Pierre Vittet (INRIA)
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
open JBir
open Javalib

open JCFADom
open JCFAOptions
open Safe
open JType


module AbField = AbFSet
module AbVar = AbVSet
module CFASolver = Solver.Make(CFAConstraints) 


  (* This is a 'virtual' field that we use to represent abstraction of array
  * elements as a field abstraction. *)
let array_field_fs = 
  make_fs "array_elements" (TObject (TClass (java_lang_object)))


let pp_var_from_PP pp = 
  let (cn,ms) = cms_split ((JBirPP.get_meth pp).cm_class_method_signature) in
  let pc = JBirPP.get_pc pp in
    `PP ((),cn,ms,pc)


(*Module allowing to handle string literal.*)
module LiteralStr =
struct

  module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

  let str_literal_map = ref StringMap.empty
  let str_literal_cpt = ref 0

  let pp_from_string prog str = 
    let cn = make_cn "java.lang.String" in
    let node = JProgram.get_node prog cn in
    let ms = make_ms "init_string_literal" [] None in
    let str_literal_cm =
      {
        cm_signature = ms;
        cm_class_method_signature = make_cms cn ms;
        cm_static = true;
        cm_final = true;
        cm_synchronized = false;
        cm_strict = false; 
        cm_access= `Private;
        cm_generic_signature = None;
        cm_bridge= false;
        cm_varargs = false;
        cm_synthetic = false;
        cm_other_flags = [];
        cm_exceptions = [];
        cm_attributes = {
          synthetic = false;
          deprecated = false;
          other = []
        };
        cm_annotations = {
          ma_global= [];
          ma_parameters= [];
        };
        cm_implementation = Java (Lazy.from_val JBir.empty)
      }
    in
    let pp = 
      try StringMap.find str !str_literal_map
      with Not_found ->
        str_literal_cpt:= !str_literal_cpt+1;
        let pc = !str_literal_cpt in
        let pp = JBirPP.get_pp node str_literal_cm pc in
          str_literal_map := StringMap.add str pp !str_literal_map;
          pp
    in
      (*       Printf.printf "search for %s %s\n" str (JBirPP.to_string pp); *)
      pp

  let singleton_from_str prog str cn = 
    AbVar.singleton [pp_from_string prog (jstr_pp str);] cn

  let add_constraint prog pp str =
    let pp_var = pp_var_from_PP pp in
    let cn_str = make_cn "java.lang.String" in
    let objt_char_ar = TArray (TBasic `Char) in
    let val_fs = make_fs "value" (TObject (TArray (TBasic `Char)))  in
    let hs_fs = make_fs "hash" (TBasic `Int)  in
    let hs32_fs = make_fs "hash32" (TBasic `Int)  in
    let val_var = `Field ((), cn_str, val_fs) in
    let val_ar_var = `Field ((), java_lang_object, array_field_fs) in
    let hs_var = `Field ((), cn_str, hs_fs) in
    let hs32_var = `Field ((), cn_str, hs32_fs) in
      [(*initialize value field*)
        {CFAConstraints.dependencies = [pp_var];
        CFAConstraints.target = val_var;
        CFAConstraints.transferFun = 
          (fun _abSt ->
             let obj = singleton_from_str prog str (TClass cn_str) in 
             let fs = singleton_from_str prog str (objt_char_ar) in
               `FieldDomain (AbField.var2fSet obj fs)
          )};
        (*initialize char [] of value field*)
       {
         CFAConstraints.dependencies = [pp_var];
         CFAConstraints.target = val_ar_var;
         CFAConstraints.transferFun = 
           (fun _abSt ->
              let obj = singleton_from_str prog str objt_char_ar in
                `FieldDomain (AbField.var2fSet obj (AbVar.primitive))
           )};
        (*initialize hash32 field*)
       {
         CFAConstraints.dependencies = [pp_var];
         CFAConstraints.target = hs32_var;
         CFAConstraints.transferFun = 
           (fun _abSt ->
              let obj = singleton_from_str prog str (TClass cn_str) in 
                `FieldDomain (AbField.var2fSet obj (AbVar.primitive))
           )};
        (*initialize hash field*)
       {
         CFAConstraints.dependencies = [pp_var];
         CFAConstraints.target = hs_var;
         CFAConstraints.transferFun = 
           (fun _abSt ->
              let obj = singleton_from_str prog str (TClass cn_str) in 
                `FieldDomain (AbField.var2fSet obj (AbVar.primitive))
           )};
        (*initialize len field*)
(*
       {
         CFAConstraints.dependencies = [pp_var];
         CFAConstraints.target = len_var;
         CFAConstraints.transferFun = 
           (fun _abSt ->
              let obj = singleton_from_str prog str objt_char_ar
              in 
                `FieldDomain (AbField.var2fSet obj (AbVar.primitive))
           )};
 *)
      ]

  let handle_string prog pp opcode = 
    let rec handle_string' e = 
      match e with
        | Const `String str -> add_constraint prog pp str
        | Binop (_ , e_obj, e_idx) ->
            (handle_string' e_obj) @ (handle_string' e_idx)
        | Field (e, _, _) 
        | Unop (_ , e ) -> (handle_string' e )
        | _ -> []
    in
      match opcode with
        | AffectVar (_, e)
        | Throw e
        | AffectStaticField (_ , _ , e)
        | MonitorEnter e
        | MonitorExit e -> handle_string' e
        | AffectField  (e1, _ , _ , e2)
        | Ifd  ((_ , e1, e2) , _) -> 
            (handle_string' e1) @ (handle_string' e2)
        | AffectArray (e1, e2 , e3) ->
            List.fold_left (fun lst e -> (handle_string' e)@lst) 
              [] [e1;e2;e3]
        | Return opt_e ->
            (match opt_e with
              | None -> []
              | Some e -> handle_string' e
            )
        | New (_ , _ , _ , lst_e)
        | NewArray (_ , _ , lst_e)
        | InvokeStatic  (_ , _ ,  _ , lst_e) -> 
            List.fold_left (fun lst e -> (handle_string' e)@lst) 
              [] lst_e
        | InvokeVirtual (_ , e1 , _ , _ , lst_e)
        | InvokeNonVirtual (_ , e1, _ , _ , lst_e) ->
            List.fold_left (fun lst e -> (handle_string' e)@lst) 
              (handle_string' e1) lst_e
        | _ -> []
end

let rec array_type_2_cn vt = 
  match vt with
    | TBasic `Int -> make_cn "Sawja_array.Int"
    | TBasic `Short-> make_cn "Sawja_array.Short"
    | TBasic `Char -> make_cn "Sawja_array.Char"
    | TBasic `Byte -> make_cn "Sawja_array.Byte"
    | TBasic `Bool -> make_cn "Sawja_array.Bool"
    | TBasic `Long -> make_cn "Sawja_array.Long"
    | TBasic `Float -> make_cn "Sawja_array.Float"
    | TBasic `Double -> make_cn "Sawja_array.Double"
    | TObject (TClass cn) -> make_cn ("Sawja_array."^(cn_name cn))
    | TObject (TArray vt) -> 
        let cn = array_type_2_cn vt in
          make_cn ("Sawja_array."^(cn_name cn))

let objtype_2_cn objt =
  match objt with
    | TClass cn -> cn 
    | TArray vt -> array_type_2_cn vt

module ClassInit =
struct
  let class2pp = ref ClassMap.empty
  let class_cpt = ref 0

  let pp_from_class prog cn =
    let obj_node = JProgram.get_node prog java_lang_object in
    let ms = make_ms "init_class" [] None in
    let init_class_cm = 
      {
        cm_signature = ms;
        cm_class_method_signature = make_cms java_lang_object ms;
        cm_static = true;
        cm_final = true;
        cm_synchronized = false;
        cm_strict = false; 
        cm_access= `Private;
        cm_generic_signature = None;
        cm_bridge= false;
        cm_varargs = false;
        cm_synthetic = false;
        cm_other_flags = [];
        cm_exceptions = [];
        cm_attributes = {
          synthetic = false;
          deprecated = false;
          other = []
        };
        cm_annotations = {
          ma_global= [];
          ma_parameters= [];
        };
        cm_implementation = Java (Lazy.from_val JBir.empty)
      };
    in
      try ClassMap.find cn !class2pp
      with Not_found ->
        class_cpt := !class_cpt +1;
        let pc = !class_cpt in
        let pp = JBirPP.get_pp obj_node init_class_cm pc in
          class2pp := ClassMap.add cn pp !class2pp;
          pp

end

let cast_set prog objt set = 
  AbVar.filter_with_compatible prog set objt

let set_from_expr prog e abSt pp =
  let rec set_from_expr' e =
    let pp_var = pp_var_from_PP pp in
    let localvar = CFAState.get_PP abSt pp_var in
      match e with 
        | Const `ANull -> AbVar.empty
        | Const `String str -> 
            let java_lang_string = make_cn "java.lang.String" in
              AbVar.singleton [LiteralStr.pp_from_string prog (jstr_pp str);] 
                (TClass java_lang_string)
        | Const `Class objt ->
            let jlclass_cn = make_cn "java.lang.Class" in
            let cn = (objtype_2_cn objt) in
            AbVar.singleton [ClassInit.pp_from_class prog cn] (TClass jlclass_cn)
        | Var (_vt, v) -> AbLocals.get_var (index v) localvar 
        | Unop (Cast objtype , e ) -> 
            cast_set prog objtype (set_from_expr' e )
        | Binop (ArrayLoad _vt, e_obj, _e_idx) ->
            let f_var = `Field ((), java_lang_object, array_field_fs) in
            let abf = CFAState.get_field abSt f_var in
              AbField.fSet2var abf (set_from_expr' e_obj )
        | StaticField (cn, fs) ->
            let f_var = `Field ((),cn,fs) in
            let abf = CFAState.get_field abSt f_var in
              AbField.fSet2var abf AbField.static_field_dom

        | Field (e, cn, fs) -> 
            let f_var = `Field ((),cn,fs) in
            let abf = CFAState.get_field abSt f_var in
              AbField.fSet2var abf (set_from_expr' e )
        | _ -> AbVar.primitive
  in set_from_expr' e

    (**Compute the possible dependancies of an expression*)
let expr_dep expr prog = 
  let field_dep cn fs = 
    let fcl = JControlFlow.resolve_field fs (JControlFlow.resolve_class prog cn) in
      List.map 
        (fun fc ->`Field ((),JProgram.get_name fc,fs)) 
        fcl 
  in
  let rec expr_dep' expr = 
    match expr with
      | Unop (Cast _,expr) -> expr_dep' expr (*TODO: Maybe we can reduce dep, taking cast type into account*)
      | Binop (ArrayLoad _,e1,_e2) -> (expr_dep' e1)
      | Field (exprv, cn, fs) ->
          (field_dep cn fs) @ (expr_dep' exprv)
      | StaticField (cn, fs) ->
          (field_dep cn fs)
      | _ -> []
  in expr_dep' expr


let abstract_init_method_instr cn_node ms csts =
  let cn = JProgram.get_name cn_node in
  let pp_var = `PP ((),cn,ms,0) in
  let m_var = `Method ((),cn,ms) in
  let cst_loc =
    {
        CFAConstraints.dependencies = [m_var];
        CFAConstraints.target = pp_var;
        CFAConstraints.transferFun =
          (fun abSt ->
             let m_abst = CFAState.get_method abSt m_var in
               `PPDomain (AbMethod.init_locals cn_node ms m_abst)
          )
    }
  in cst_loc::csts

(* Handle native exception: 
*  On every exception handler, we check if it can catch a native exception. For
*  those exception handler, we add a constraint stating that such an exception
*  might have been throwed from any pp covered by the handler.*)
let handle_native_exc prog node cm =
  let impl = 
    match cm.cm_implementation with
      | Native -> raise Safe.Domain.DebugDom
      | Java laz -> Lazy.force laz
  in
  let ms = cm.cm_signature in
  let m_var = `Method ((),(JProgram.get_name node), ms) in
  let every_covered_pc exch = 
    let rec adding' pc lst = 
      if pc > exch.e_end 
      then lst
      else
        adding' (pc+1) (pc::lst)
    in
      adding' exch.e_start []
  in
  let pp_from_pc pc = JBirPP.get_pp node cm pc in
  let pp_var_from_pc pc = pp_var_from_PP (pp_from_pc pc) in
  let native_exc_list = default_native_throwable in
  List.fold_left
    (fun csts exch ->
       let pp = JBirPP.get_pp node cm exch.e_handler in
       let pp_var = pp_var_from_PP pp in
       let lst_pc = every_covered_pc exch in
       let cst = 
         {CFAConstraints.dependencies = m_var::(List.map pp_var_from_pc lst_pc);
          CFAConstraints.target = pp_var; 
          CFAConstraints.transferFun = 
            (fun _abSt ->
               `PPDomain 
                 (
                   (*first filter by native which can be handled by the catch*)
                   let filter_native = 
                     List.filter
                       (fun nt_exc ->
                          let nt_exc_node = JProgram.get_node prog nt_exc in
                            match exch.e_catch_type with
                              | None -> true
                              | Some ct ->
                                  let catch_node = 
                                    JProgram.get_node prog ct in
                                    JProgram.extends nt_exc_node catch_node
                       )
                       native_exc_list
                   in
                   let native_as_set = 
                     List.fold_left
                       (fun abV exc_cn ->
                          List.fold_left 
                            (fun abV pc-> 
                               AbVar.join abV 
                                 (AbVar.singleton [pp_from_pc pc] (TClass exc_cn))
                            )
                            abV
                            lst_pc
                       )
                       AbVar.bot
                       filter_native
                   in
                     AbLocals.set_var (index exch.e_catch_var) native_as_set AbLocals.init 
                 ))
         }
       in cst::csts
    )
    []
    (JBir.exc_tbl impl)


let affect_array f_obje f_abse dep =
  let f_var = `Field ((),java_lang_object,array_field_fs) in
    {CFAConstraints.dependencies= dep;
     CFAConstraints.target = f_var;
     CFAConstraints.transferFun= 
       (fun abSt -> `FieldDomain (AbField.var2fSet (f_obje abSt) (f_abse abSt)))
    }


let handle_throw ?(other_dep=[]) prog pp excAbSt =
  let open JBirPP in
  let pp_var = pp_var_from_PP pp in
  let propagate_locals ?(f=fun abSt -> CFAState.get_PP abSt pp_var) _ =  
    (fun abSt -> `PPDomain (f abSt)) in
  let possible_catch = handlers pp in
  let pp_var = pp_var_from_PP pp in
  (*constraints for the different catchs*)
  let (csts, already_catched_cn) = 
    List.fold_left
      (fun (csts, already_catched_cn) exch ->
         let pp_target_var = pp_var_from_PP (get_pp (get_class pp) 
                                               (get_meth pp) exch.e_handler) 
         in
           {
             CFAConstraints.dependencies = pp_var::other_dep;
             CFAConstraints.target = pp_target_var; 
             CFAConstraints.transferFun = 
               propagate_locals
                 ~f:(fun abSt -> 
                       let local = CFAState.get_PP abSt pp_var in
                         (*search wich exception are compatible we the
                         * catched var.*)
                       let varAbst = 
                         match exch.e_catch_type with
                           | None -> (excAbSt abSt)
                           | Some cn -> 
                               (AbVar.filter_with_compatible prog
                                  (excAbSt abSt)
                                  (TClass cn)
                               )
                       in
                        (*Remove exception which would have already been by
                        * previous catch.*)
                       let varAbst = 
                         List.fold_left
                           (fun varAbst cn ->
                              AbVar.filter_with_uncompatible prog varAbst (TClass cn)
                           )
                           varAbst
                           already_catched_cn
                       in
                         if (AbVar.is_empty varAbst) || (AbVar.isBot varAbst)
                         then  AbLocals.bot
                         else  AbLocals.set_var (index exch.e_catch_var)
                                 varAbst local
                 ) ();
           } :: csts, 
           (match exch.e_catch_type with
              | Some cn -> cn::already_catched_cn
              | None -> already_catched_cn)
      )
      ([], []) 
      possible_catch
  in 
  (*constraint when it is not catched: can be thrown by the method*)
  let m_var = `Method ((),(JProgram.get_name (get_class pp)), 
                       (get_meth pp).cm_signature) in
  let cst_uncatched = 
    {
      CFAConstraints.dependencies = pp_var::other_dep;
      CFAConstraints.target = m_var; 
      CFAConstraints.transferFun = 
        (fun abSt ->
           `MethodDomain
             (let ab_m = CFAState.get_method abSt m_var in
              let uncatchedAbst = (excAbSt abSt) in
              let uncatchedAbst = 
                List.fold_left
                  (fun varAbst cn ->
                     AbVar.filter_with_uncompatible prog varAbst (TClass cn)
                  )
                  uncatchedAbst
                  already_catched_cn
              in
              let uncatchedAbst = 
                if (AbVar.is_empty uncatchedAbst) || (AbVar.isBot uncatchedAbst)
                then AbVar.bot
                else uncatchedAbst 
              in
                AbMethod.join_exc_return ab_m uncatchedAbst
             )
        )
    }
  in cst_uncatched::csts

let rec ar_2_type cur_dim vt =
  if cur_dim = 1 
  then (TArray vt) 
  else (TArray (TObject (ar_2_type (cur_dim-1) vt)))


let rec handle_new_array pp pp_var vt ar_dim over_ar =
  let abVar_for_new_ar vt dim = 
      (fun _ -> AbVar.singleton [pp] (ar_2_type dim vt))
  in

  let has_obj_content vt =
    (match vt with
       | TBasic _ -> false
       | TObject _ -> true
    ) in
  let obje = abVar_for_new_ar vt ar_dim in
  let vare = (fun abSt -> 
                if (ar_dim > 1 )
                then abVar_for_new_ar vt (ar_dim -1) abSt
                else (if (has_obj_content vt)
                then AbVar.empty 
                else AbVar.primitive)) in
  let content_cst = affect_array obje vare [pp_var;] in
    if ar_dim = 1
    then content_cst::over_ar
    else handle_new_array pp pp_var vt (ar_dim-1) (content_cst::over_ar)





let abstract_instruction opt prog pp opcode succs csts =
  let pp_var = pp_var_from_PP pp in
  let propagate_locals ?(f=fun abSt -> CFAState.get_PP abSt pp_var) _ =  
    (fun abSt -> `PPDomain (f abSt))
  in
  let is_dead abSt  =
    let l = CFAState.get_PP abSt pp_var in
    AbLocals.isBot l 
  in
  let if_alive_meth abSt f = 
    match is_dead abSt with
      | true -> AbMethod.bot
      | false -> f
  in
  let make_csts ?(cstsl=csts) ?(other_dep=[]) ?(prop_locals_f=fun abSt -> CFAState.get_PP abSt pp_var) _ =
    List.fold_right 
      (fun target csts ->
         let pp_targ = pp_var_from_PP target in
           {CFAConstraints.dependencies = pp_var::other_dep;
            CFAConstraints.target = pp_targ;
            CFAConstraints.transferFun = propagate_locals ~f:prop_locals_f ()
           }::csts
      )
      succs 
      cstsl
  in
  (*for every instruction, we first check there is not literal string in
   * expression.*)
  let csts = (LiteralStr.handle_string prog pp opcode)@ csts in

  (*init: if we are in an allocation, name of initizalized class.
  * static: true if the invoke is a static call.
  * *)
  let handle_invoke ?(init=None) ?(static=false) opt_ret cn_lst ms args =
    (*Constraint on the method arguments.*)
    let csts_arg = 
      let deps = 
        List.fold_left 
          (fun odep arg -> (expr_dep arg prog)@odep)
          [pp_var;]
          args
      in
        List.fold_left
          (fun csts cn -> 
             let m_var = `Method ((),cn,ms) in
             let cst = 
               {
                 CFAConstraints.dependencies = pp_var::(m_var::deps);
                 CFAConstraints.target = m_var;
                 CFAConstraints.transferFun =
                   (fun abSt ->
                      `MethodDomain
                      (if_alive_meth abSt (
                         let pos = ref (-1) in
                         let set_args = 
                           List.fold_left 
                             (fun nl arg ->
                                pos := !pos +1;  
                                match init, !pos with
                                  | Some this, 0 ->
                                      (*if in an init, we force this to its cn.*)
                                      let v = (AbVar.singleton [pp] this)
                                      in
                                        AbLocals.set_var 0 v nl
                                  | _ -> 
                                      let varAb = (set_from_expr prog arg abSt pp) in
                                        AbLocals.set_var !pos varAb nl
                             ) AbLocals.init args
                         in
                           match static with 
                            | false when (AbVar.isBot 
                                            (AbLocals.get_var 0 set_args) 
                                            || 
                                          AbVar.is_empty 
                                            (AbLocals.get_var 0 set_args)
                                            )
                              -> 
                                AbMethod.bot

                             | false -> 
                                 (*We can refine : this is of class cn or a
                                  * subclass of cn*)
                                 let set_args = 
                                   AbLocals.set_var 0 
                                     (AbVar.filter_with_compatible  
                                        prog (AbLocals.get_var 0 set_args) (TClass cn)) 
                                   set_args 
                                 in
                                   AbMethod.join_args AbMethod.init set_args
                            | true ->
                                  AbMethod.join_args AbMethod.init set_args
                   )))
               }
             in cst::csts
          )
          []
          cn_lst
    in
    let csts = 
      match init, opt_ret with 
        | Some _, _ -> (csts_arg@csts) (*make_csts done while handling the new*)
        | _,None -> make_csts ~cstsl:(csts_arg@csts) ()
        | _,Some ret_v ->
            let csts_ret = 
              (*constraint on the local variables*)
              List.fold_left
                (fun csts cn -> 
                   let m_var = `Method ((),cn,ms) in
                     List.fold_left 
                       (fun csts  target ->
                          let cst = 
                            let pp_targ = pp_var_from_PP target in
                              {
                                CFAConstraints.dependencies = [pp_var;m_var];
                                CFAConstraints.target = pp_targ;
                                CFAConstraints.transferFun =
                                  (fun abSt ->
                                     let l = CFAState.get_PP abSt pp_var in
                                     let ab_m = CFAState.get_method abSt m_var in
                                       `PPDomain (AbLocals.set_var (index ret_v) 
                                                    (AbMethod.get_return ab_m) l)
                                  )
                              }
                          in 
                            cst::csts
                       )
                       csts
                       succs
                )
                []
                cn_lst
            in
              csts_arg@csts_ret@csts
    in
      (*csts on uncatched exception from called function*)
    let csts_exc = 
      List.fold_left
        (fun csts cn -> 
           let m_called_var = `Method ((),cn,ms) in
           let excAbst = 
             (fun abSt -> let mAbSt = CFAState.get_method abSt m_called_var in
                AbMethod.get_exc_return mAbSt)
           in
             (handle_throw ~other_dep:[m_called_var] prog pp excAbst)@csts
        )
        []
        cn_lst
    in
      csts_exc@csts

  (**** handle invoke end*****)

  in
    match opcode with
      | Goto _ 
      | MonitorEnter _
      | Check _
      | Formula _
      | MonitorExit _
      | Nop -> make_csts () 
      | Ifd _ -> make_csts () 
      | AffectVar (v,e) ->
          let dep = expr_dep e prog in
            make_csts ~other_dep:dep ~prop_locals_f:
              (fun abSt -> 
                 let l = CFAState.get_PP abSt pp_var in
(*
                                Printf.printf "2\n";
                                Printf.printf "cn: %s ms: %s %s %d\n"
                                  (cn_name (JProgram.get_name(JBirPP.get_class pp))) 
                                  (ms_name ((JBirPP.get_meth pp).cm_signature))
                                  (JPrint.value_type_list
                               (ms_args ((JBirPP.get_meth pp).cm_signature)))
                                  (JBirPP.get_pc pp)
                                  ;
 *)
                   AbLocals.set_var (index v) (set_from_expr prog e abSt pp) l
              ) ()
      | AffectArray (e1, _e2, e3) (*e1[e2] = e3*) ->
          let dep = expr_dep e3 prog in
          let obje = (fun abSt -> set_from_expr prog e1 abSt pp) in
          let vare = (fun abSt -> set_from_expr prog e3 abSt pp) in
          let array_cst = affect_array obje vare (pp_var::dep) in
            make_csts ~cstsl:(array_cst::csts) ()
      | AffectField (e1, cn, fs, e2) (*e1.<cn:fs> = e2*) -> 
          let dep = expr_dep e2 prog in
          let f_var = `Field ((),cn,fs) in
          let af_const = 
            {CFAConstraints.dependencies= pp_var::dep;
             CFAConstraints.target = f_var;
             CFAConstraints.transferFun= 
               (fun abSt -> `FieldDomain (
(*                                 Printf.printf "4\n"; *)
                  AbField.var2fSet 
                                            (set_from_expr prog e1 abSt pp) 
                                            (set_from_expr prog e2 abSt pp)))
            }
          in
            make_csts ~cstsl:(af_const::csts) ()
      | AffectStaticField (cn, fs, e) -> (*<cn:fs> = e *)
          let dep = expr_dep e prog in
          let f_var = `Field ((),cn,fs) in
          let af_const = 
            {CFAConstraints.dependencies= pp_var::dep;
             CFAConstraints.target = f_var;
             CFAConstraints.transferFun= 
               (fun abSt -> `FieldDomain (
                  
(*                                 Printf.printf "5\n"; *)
                  AbField.var2fSet AbField.static_field_dom
                                            (set_from_expr prog e abSt pp)))
            }
          in
            make_csts ~cstsl:(af_const::csts) ()
      | Throw e -> 
(*                                 Printf.printf "6\n"; *)
          let excAbSt = (fun abSt -> set_from_expr prog e abSt pp) in
          let exc_csts = handle_throw prog pp excAbSt in
            exc_csts@csts
      | Return opt_retexpr ->
          let c = JBirPP.get_class pp in
          let ms = 
            let m = JBirPP.get_meth pp in m.cm_signature in
          let m_var = `Method ((),JProgram.get_name c,ms) in
          let cstreturn = 
            (match opt_retexpr with 
              | None -> csts
              | Some ret_expr ->
                  let deps = (expr_dep ret_expr prog) @ [pp_var; m_var] in
                    { CFAConstraints.dependencies = deps;
                      CFAConstraints.target = m_var;
                      CFAConstraints.transferFun =
                        (fun abSt ->
                           `MethodDomain(
                             if_alive_meth abSt
                               (let vexpr = 
(*                                 Printf.printf "7\n"; *)
                                  set_from_expr prog ret_expr abSt pp
                                in
                                  if (AbVar.isTop vexpr)
                                  then raise Safe.Domain.DebugDom
                                  else
                                    AbMethod.join_return
                                      AbMethod.init vexpr)))
                    }::csts)
	  in
            cstreturn
      | New (v, cn, vt_args, args) ->
          let ms = make_ms "<init>" vt_args None in
          let this = Var (TObject (TClass cn),v) in
          let csts = handle_invoke ~init:(Some (TClass cn)) None [cn] ms (this::args) in
          make_csts ~cstsl:csts ~prop_locals_f: 
            (fun abSt -> 
               let l = CFAState.get_PP abSt pp_var in
                 AbLocals.set_var (index v) (AbVSet.singleton [pp] (TClass cn)) l
            ) ()

      | NewArray (v, vt, args) ->
          let dim = List.length args in
          let content_cst =  handle_new_array pp pp_var vt dim [] in
            make_csts ~cstsl:(content_cst@csts) ~prop_locals_f:
              (fun abSt -> 
                 let l = CFAState.get_PP abSt pp_var in
                   AbLocals.set_var (index v) 
                     (AbVSet.singleton [pp] (ar_2_type dim vt)) l
              ) ()
      | InvokeStatic (opt_ret, cn, ms, args) ->
          handle_invoke ~static:true opt_ret [cn] ms args
      | InvokeVirtual (opt_ret, obje, _, ms, args) 
      | InvokeNonVirtual (opt_ret, obje, _ , ms, args) ->
          let cn_lst = 
            List.map 
              (fun called_pp -> 
                 JProgram.get_name (JBirPP.get_class called_pp)
              )
              (JBirPP.static_pp_lookup prog pp)
          in
            handle_invoke opt_ret cn_lst ms (obje::args)
      | MayInit cn ->
          if opt.cfa_clinit_as_entry
          then make_csts () (*clinit considered as entry point*)
          else (
            let csts = handle_invoke ~static:true None [cn] clinit_signature [] 
            in make_csts ~cstsl:csts ()
          )
      


let compute_instr_csts prog opt node m =
  let open JBirPP in
    match m.cm_implementation with 
      | Native -> []
      | Java _laz -> 
          let native_exc_csts = handle_native_exc prog node m in
          let iter_on_pp pp csts = 
            let lst_succ = (normal_successors pp) in
              abstract_instruction opt prog pp (get_opcode pp) lst_succ csts
          in
          let cn = JProgram.get_name node in
          let ms = m.cm_signature in
          let first_pp = get_first_pp prog cn ms in
          let reachable_pp = reachable_pp first_pp in
          let csts_normal = 
            List.fold_left
              (fun csts pp -> 
                 iter_on_pp pp csts 
              )
              []
              reachable_pp
          in native_exc_csts@csts_normal




(*TODO: Do not use list but map/set ???*)
let get_csts program opt main_entry_points entry_points =
  let init_meth_csts = 
    ClassMap.fold
      (fun _cn node csts ->
         JProgram.cm_fold
           (fun cm csts ->
              let ms = cm.cm_signature in
              match cm.cm_implementation with 
                | Native -> csts
                | Java _ -> abstract_init_method_instr node ms csts 
           )
           node
           csts
      )
      program.JProgram.classes
      []
  in
  (*initialising string array argument of the main method*)
  let main_argument_csts = 
    (*add the string array itself*)
    let (main_cn, main_ms) = cms_split main_entry_points in
    let m_var = `Method ((),main_cn,main_ms) in
    let string_cn = make_cn "java.lang.String" in
    let main_node = JProgram.get_node program main_cn in
    let main_cm = JProgram.get_concrete_method main_node main_ms in
    let str_ar_obj = TArray (TObject (TClass string_cn)) in
(*     let char_ar_cn = make_cn "Sawja_array.Char" in *)
    let char_ar_obj = TArray (TBasic `Char) in
    let val_fs = make_fs "value" (TObject (TArray (TBasic `Char)))  in
    let val_var = `Field ((), string_cn, val_fs) in
(*     let len_fs = make_fs "length" (TBasic `Int)  in *)
(*     let len_var = `Field ((), char_ar_cn, len_fs) in *)
      (*put the string array*)
    let pre_init_singleton cn =  
      AbVar.singleton [JBirPP.get_pp main_node main_cm (-1)] cn in

    let str_ar = 
      let obj_f = (fun _abSt -> pre_init_singleton str_ar_obj) in
      let abse_f = (fun _abst -> pre_init_singleton (TClass string_cn)) in
        affect_array obj_f abse_f []
    in
      (*set the value field*)
    let str_val_ar =
      {CFAConstraints.dependencies = [];
       CFAConstraints.target = val_var;
       CFAConstraints.transferFun = 
         (fun _abSt ->
            let obj = pre_init_singleton (TClass string_cn) in 
            let fs = pre_init_singleton char_ar_obj in
              `FieldDomain (AbField.var2fSet obj fs))
      }
    in
      (*set the char array content*)
    let str_ar_content =
      let obj_f = (fun _abst -> pre_init_singleton char_ar_obj) in
        affect_array obj_f (fun _ -> AbVar.primitive) []
    in
        (*Set the local map variable*)
    let args = 
      {CFAConstraints.dependencies = [];
       CFAConstraints.target = m_var;
       CFAConstraints.transferFun = 
         (fun _abSt ->
             let new_args =
               AbLocals.set_var 0 (pre_init_singleton str_ar_obj) AbLocals.init
             in
               `MethodDomain (AbMethod.set_args (AbMethod.init) new_args))
      }
(*
    in
    let len_str = 
      {
        CFAConstraints.dependencies = [];
        CFAConstraints.target = len_var;
        CFAConstraints.transferFun = 
          (fun _abSt ->
             let obj = pre_init_singleton str_ar_obj in
               `FieldDomain (AbField.var2fSet obj (AbVar.primitive)))
      }
 *)
    in str_ar::str_val_ar::str_ar_content::[args]
  in
  let init_csts =
    List.fold_left 
      (fun lst cms ->
         let (cn, ms) = cms_split cms in
           try 
             let pp = 
               JBirPP.get_first_pp program cn ms in
             let pp_var = pp_var_from_PP pp in
               {CFAConstraints.dependencies = [];
                CFAConstraints.target = pp_var ;
                CFAConstraints.transferFun = 
                  (fun _abst -> `PPDomain AbLocals.init )
               }::lst
           with JControlFlow.PP.NoCode (cn, ms )->
             Printf.printf "no code for method %s %s\n" (cn_name cn) (ms_name ms);
             lst
      )
      (init_meth_csts@main_argument_csts)
      (main_entry_points::entry_points)
  in
  (*Add initial constraints on static Fields. They are initially set as Null.*)
  let init_csts = 
    JProgram.fold
    (fun csts nd ->
       JProgram.f_fold
         (fun f csts -> 
            if is_static_field f
            then 
              (let (cn,fs) = cfs_split (get_class_field_signature f) in
              let f_var = `Field ((),cn ,fs) in 
              {
                CFAConstraints.dependencies = [];
                CFAConstraints.target = f_var ;
                CFAConstraints.transferFun = 
                  (fun _abSt -> `FieldDomain AbField.empty)
              }::csts)
            else csts
         )
         nd
         csts
    )
    init_csts
    program  
  in
  let instr_csts = 
    ClassMethodMap.fold
      (fun _cms (node,m) csts ->
         (compute_instr_csts program opt node m)@csts)
      program.JProgram.parsed_methods
      []
  in init_csts@instr_csts




module HeapInit =
struct 

  let pp_from_heap_ref prog ref = 
    let obj_node = JProgram.get_node prog java_lang_object in
    let ms = make_ms "init_heap" [] None in
    let init_heap_cm = 
      {
        cm_signature = ms;
        cm_class_method_signature = make_cms java_lang_object ms;
        cm_static = true;
        cm_final = true;
        cm_synchronized = false;
        cm_strict = false; 
        cm_access= `Private;
        cm_generic_signature = None;
        cm_bridge= false;
        cm_varargs = false;
        cm_synthetic = false;
        cm_other_flags = [];
        cm_exceptions = [];
        cm_attributes = {
          synthetic = false;
          deprecated = false;
          other = []
        };
        cm_annotations = {
          ma_global= [];
          ma_parameters= [];
        };
        cm_implementation = Java (Lazy.from_val JBir.empty)
      };
    in
      JBirPP.get_pp obj_node init_heap_cm ref


  let init_state_from_heap prog hp state =
    let open ParserType in
    let get_abstract_val v = 
      match v with
        | VInt _ | VChar _ | VShort _ | VBool _ | VByte _ | VLong _
        | VFloat _ | VDouble _ -> AbVar.primitive
        | VObject (_cn, Null) -> AbVar.empty
        | VObject (_cn, Ref ref) -> AbVar.singleton [pp_from_heap_ref prog ref] (get_dyn_type ref)
        | VArray (_vt, Null) -> AbVar.empty
        | VArray (_vt, Ref ref) -> AbVar.singleton [pp_from_heap_ref prog ref]
                                    (get_dyn_type ref)
    in

    let get_abstract_ar_val ar_v = 
      Array.fold_left
        (fun abVal v ->
           AbVar.join abVal (get_abstract_val v)
        )
        AbVar.bot
        ar_v
    in
      ClassMap.iter
        (*handling class*)
        (fun cn cl_el ->
           (*init static field*)
           FieldMap.iter
             (fun fs parser_v -> 
                let fs_var = `Field ((), cn, fs) in
                let abVal = get_abstract_val parser_v in
                  CFAState.join 
                    state
                    fs_var
                    (`FieldDomain (AbField.var2fSet AbField.static_field_dom abVal))
             )
             cl_el.cl_static_fields;
           (*init instance field*)
           Ptmap.iter
             (fun id_inst fmap ->
                FieldMap.iter
                  (fun fs parser_v ->
                     let fs_var = `Field ((), cn, fs) in
                     let abVal = get_abstract_val parser_v in
                     let abObj = AbVar.singleton [pp_from_heap_ref prog id_inst] (TClass cn) in
                       CFAState.join 
                         state
                         fs_var
                         (`FieldDomain (AbField.var2fSet abObj abVal))
                  )
                  fmap
             )
             cl_el.cl_instances
        )
        hp.hp_class ;
      (*handling array*)
      ObjectMap.iter
        (fun obj pmap ->
           Ptmap.iter
             (fun id_inst ar_val ->
                let fs_var = `Field ((),java_lang_object , array_field_fs) in
                let abVal = get_abstract_ar_val ar_val in
                let abObj = AbVar.singleton [pp_from_heap_ref prog id_inst] obj in 
                  CFAState.join
                    state
                    fs_var
                    (`FieldDomain (AbField.var2fSet abObj abVal))
             )
             pmap
        )
        hp.hp_array;
      ()
end 

let initial_state program init_heap entry_points =
  (* TODO: calculate init size on number of fields or methods of program ? *)
  let state = CFAState.bot (1,1,10000,100000, 1000000)   in
    (*The initial entry points are initialized in the state.*)
    (match init_heap with
         None ->
           List.iter
             (function `Method ((),cn,ms) ->
                CFAState.join
                  state
                  (`Method ((),cn,ms))
                  (`MethodDomain (AbMethod.init))
             )
             entry_points
       | Some hp ->
           HeapInit.init_state_from_heap program hp state
    );
    state

(*TODO: always fail if we are in unreachable code*)
let cfa_static_lookup state prog classes = 
  let open JProgram in
  fun cn ms pc ->
    let abm = CFAState.get_method state (`Method ((),cn,ms))
    in
      if AbMethod.isBot abm
      then ClassMethodSet.empty
      else
        (let caller_c = ClassMap.find cn classes in
         let m = get_method caller_c ms in
           match m with
             | AbstractMethod _ -> 
                 failwith "Can't call static_lookup on Abstract Methods"
             | ConcreteMethod cm ->
                 let pp = JBirPP.get_pp caller_c cm pc in 
                 let get_expr_state e = 
                   (*                                 Printf.printf "8\n"; *)
                   (AbVSet.concretize (set_from_expr prog e state pp))
                 in
                   (match cm.cm_implementation with 
                      | Native -> 
                          failwith "Can't call static_lookup on Native methods"
                      | Java bir_code ->
                          (match (JBir.code (Lazy.force bir_code)).(pc) with
                             | InvokeStatic (_ret ,called_cn, called_ms,_args) ->
                                 let callee = 
                                   match ClassMap.find called_cn classes with
                                   | Class c -> c
                                   | Interface _ -> 
                                       raise IncompatibleClassChangeError
                                 in 
                                 let (_c,cm) = JControlFlow.invoke_static_lookup 
                                                 callee called_ms
                                 in
                                   ClassMethodSet.singleton cm.cm_class_method_signature
                             | InvokeVirtual (_ret, obje, _, called_ms, _args) ->
                                 let possible_obj = get_expr_state obje in
                                   JType.ObjectSet.fold
                                     (fun obj cmsset -> 
                                        match obj with
                                          | TClass cn ->
                                              let cms = make_cms cn called_ms in
                                                ClassMethodSet.add cms cmsset
                                          | TArray _ -> raise Safe.Domain.DebugDom
                                     )
                                     possible_obj
                                     ClassMethodSet.empty 
                             | InvokeNonVirtual  (_ret, _e, cn, ms , _args) ->
                                 let callee = match ClassMap.find cn classes with
                                   | Class c -> c
                                   | Interface _ -> 
                                       raise IncompatibleClassChangeError
                                 in 
                                 let (_c,cm) = JControlFlow.invoke_special_lookup 
                                                 caller_c callee ms
                                 in
                                   ClassMethodSet.singleton
                                     cm.cm_class_method_signature
                             | _ -> raise Not_found
                          )
                   )
        )

let upd_reachable_methods program state = 
  ClassMap.fold
    (fun cn nd rmmap -> 
       JProgram.cm_fold
         (fun cm rmmap -> 
            let abm = CFAState.get_method state 
                        (`Method ((),cn,cm.cm_signature)) in
              if AbMethod.isBot abm
              then rmmap
              else 
                (let cms = make_cms cn cm.cm_signature in
                   ClassMethodMap.add cms (nd, cm) rmmap)
         )
         nd
         rmmap
    )
    program.JProgram.classes
    ClassMethodMap.empty


let cfa_program_from_state st prog = 
  let open JProgram in
  {
    prog with
        static_lookup_method =
          cfa_static_lookup
            st
            prog
            prog.classes;
        parsed_methods = upd_reachable_methods prog st
  }

let print_cfa_prog prog state dir = 
  (try 
     (if (not (Sys.is_directory dir))
      then (Printf.printf "Impossible to dump cfa program: %s is not a valid directory." dir;
            exit 1))
   with Sys_error _ -> Unix.mkdir dir 0o744);
  JCFAPrinter.print prog state dir


let get_CFA_program 
      ?(opt=default_opt)
      ?(init_heap=None)
      (program: JBir.t JProgram.program)
      (entry_points:class_method_signature list)
      (main_entry_points : class_method_signature)
      : JBir.t JProgram.program =
         CFASolver.debug_level := opt.cfa_debug;
  let entry_st=
    match init_heap with
       None ->
          List.map
            (fun cms -> let cn,ms =cms_split cms in `Method ((),cn,ms))
            entry_points
      | Some _hp -> []
  in
  let csts = get_csts program opt main_entry_points entry_points
  and state = initial_state program init_heap entry_st in

  let state =
    try 
    CFASolver.solve_constraints program csts state entry_st
    with CFAState.DebugSt st -> st
  in
  let prog = 
    cfa_program_from_state state program 
  in
    match opt.cfa_html_dump with
      | None -> prog
      | Some dir -> print_cfa_prog prog state dir; prog
