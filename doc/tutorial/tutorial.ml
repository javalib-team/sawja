(* CAUTION: this file is only for testing purpose, it must contain
   all code given in sawja_tutorial.md and allows to test that tutorial
   text corresponds to actual version of Sawja*)

open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram
let (prta,instantiated_classes) =
  JRTA.parse_program ("/local/vmonfort/workspace/javalib/trunk/javalib/doc/tutorial/test/:/local/vmonfort/sauv_synack/jre1.7.0/lib/rt.jar")
    (JBasics.make_cms
       (JBasics.make_cn "A") 
       JProgram.main_signature)

(* p_class annots a class, saying if it may be instantiated
   or not. *)
let p_class =
  (fun cn ->
     let ioc = get_node prta cn in
       match ioc with
         | Class _c ->
             if ClassMap.mem (get_name ioc) instantiated_classes then
               ["Instantiated"] else ["Not instantiatied"]
         | _ -> []
  )

(* p_method annots a method, saying if it is concrete or abstract,
   and if it has been parsed or not (by RTA). *)
let p_method =
  (fun cn ms ->
     let m = get_method (get_node prta cn) ms in
       match m with
         | AbstractMethod _ -> ["Abstract Method"]
         | ConcreteMethod _cm ->
             let cms = make_cms cn ms in
             let parse =
               if ClassMethodMap.mem cms prta.parsed_methods then
                 "Parsed" else "Not parsed" in
               ["Concrete Method "; parse]
  )

(* There is no field annotation. *)
let p_field = (fun _ _ -> [])

(* There is no program point annotation. *)
let p_pp = (fun _ _ _ -> [])

(* This is the info type. *)
let simple_info = 
  { JPrintHtml.p_class = p_class;
    JPrintHtml.p_field = p_field;
    JPrintHtml.p_method = p_method;
    JPrintHtml.p_pp = p_pp }

let output = "./bc"
let () = JPrintHtml.JCodePrinter.print_program ~info:simple_info prta output

let pbir = JProgram.map_program2
  (fun _ -> JBir.transform ~bcv:false ~ch_link:false) 
  (Some (fun code pp -> code.JBir.pc_ir2bc.(pp))) prta

let output = "./bir"
let () = JPrintHtml.JBirPrinter.print_program ~info:simple_info pbir output

let obj = JProgram.get_node pbir JBasics.java_lang_object
let () = JPrint.print_class (JProgram.to_ioc obj)
  JBir.print stdout





module type PrintInterface =
sig
  type instr
  type code
  val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit
  val method_param_names : code program -> class_name ->
     method_signature -> string list option
  val inst_html : code program -> class_name -> method_signature ->
    int -> instr -> JPrintHtml.elem list
  val jcode_pp :
    ('a program -> class_name -> method_signature -> int -> int) option
end

let iter_code f lazy_code =
      let code = Lazy.force lazy_code in
        Array.iteri
          (fun pp opcode ->
            match opcode with
              | JCode.OpInvalid -> ()
              | _ -> f pp [opcode]
          ) code.JCode.c_code

let method_param_names _ _ _ = None

open JPrintHtml

let inst_html program cs ms pp op =
      match op with
        | JCode.OpNew ccs ->
          let v = TObject (TClass ccs) in
            [simple_elem "new"; value_elem program cs v]
        | JCode.OpNewArray v ->
            [simple_elem "newarray"; value_elem program cs v]
        | JCode.OpGetField (ccs,fs) ->
          let ftype = fs_type fs in
            [simple_elem "getfield";
             field_elem program cs ccs fs;
             simple_elem " : ";
             value_elem program cs ftype]
        | JCode.OpInvoke ((`Virtual o),cms) ->
          let ccs = match o with
            | TClass ccs -> ccs
            | _ -> 
		JBasics.java_lang_object 
	  in
	  let inst =
	    Javalib.JPrint.jopcode ~jvm:true op 
	  in
            [simple_elem inst;
             invoke_elem program cs ms pp ccs cms;
             method_args_elem program cs ms]
        | _ ->
	    let inst =
	      Javalib.JPrint.jopcode ~jvm:true op 
	    in
              [simple_elem inst]

let inst_html _program _ _ _ op =
      let inst =
        Javalib.JPrint.jopcode ~jvm:true op in
        [simple_elem inst]

let jcode_pp _ _ _ x = x
