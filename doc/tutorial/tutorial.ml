(* CAUTION: this file is only for testing purpose, it must contain
   all code given in sawja_tutorial.md and allows to test that tutorial
   text corresponds to actual version of Sawja*)

open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram
let (prta,instantiated_classes) =
  JRTA.parse_program (Sys.getenv "CLASSPATH")
        (JBasics.make_cms
           (JBasics.make_cn "soot.Main") JProgram.main_signature)

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

let output = "./prta"
let () = JPrintHtml.JCodePrinter.print_program ~info:simple_info prta output

let pbir = JProgram.map_program2
   (fun _ -> JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[]) 
   (Some (fun code pp -> (JBir.pc_ir2bc code).(pp)))
   prta

let obj = JProgram.get_node pbir JBasics.java_lang_object
let () = JPrint.print_class (JProgram.to_ioc obj)
    JBir.print stdout
let output = "./pbir"
let () = JBir.print_program pbir output

module type PrintInterface =
sig
  type instr
  type code
  val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit
  val method_param_names : code program -> class_name ->
     method_signature -> string list option
  val inst_html : code program -> class_name -> method_signature ->
    int -> instr -> JPrintHtml.elem list
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
      let ccs = 
              match o with
              | TClass ccs -> ccs
            | _ -> JBasics.java_lang_object 
        and inst =
          Javalib.JPrint.jopcode ~jvm:true op 
          in
             [simple_elem inst;
           invoke_elem program cs ms pp ccs cms;
           method_args_elem program cs ms]
  (*| ... -> ...*)
    | _ ->
      let inst =
        Javalib.JPrint.jopcode ~jvm:true op in
        [simple_elem inst]

let inst_html _program _ _ _ op =
  let inst =
    Javalib.JPrint.jopcode ~jvm:true op in
    [simple_elem inst]

(** Fill debug information when dead affectations are detected*)
let fill_debug_infos dead_found cn ms code live plugin_infos =
  let _warns = 
    if dead_found
    then
      plugin_infos.JPrintPlugin.p_warnings <- 
        JPrintPlugin.add_meth_iow 
        (JPrintPlugin.MethodSignature "The method contains dead affectation(s).")
        cn ms plugin_infos.JPrintPlugin.p_warnings
  and _infos = 
    if dead_found
    then
      begin

      (* We give information on the variables liveness for this
         method*)
      Array.iteri
        (fun i _ -> 
           let info_live = 
             Printf.sprintf "Live variables: %s\n"
               (Live_bir.to_string (live i)) 
           in
             plugin_infos.JPrintPlugin.p_infos <- 
               JPrintPlugin.add_pp_iow info_live cn ms i plugin_infos.JPrintPlugin.p_infos
        )
        (JBir.code code);
        plugin_infos.JPrintPlugin.p_infos <- 
          JPrintPlugin.add_meth_iow 
          (JPrintPlugin.MethodSignature "Dead variable affectation(s) found") 
          cn ms plugin_infos.JPrintPlugin.p_infos ;
      end
    else
      plugin_infos.JPrintPlugin.p_infos <- 
        JPrintPlugin.add_meth_iow 
          (JPrintPlugin.MethodSignature "No dead variable affectation found") 
          cn ms plugin_infos.JPrintPlugin.p_infos;
  in ()



(**[method_dead_affect cn ms code live plug_info] modifies the data
  structure [plug_info] containing information for the Eclipse
  plugin. [cn] is a class_name, [ms] a method_signature, [code] the
  JBir code of the method and [live] the result of the live analysis
  on the code.*)
let method_dead_affect cn ms code live plugin_infos =

  (*Corner cases: false positive on AffectVar instruction*)
  let not_corner_case i = 
    (* Check all AffectVar instructions corresponding to a catch(Exception e) statement*)
    List.for_all
      (fun exc_h -> not(i = exc_h.JBir.e_handler))
      (JBir.exc_tbl code)
  in
  let dead_var_exists = ref false in
    Array.iteri 
      (fun i op -> 
         (match op with 
            | JBir.AffectVar (var,_) when not_corner_case i -> 

                (* Is the variable dead at next instruction ?*)
                let live_res = live (i+1) in
                let dead_var = not (Live_bir.Env.mem var live_res) in
                  if dead_var
                  then 
                    begin

                      (* We add a warning on the dead affectation instruction*)
                      let warn_pp = 
                        (Printf.sprintf 
                           "Variable '%s' is affected but never read !" 
                           (JBir.var_name_g var), None)
                      in
                        plugin_infos.JPrintPlugin.p_warnings <- 
                        JPrintPlugin.add_pp_iow 
                          warn_pp cn ms i plugin_infos.JPrintPlugin.p_warnings;
                        dead_var_exists := true
                    end
            | _ -> ()))
      (JBir.code code);

    (* Fill information for plugin depending on the method analysis result *)
    fill_debug_infos 
      !dead_var_exists cn ms code live plugin_infos (*==> Last function to write *)



(**[run_dead_affect ioc] returns the data structure containing
   information for the Eclipse plugin. [ioc] is the interface_or_class
   to check.*)
let run_dead_affect ioc =
  let plugin_infos = JBir.PluginPrinter.empty_infos in
    Javalib.cm_iter
      (fun cm ->
   match cm.Javalib.cm_implementation with 
       Javalib.Native -> ()
     | Javalib.Java lazc -> 
         let code = Lazy.force lazc in

         (* Launch the live variable analysis *)
         let live = Live_bir.run code in
         let (cn,ms) = JBasics.cms_split cm.Javalib.cm_class_method_signature in
           method_dead_affect 
             cn ms code live plugin_infos (*==> We need to write this function next*)
      )
      ioc;
    plugin_infos

(** [main cp output cn_string] loads the class [cn_string], converts it
   in JBir representation, runs the analysis and prints the information
   for the plugin. [cp] is a class_path, [output] a folder path for
   generation of plugin information and [cn_string] the fully
   qualified name of a Java class file.*)
let main cp output cn_string =
  let cn = JBasics.make_cn cn_string in
  let ioc = Javalib.get_class cp cn in
  let bir_ioc = Javalib.map_interface_or_class_context JBir.transform ioc in
  let plugin_infos = run_dead_affect bir_ioc (*==> We need to write this function next*)
  in 

    (* Print infos on the current class for the Eclipse plugin*)
    JBir.PluginPrinter.print_class plugin_infos bir_ioc output


(** [_plugin_exec] parses arguments and executes the analysis for each given
    class file*)
let _plugin_exec =
 
    (* Arguments values*)
  let targets = ref []
  and classpath = ref "" 
  and path_output = ref "" in

    (* Description and instanciation of the arguments*)
  let exec_args = 
    [ ("--files", "Class files",
       ArgPlugin.ClassFiles (fun sl -> targets := sl),
       "The class files to pass to the live affectation checker.");
      ("--classpath", "Class path",
       ArgPlugin.ClassPath (fun s -> classpath := s),
       "Locations where class files are looked for.");
    ]
  in
  let usage_msg = "Usage: " ^Sys.argv.(0)^ " [options]" in

    (* Add analysis description and output, and launch parsing of arguments *)
    ArgPlugin.parse
      ("DVAD","Dead Variables Affected Detection: detects variables affected but never read.")
      exec_args
      (ArgPlugin.PluginOutput ("--plug_output",(fun s -> path_output := s)))
      usage_msg;
    try
      let cp = Javalib.class_path !classpath in

        (* Launch the analysis on each given class *)
        List.iter
          (fun cn_string -> 
             main cp !path_output cn_string (*==> Next function to write *)
          ) 
          !targets
    with e ->
      (* If an exception is raised, it will appear on the standard
         error output and then in the Error Log view of Eclipse*)
      raise e

(* print the html representation of the class [cn_string] at A3Bir
  level with the default formula enabled. [cp] is the java classpath and
  [outputDir] is the directory in which the html file will be
  generated. *)

let print cp cn_string outputDir =
  let cn = JBasics.make_cn cn_string in
  let ioc = Javalib.get_class cp cn in
  let bir_ioc = Javalib.map_interface_or_class_context 
                  (A3Bir.transform ~formula:true ) ioc 
  in
    A3Bir.print_class bir_ioc outputDir

(* Example of use in the same folder as the Java class *)
let main = 
  let cp = Javalib.class_path "." in
    print cp "TestFormula" "html_dir"

(* print the html representation of the class [cn_string] at A3Bir
  level with a personnalized formula enabled. [cp] is the java
  classpath and [outputDir] is the directory in which the html file
  will be generated. *)

let print cp cn_string outputDir =
  let cn = JBasics.make_cn cn_string in
  let ioc = Javalib.get_class cp cn in
  let fhandler = 
   let cn_formula_cl = JBasics.make_cn "MyFormulaClass" in
   let ms1_formula = JBasics.make_ms "myFormulaFun1" [JBasics.TBasic `Bool] None in
   let ms2_formula = JBasics.make_ms "myFormulaFun2" [JBasics.TBasic `Bool] None in
       [JBasics.make_cms cn_formula_cl ms1_formula; 
        JBasics.make_cms cn_formula_cl ms2_formula ]
  in
  let bir_ioc = Javalib.map_interface_or_class_context 
                  (A3Bir.transform ~formula:true ~formula_cmd:fhandler) ioc 
  in
    A3Bir.print_class bir_ioc outputDir

