open Javalib_pack
open JBasics
open Javalib

open Sawja_pack

(** Fill debug information, should be use in case which dead
    affectations are detected*)
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
      (* We give information of the variables liveness for this
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
	code.JBir.code;
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
    
(**[method_dead_affect cn ms code live] returns false if no dead
   affectation are found (true otherwise) and modify data structure
   containing information for the Eclipse plugin. [cn] is a
   class_name, [ms] a method_signature, [code] the JBir code of the
   method and [live] the result of the live analysis on the
   code. [plugin_infos] is the data structure to fill for the
   plugin.*)
let method_dead_affect cn ms code live plugin_infos =
  (*Corner cases: "false" positive on AffectVar instruction*)
  let not_corner_case i op = 
    (* AffectVar instruction corresponds to a catch(Exception e) statement*)
    let not_catch_var = 
      List.for_all
	(fun exc_h -> not(i = exc_h.JBir.e_handler))
	code.JBir.exc_tbl
    in not_catch_var
  in
  let dead_var_exists = ref false in
    Array.iteri 
      (fun i op -> 
	 (match op with 
	    | JBir.AffectVar (var,expr) when not_corner_case i op -> 
		(* Variable is dead at next instruction ?*)
		let live_res = live (i+1) in
		let dead_var = not (Live_bir.Env.mem var live_res) in
		  if dead_var
		  then 
		    begin
		      (* We add a warning on the dead instruction*)
		      let warn_pp = 
			match JBir.var_name_debug var with
			    None -> 
			      (Printf.sprintf 
				 "Variable of type '%s' is affected but never read !" 
				 (JPrint.value_type (JBir.type_of_expr expr)),None)
			  | Some name ->
			      (Printf.sprintf 
				 "Variable '%s' is affected but never read !" 
				 name,None)
		      in
			plugin_infos.JPrintPlugin.p_warnings <- 
			  JPrintPlugin.add_pp_iow warn_pp cn ms i plugin_infos.JPrintPlugin.p_warnings;
			dead_var_exists := true
		    end
	    | _ -> ()))
      code.JBir.code;
    (* Fill information for plugin depending on the method analysis result*)
    fill_debug_infos !dead_var_exists cn ms code live plugin_infos
		    
      
(**[method_dead_affect ioc] returns false if no dead affectation are
   found in the class [ioc] (true otherwise) and the data structure
   containing information for the Eclipse plugin. [ioc] is the
   interface_or_class to check.*)
let run_dead_affect ioc =
  let plugin_infos = JPrintPlugin.JBirPrinter.empty_infos in
    cm_iter
      (fun cm ->
	 match cm.cm_implementation with 
	     Native -> ()
	   | Java lazc -> 
	       let code = Lazy.force lazc in
	       let live = Live_bir.run code in
	       let (cn,ms) = cms_split cm.cm_class_method_signature in
		 method_dead_affect cn ms code live plugin_infos)
      ioc;
    plugin_infos

(**[main cp output cn_string] returns false if no dead affectation are
   found for the class [cn_string], true otherwise, and print the
   information for the plugin. [cp] is a class_path, [output] a folder
   path for generation of plugin information and [cn_string] the fully
   qualified name of a Java class file.*)
let main cp output cn_string =
  let cn = make_cn cn_string in
  let ioc = get_class cp cn in
  let bir_ioc = map_interface_or_class_context (JBir.transform ~bcv:true) ioc in
  let plugin_infos = run_dead_affect bir_ioc in
    (* Print infos on the current class for the Eclipse plugin*)
    JPrintPlugin.JBirPrinter.print_class plugin_infos bir_ioc output
      
(** [_plugin_exec] parses arguments of executable and executes the
    analysis for each class file given by the arguments*)
let _plugin_exec = 
  let targets = ref []
  and classpath = ref "" 
  and path_output = ref "" in
  let exec_args = 
    [ ("--files", "Class files",
       ArgPlugin.ClassFiles (fun sl -> targets := sl),
       "The class files to pass to the dead affectation checker.");
      ("--classpath", "Class path",
       ArgPlugin.ClassPath (fun s -> classpath := s),
       "Locations where class files are looked for.");
    ]
  in
  let usage_msg = "Usage: " ^Sys.argv.(0)^ " [options]" in
    ArgPlugin.parse
      ("DVAD","Dead Variables Affected Detection: detects variables affected but never read.")
      exec_args
      (ArgPlugin.PluginOutput ("--plug_output",(fun s -> path_output := s)))
      usage_msg;
    try
      let cp = class_path !classpath in
	List.map
	  (fun cn_string -> 
	     ignore(main cp !path_output cn_string))
	  !targets
    with e ->
      (* If an exception is raised, it will write on standard error
	 appears in the standard error output and then in the Error Log
	 view of Eclipse*)
      raise e
    
    
