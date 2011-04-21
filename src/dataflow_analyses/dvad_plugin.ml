open Javalib_pack
open JBasics
open Javalib

open Sawja_pack

(** Fill debug information, should be use in case which dead
    affectations are detected*)
let fill_debug_infos dead_found cn ms code live (iref,wref) =
  let warns = 
    if dead_found
    then
      JPrintPlugin.add_meth_iow 
	(JPrintPlugin.MethodSignature "The method contains dead affectation(s).")
	cn ms !wref
    else
      !wref
  and infos = 
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
	     iref := JPrintPlugin.add_pp_iow info_live cn ms i !iref
	)
	code.JBir.code;
	iref := 
	  JPrintPlugin.add_meth_iow 
	    (JPrintPlugin.MethodSignature "Dead variable affectation(s) found") 
	    cn ms !iref;
      end
    else
      iref := 
	JPrintPlugin.add_meth_iow 
	  (JPrintPlugin.MethodSignature "No dead variable affectation found") 
	  cn ms !iref;
    
    !iref
  in
    (infos,warns)
    
(**[method_dead_affect cn ms code live] returns false if no dead
   affectation are found (true otherwise) and data structures
   containing information for the Eclipse plugin. [cn] is a
   class_name, [ms] a method_signature, [code] the JBir code of the
   method and [live] the result of the live analysis on the code.*)
let method_dead_affect cn ms code live (i,w) =
  let wref = ref w 
  and iref = ref i
  and w_gen = ref false 
  in
    Array.iteri 
      (fun i op -> 
	 (match op with 
	      JBir.AffectVar (var,_) -> 
		(* Variable is dead at next instruction ?*)
		 let live_res = live (i+1) in
		   if not (Live_bir.Env.mem var live_res)
		   then 
		     begin
		       (* We add a warning on the dead instruction*)
		       let warn_pp = 
			 (Printf.sprintf 
			   "Variable '%s' is affected but never read !" 
			   (JBir.var_name_g var),None)
		       in
			 w_gen := true;
			 wref := JPrintPlugin.add_pp_iow warn_pp cn ms i !wref;
		     end
	     | _ -> ()))
       code.JBir.code;
    (!w_gen, fill_debug_infos !w_gen cn ms code live (iref,wref))

      
(**[method_dead_affect ioc] returns false if no dead affectation are
   found in the class [ioc] (true otherwise) and data structures
   containing information for the Eclipse plugin. [ioc] is the
   interface_or_class to check.*)
let run_dead_affect ioc =
  cm_fold
    (fun cm (dead_vars,(infos,warns)) ->
       match cm.cm_implementation with 
	   Native -> (dead_vars,(infos,warns))
	 | Java lazc -> 
	     let code = Lazy.force lazc in
	     let live = Live_bir.run code in
	     let (cn,ms) = cms_split cm.cm_class_method_signature in
	     let (dvars,(i,w)) = method_dead_affect cn ms code live (infos,warns) in
	       (dvars or dead_vars),(i,w)
    )
    ioc
    (false,(ClassMap.empty, ClassMap.empty))

(**[main cp output cn_string] returns false if no dead affectation are
   found for the class [cn_string], true otherwise. [cp] is a
   class_path, [output] a folder path for generation of plugin
   information and [cn_string] the fully qualified name of a Java
   class file.*)
let main cp output cn_string =
  let cn = make_cn cn_string in
  let ioc = get_class cp cn in
  let bir_ioc = map_interface_or_class_context JBir.transform ioc in
  let (res,(p_infos,p_warns)) = run_dead_affect bir_ioc in
  let plugin_infos = 
    {JPrintPlugin.p_infos=p_infos; 
     JPrintPlugin.p_warnings=p_warns} 
  in
    JPrintPlugin.
      JBirPrinter.print_class plugin_infos bir_ioc output;
    res
      
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
    
    
