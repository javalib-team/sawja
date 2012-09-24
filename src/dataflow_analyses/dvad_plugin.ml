(*
 * This file is part of SAWJA
 * Copyright (c)2011 Vincent Monfort (INRIA)
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
open Sawja_pack

(* Colors for HTML code*)
let colors = ["red";"green";"blue";"orange";"grey"
	     ;"darkred";"darkgreen";"darkblue";"goldenrod"
	     ;"black";"indigo";"sienna";"seagreen";"crimson"]
let modulo = List.length colors


(** Fill debug information, depending if dead affectations are
    detected*)
let fill_debug_infos dead_found cn ms code live plugin_infos =
  (* Do not add additional warning on method signature for real use*)
  (*let _warns = 
    if dead_found
    then
    plugin_infos.JPrintPlugin.p_warnings <- 
    JPrintPlugin.add_meth_iow 
    (JPrintPlugin.MethodSignature "The method contains dead affectation(s).")
    cn ms plugin_infos.JPrintPlugin.p_warnings*)
  let _infos = 
    if dead_found
    then
      begin
	(* We give information of the variables liveness for this
	   method*)
	Array.iteri
	  (fun i _ -> 
	     let info_live = 
	       (* Use HTML for a better display in Eclipse*)
	       let html_rep live = 
		 let cpt = ref 0 in
		   snd 
		     (Live_bir.Env.fold 
			(fun var (pre,html) -> 
			   incr cpt;
			   (", ",html^pre
			      ^"<span style=\"color:"
			      ^(List.nth colors ((!cpt-1) mod modulo))^";\">"
			      ^JBir.var_name_g var^"</span>"
			   ))
			live
			("",""))
	       in
		 Printf.sprintf ("<div><b>LIVE[%s]</b> = {%s}</div>")
		   (string_of_int i)
		   (html_rep (live i))
	     in
	       plugin_infos.JPrintPlugin.p_infos <- 
		 JPrintPlugin.add_pp_iow info_live cn ms i plugin_infos.JPrintPlugin.p_infos
	  )
	  (JBir.code code);
	plugin_infos.JPrintPlugin.p_infos <- 
	  JPrintPlugin.add_meth_iow 
	  (JPrintPlugin.MethodSignature "<div>Dead variable affectation(s) found</div>") 
	  cn ms plugin_infos.JPrintPlugin.p_infos ;
      end
    else
      plugin_infos.JPrintPlugin.p_infos <- 
	JPrintPlugin.add_meth_iow 
	(JPrintPlugin.MethodSignature "<div>No dead variable affectation found</div>") 
	cn ms plugin_infos.JPrintPlugin.p_infos;
  in ()

(**[method_dead_affect cn ms code live plug_info ] modifies the data
   structure [plug_info] containing information for the Eclipse
   plugin. [cn] is a class_name, [ms] a method_signature, [code] the
   JBir code of the method and [live] the result of the live analysis
   on the code.*)
let method_dead_affect cn ms code live plugin_infos =
  (*Corner cases: "false" positive on AffectVar instruction*)
  let not_corner_case i _op = 
    (* AffectVar instruction corresponds to a catch(Exception e) statement*)
    let not_catch_var i = 
      List.for_all
	(fun exc_h -> not(i = exc_h.JBir.e_handler))
	(JBir.exc_tbl code)
    in not_catch_var i
  in
  let dead_var_exists = ref false in
    Array.iteri 
      (fun i op -> 
	 (match op with 
	    | JBir.AffectVar (var,expr) when not_corner_case i op -> 
		(* Is the variable dead at next instruction ?*)
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
      (JBir.code code);
    (* Fill information for plugin depending on the method analysis result*)
    fill_debug_infos !dead_var_exists cn ms code live plugin_infos
	    
      



(**[method_dead_affect ioc] returns the data structure containing
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
	       let live = Live_bir.run code in
	       let (cn,ms) = JBasics.cms_split cm.Javalib.cm_class_method_signature in
		 method_dead_affect cn ms code live plugin_infos)
      ioc;
    plugin_infos




(**[main cp output cn_string] loads the class [cn_string], converts it
   in JBir representation, runs the analysis and print the information
   for the plugin. [cp] is a class_path, [output] a folder path for
   generation of plugin information and [cn_string] the fully
   qualified name of a Java class file.*)
let main cp output cn_string =
  let cn = JBasics.make_cn cn_string in
  let ioc = Javalib.get_class cp cn in
  let bir_ioc = Javalib.map_interface_or_class_context JBir.transform ioc in
  let plugin_infos = run_dead_affect bir_ioc in
    (* Print infos on the current class for the Eclipse plugin*)
    JBir.PluginPrinter.print_class ~html_info:true plugin_infos bir_ioc output




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
      let cp = Javalib.class_path !classpath in
	List.iter
	  (fun cn_string -> 
	     main cp !path_output cn_string)
	  !targets
    with e ->
      (* If an exception is raised, it will appear on the standard
	 error output and then in the Error Log view of Eclipse*)
      raise e
    
    
