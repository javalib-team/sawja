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

open JPrintUtil

let xml_desc_arg = "--argumentsXMLDesc"

let args_tag = "arguments"

let analysis_tag = "analysis"

let output_tag = "output"
let cp_tag = "classpath"
let path_tag = "path"
let class_tag = "class"
let classes_tag = "classes"
let string_tag = "string"
let choice_tag = "choice"
let bool_tag = "bool"

let name_att = "name"
let sdesc_att = "short_desc"
let def_attr = "default"
let choices_attr = "choices"

type spec = 
  | ClassPath of (string -> unit)
  | Path of (string -> unit)
  | ClassFiles of (string list -> unit)
  | ClassFile of (string -> unit)
  | Choice of ((string list) * (string -> unit) * string)
  | String of ((string -> unit) * string option)
  | Boolean of ((bool -> unit) * bool option)
  | NotPlugin of Arg.spec

type analysis = string * string

type plugin_output = 
    PluginOutput of (string * (string -> unit))

type key = string 

type name = string

type doc = string 

type usage_msg = string 

let plug2arg = 
  function
      PluginOutput (key,f) -> 
	(key,Arg.String f," output path for Sawja's Eclipse plugin")

let transform2arg = function
    ClassPath f
  | Path f
  | ClassFile f -> Arg.String f
  | ClassFiles f -> Arg.String (fun s -> f (ExtString.String.nsplit s ":"))
  | Choice (list, f, _def) -> Arg.Symbol (list,f)
  | String (f, _def) -> Arg.String f
  | Boolean (f, _def) -> Arg.Bool f
  | NotPlugin spec -> spec

let gen_xml_desc (name, desc) arg_list plug_out_name =
  let arg_unit_list = 
    let get_tag_and_def = function
	ClassPath _ -> (cp_tag, [])
      | Path _ -> (path_tag, [])
      | ClassFile _ -> (class_tag, [])
      | ClassFiles _ -> (classes_tag, [])
      | Choice (list,_,def) -> 
	  (*ensures def is an element of the list*)
	  let ok = List.exists (fun s -> s = def) list in
	    if not ok 
	    then invalid_arg 
	      ("In ArgPlugin.Choice default value '"^def^"' is not included in "^
		 (List.fold_left (fun acc s -> acc^s^";") "[" list)^"]");
	    let choices,def = 
	      let replace = 
		(ExtString.String.replace_chars (function ';' -> ":" | c -> String.make 1 c))
	      in
		JUtil.print_list_sep_id ";" (List.map replace list), replace def
	    in
	      (choice_tag, [(choices_attr,choices);(def_attr,def)])
      | String (_,Some def) -> (string_tag, [(def_attr,def)])
      | String (_,None) -> (string_tag, [])
      | Boolean (_,Some def) -> (bool_tag,[(def_attr,string_of_bool def)])
      | Boolean (_,None) -> (bool_tag,[])
      | NotPlugin _ -> assert false
    in
      List.fold_right
	(fun (key,name,spec,doc) arg_list -> 
	   match spec with
	       NotPlugin _ -> arg_list
	     | _ -> 
		 let (tag_name,def) = get_tag_and_def spec in
		 let attrs = (name_att, key)::(sdesc_att,name)::def in
		 let desc = [PCData doc] in
		   (gen_custom_tag tag_name attrs desc)::arg_list
	)
	arg_list
	[]
  in
    gen_custom_tag args_tag []
      ((gen_custom_tag analysis_tag [(name_att,name)] [PCData desc])
	::(gen_simple_tag output_tag [(name_att,plug_out_name)])
	::arg_unit_list)
      	      

let parse analysis arg_list plugin_output usage_msg =
  let output_name = 
    match plugin_output with
	PluginOutput (name,_) -> name
  in
  let xml_desc = gen_xml_desc analysis arg_list output_name in
  let string_out = IO.output_string () in
  let sxml_desc = 
    print_xml_tree_ext xml_desc string_out;
    IO.close_out string_out
  in
  let args = 
    List.map 
      (fun (key, _name, spec, doc) -> (key, transform2arg spec, doc)) 
      arg_list
  in
  let new_args =
    args @ 
      [plug2arg plugin_output;
       (xml_desc_arg,
	Arg.Unit (fun () -> raise (Arg.Help sxml_desc)),
	" XML description of arguments for Sawja's Eclipse plugin")]
  in
    Arg.parse new_args (fun _ -> ()) usage_msg
