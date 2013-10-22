(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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


let replace_forb_xml_ch ?(repl_amp=false) s =
    ExtString.String.replace_chars
      (function
	   (* This one is problematic if replacements are already done*)
	   '&' when repl_amp -> "&amp;"
	 | '<' -> "&lt;"
	 | '>' -> "&gt;"
	 | '"' -> "&quot;"
	 | '\'' -> "&apos;"
	 | c -> String.make 1 c)
      s

let mkdirp path perm =
  let rec mkdirp path =
    try
      Unix.mkdir path perm
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      let parent = Filename.dirname path in
	if parent <> path then mkdirp parent;
	Unix.mkdir path perm 
  in
    mkdirp path

type xml_tree = | CustomTag of string * (xml_tree list) * string
		| SimpleTag of string
		| PCData of string
		| CData of string
		     

let gen_tag_attributes attributes =
  String.concat " "
    (List.map (fun (k,v) -> k ^ "=" ^ "\"" ^ v ^ "\"") attributes)
    
let gen_opening_tag ?(iscustom=true) tagname attributes =      
    let attributes = 
    List.map
      (fun (k,v) -> 
	 replace_forb_xml_ch k, replace_forb_xml_ch v)
      attributes
  in
  let tag_attributes = (gen_tag_attributes attributes) in
    "<" ^ tagname
    ^ (if tag_attributes <> "" then " " else "")
    ^ tag_attributes
    ^ (if iscustom then "" else " /") ^ ">"
      
let gen_closing_tag tagname =
  "</" ^ tagname ^ ">"

let gen_custom_tag tagname attributes xmltree =
  let opening_tag = gen_opening_tag tagname attributes
  and closing_tag = gen_closing_tag tagname in
    CustomTag (opening_tag, xmltree, closing_tag)
      
let gen_simple_tag tagname attributes =
  SimpleTag(gen_opening_tag ~iscustom:false tagname attributes)

let create_package_dir outputdir package =
  match package with
    | [] -> ()
    | hd :: tl ->
	let perm = 0o777 in
	let create_dir dirname =
	  if not(Sys.file_exists dirname
		 && Sys.is_directory dirname) then
	    mkdirp dirname perm in
	let dirname =
	  List.fold_left
	    (fun dirname basename ->
	       create_dir dirname;
	       Filename.concat dirname basename
	    ) (Filename.concat outputdir hd) tl in
	  create_dir dirname
	    
let replace_pcdata_ch s =
  ExtString.String.replace_chars
    (function
       | '<' -> "&lt;"
       | '>' -> "&gt;"
       | '"' -> "&quot;"
       | '\'' -> "&apos;"
       | '\n' -> "<br/>"
       | c -> String.make 1 c)
    s

let print_xml_tree_ext ?(br=true) ?(spc=0) xmltree out =
  let rec print dec xmltree =
    let spc = String.make (dec * spc) ' ' in
      match xmltree with
	| CustomTag (opening,tree,closing) ->
	    if br then
	      IO.write out '\n';
	    IO.nwrite out spc;
	    IO.nwrite out opening;
	    List.iter (fun tree -> print (dec+1) tree) tree;
	    if br then
	      IO.write out '\n';
	    IO.nwrite out spc;
	    IO.nwrite out closing;
	    
	| SimpleTag tag ->
	    if br then
	      IO.write out '\n';
	    IO.nwrite out spc;
	    IO.nwrite out tag;
	| PCData data ->
	    let data = replace_pcdata_ch data in
	      if br then
		IO.write out '\n';
	      IO.nwrite out spc;
	      IO.nwrite out data;
	| CData data ->
	    if br then
	      IO.write out '\n';
	    IO.nwrite out spc;
	    IO.nwrite out "<![CDATA[";
	    IO.nwrite out data;
	    IO.nwrite out "]]>";

  in
    print 0 xmltree

let print_xml_tree ?(spc=0) xmltree out =
  let out = IO.output_channel out in
    print_xml_tree_ext ~spc:spc xmltree out

open Javalib_pack    
open JBasics
open Javalib

module JCodeUtil = 
  struct 
    open JCode
    let iter_code f code =
	Array.iteri
	  (fun pp opcode ->
	     match opcode with
	       | OpInvalid -> ()
	       | _ -> f pp [opcode]
	  ) code.c_code
	  
    let method_param_names ioc ms =
      let m = Javalib.get_method ioc ms in
      	match m with
      	  | AbstractMethod _
      	  | ConcreteMethod {cm_implementation = Native} -> None
      	  | ConcreteMethod ({cm_implementation = Java code} as cm) ->
      	      let is_static = cm.cm_static in
      		Some
      		  (ExtList.List.mapi
      		     (fun i _ ->
      			let n = if is_static then i else i + 1 in
      			let v = get_local_variable_info n 0 (Lazy.force code) in
      			  match v with
      			    | None -> string_of_int n
      			    | Some (name,_) -> name
      		     ) (ms_args ms)
      		  )
  end

