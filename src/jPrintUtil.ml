
type html_tree = | CustomTag of string * (html_tree list) * string
		 | SimpleTag of string
		 | PCData of string
		     

let gen_tag_attributes attributes =
  String.concat " "
    (List.map (fun (k,v) -> k ^ "=" ^ "\"" ^ v ^ "\"") attributes)
    
let gen_opening_tag ?(iscustom=true) tagname attributes =
  let tag_attributes = (gen_tag_attributes attributes) in
    "<" ^ tagname
    ^ (if tag_attributes <> "" then " " else "")
    ^ tag_attributes
    ^ (if iscustom then "" else " /") ^ ">"
      
let gen_closing_tag tagname =
  "</" ^ tagname ^ ">"

let gen_custom_tag tagname attributes htmltree =
  let opening_tag = gen_opening_tag tagname attributes
  and closing_tag = gen_closing_tag tagname in
    CustomTag (opening_tag, htmltree, closing_tag)
      
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
	    Unix.mkdir dirname perm in
	let dirname =
	  List.fold_left
	    (fun dirname basename ->
	       create_dir dirname;
	       Filename.concat dirname basename
	    ) (Filename.concat outputdir hd) tl in
	  create_dir dirname
	      
let print_html_tree ?(spc=0) htmltree out =
  let out = IO.output_channel out in
  let rec print dec htmltree =
    let spc = String.make (dec * spc) ' ' in
    match htmltree with
      | CustomTag (opening,tree,closing) ->
	  IO.write out '\n';
	  IO.nwrite out spc;
	  IO.nwrite out opening;
	  List.iter (fun tree -> print (dec+1) tree) tree;
	  IO.write out '\n';
	  IO.nwrite out spc;
	  IO.nwrite out closing;
	  
      | SimpleTag tag ->
	  IO.write out '\n';
	  IO.nwrite out spc;
	  IO.nwrite out tag;
      | PCData data ->
	  IO.write out '\n';
	  IO.nwrite out spc;
	  IO.nwrite out data;
  in
    print 0 htmltree

open Javalib_pack    
open JBasics
open Javalib

module JCodeUtil = 
  struct 
    open JCode
    let iter_code f lazy_code =
      let code = Lazy.force lazy_code in
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


module JBirUtil = 
struct
  
  let iter_code f lazy_code =
    try
      let code = Lazy.force lazy_code in
	Array.iteri (fun i ins -> f i [ins]) code.JBir.code
    with _ -> 
      print_endline "Lazy.force fail";
      ()
  let method_param_names ioc ms =
    let m = Javalib.get_method ioc ms in
      match m with
	| AbstractMethod _
	| ConcreteMethod {cm_implementation = Native} -> None
	| ConcreteMethod ({cm_implementation = Java code} as cm) ->
	    try
	      let code = Lazy.force code in
	      let is_static = cm.cm_static in
		Some
		  (ExtList.List.mapi
		     (fun i _ ->
			let n = if is_static then i else i + 1 in
			let var = snd (List.nth code.JBir.params n) in
			  JBir.var_name_g var
		     ) (ms_args ms)
		  )
	    with _ -> None
end
  
module A3BirUtil = 
struct
  let iter_code f lazy_code =
    try
      let code = Lazy.force lazy_code in
	Array.iteri (fun i ins -> f i [ins]) code.A3Bir.code
    with
	_ -> ()
	  
  let method_param_names ioc ms =
    let m = Javalib.get_method ioc ms in
      match m with
	| AbstractMethod _
	| ConcreteMethod {cm_implementation = Native} -> None
	| ConcreteMethod ({cm_implementation = Java code} as cm) ->
	    try
	      let code = Lazy.force code in
	      let is_static = cm.cm_static in
		Some
		  (ExtList.List.mapi
		     (fun i _ ->
			let n = if is_static then i else i + 1 in
			let var = snd (List.nth code.A3Bir.params n) in
			  A3Bir.var_name_g var
		     ) (ms_args ms)
		  )
	    with _ -> None
end

module JBirSSAUtil = 
struct
  let iter_code f lazy_code =
    try
      let code = Lazy.force lazy_code in
	Array.iteri (fun i ins -> f i [ins]) code.JBirSSA.code
    with _ -> 
      print_endline "Lazy.force fail";
      ()

  let method_param_names ioc ms =
    let m = Javalib.get_method ioc ms in
      match m with
	| AbstractMethod _
	| ConcreteMethod {cm_implementation = Native} -> None
	| ConcreteMethod ({cm_implementation = Java code} as cm) ->
	    try
	      let code = Lazy.force code in
	      let is_static = cm.cm_static in
		Some
		  (ExtList.List.mapi
		     (fun i _ ->
			let n = if is_static then i else i + 1 in
			let var = snd (List.nth code.JBirSSA.params n) in
			  JBirSSA.var_name_g var
		     ) (ms_args ms)
		  )
	    with _ -> None	      
end

module A3BirSSAUtil = 
struct
  let iter_code f lazy_code =
    try
      let code = Lazy.force lazy_code in
	Array.iteri (fun i ins -> f i [ins]) code.A3BirSSA.code
    with
	_ -> ()
	  
  let method_param_names ioc ms =
    let m = Javalib.get_method ioc ms in
      match m with
	| AbstractMethod _
	| ConcreteMethod {cm_implementation = Native} -> None
	| ConcreteMethod ({cm_implementation = Java code} as cm) ->
	    try
	      let code = Lazy.force code in
	      let is_static = cm.cm_static in
		Some
		  (ExtList.List.mapi
		     (fun i _ ->
			let n = if is_static then i else i + 1 in
			let var = snd (List.nth code.A3BirSSA.params n) in
			  A3BirSSA.var_name_g var
		     ) (ms_args ms)
		  )
	    with _ -> None
end
