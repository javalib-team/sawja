(*
 * This file is part of Jaws
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see 
 * <http://www.gnu.org/licenses/>.
 *)

open JBasics
open JCode
open Javalib
open JProgram

let html_indent = 3

let class_class = "class"
let classname_class = "classname"
let methodname_class = "methodname"
let field_class = "field"
let field_signature_class = "field_signature"
let method_class = "method"
let method_signature_class = "method_signature"
let code_class = "code"
let annot_class = "annot"
let instruction_class = "instruction"
let parameter_class = "parameter"
let clickable_class = "clickable"

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

let gen_span htmltree attributes =
  gen_custom_tag "span" attributes htmltree

let gen_titled_span htmltree attributes title =
  gen_custom_tag "span" (attributes @ ["title",title]) htmltree
  
let gen_div htmltree attributes =
  gen_custom_tag "div" attributes htmltree

let gen_hyperlink href info =
  gen_custom_tag "a" [("href",href)] [(PCData info)]

let gen_titled_hyperlink href info title =
  gen_custom_tag "a" [("href",href);("title",title)] [(PCData info)]

let gen_anchor name info =
  gen_custom_tag "a" [("name",name);("href","#")] [(PCData info)]

let gen_html_document htmltree =
  gen_custom_tag "html" [("xmlns","http://www.w3.org/1999/xhtml");
			 ("lang","en")] htmltree

let gen_html_head htmltree =
  gen_custom_tag "head" [] htmltree

let gen_html_title title =
  gen_custom_tag "title" [] [PCData title]

let gen_html_body htmltree =
  gen_custom_tag "body" [] htmltree

let gen_css_link href =
  gen_simple_tag "link" [("rel","StyleSheet"); ("href",href);
			 ("type","text/css"); ("media","all")]

let gen_javascript_src src =
  gen_custom_tag "script" [("type","text/javascript"); ("src",src)] []

let gen_annots annots =
  let ullist =
    List.map
      (fun annot ->
	 gen_custom_tag "li" [] [PCData annot]
      ) annots in
    match annots with
      | [] -> PCData ""
      | _ ->
	  gen_custom_tag "ul" [("class",annot_class)] ullist

let gen_hidden_list hlist =
  let ullist =
    List.map
      (fun elt ->
	 gen_custom_tag "li" [] [gen_div elt []]
      ) hlist in
    gen_custom_tag "ul" [("class",clickable_class)] ullist

type param = | SimpleParam of html_tree list 
	     | DynamicParam of (html_tree list) * ((html_tree list) list)

let gen_inst pp inst prm annots =
  let instruction = gen_span [inst] [("class",instruction_class)]
  and annotations = gen_annots annots in
  let parameter =
    match prm with
      | SimpleParam pl ->
	  (match pl with
	     | [] -> []
	     | _ ->
		 [gen_span pl [("class",parameter_class)]]
	  )
      | DynamicParam (label,l) ->
	  (gen_span label
	     [("class",parameter_class ^ " " ^ clickable_class);
	      ("onclick","showInfoList(this)")])
	  :: [gen_hidden_list l] in
    gen_custom_tag "li" [("value",string_of_int pp)]
      [gen_div ((instruction :: parameter) @ [annotations]) []]

let gen_code insts =
  let ollist =
    List.map
      (fun (pp,inst,prm,annots) ->
	 gen_inst pp inst prm annots
      ) insts in
    match insts with
      | [] -> PCData ""
      | _ ->
	  gen_custom_tag "ol" [("class",code_class)] ollist

let add_anchor anchor_name anchor_info htmllist =
  if (anchor_name <> "") then
    (gen_anchor anchor_name anchor_info) :: htmllist
  else
    htmllist

let gen_method anchor_name method_signature callers annots insts =
  let meth_sig = gen_div (method_signature @ [gen_annots annots] @ callers)
    [("class", method_signature_class)] in
  let meth_body = [meth_sig; gen_code insts] in
    gen_div (add_anchor anchor_name "" meth_body)
      [("class", method_class)]

let gen_field anchor_name field_signature annots =
  let field_sig = gen_span field_signature
    [("class", field_signature_class)] in
  let field_body = [field_sig; gen_annots annots] in
    gen_div (add_anchor anchor_name "" field_body) [("class",field_class)]

let gen_class anchor_name classname annots content =
  let class_name = gen_div classname [("class",classname_class)] in 
  let class_body = (class_name :: (gen_annots annots) :: content) in
    gen_div (add_anchor anchor_name "" class_body)
      [("class", class_class)]

let rec print_html_tree_to_fmt ?(isroot=true) htmltree fmt =
  match htmltree with
    | CustomTag (opening,tree,closing) ->
	if not isroot then
	  Format.pp_force_newline fmt ();
	Format.pp_open_vbox fmt html_indent;
	Format.pp_print_string fmt opening;
	let memsimpledata = ref false in
	  List.iter (fun tree ->
		       print_html_tree_to_fmt ~isroot:false tree fmt;
		       match tree with
			 | SimpleTag _ -> memsimpledata := false
			 | PCData _ -> memsimpledata := true
			 | _ -> memsimpledata := false
		    ) tree;
	  Format.pp_close_box fmt ();
	  if not !memsimpledata then
	    Format.pp_force_newline fmt ();
	  Format.pp_print_string fmt closing
    | SimpleTag tag ->
	Format.pp_force_newline fmt ();
	Format.pp_print_string fmt tag
    | PCData data ->
	Format.pp_print_string fmt data

let print_html_tree htmltree out =
  let b = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer b in
    Format.pp_set_formatter_out_channel fmt out;
    print_html_tree_to_fmt htmltree fmt;
    Format.pp_print_flush fmt ()

type info =
    { p_class : class_name -> string list;
      (** Prints class information that is printed inside the class, along with
	  other attributes of the class. *)
      p_field : class_name -> field_signature -> string list;
      (** Prints field information that is printed along with the corresponding
	  field. *)  
      p_method : class_name -> method_signature -> string list;
      (** Prints method information that is printed inside the method,
	  along with other attributes of the method. *)
      p_callers : class_name -> method_signature -> ClassMethSet.t;
      (** Prints information about the possible method callers. *)
      p_pp : class_name -> method_signature -> int -> string list;
      (** Prints information associated to program points. The information is
	  printed after the instruction. *)
    }

let void_info =
  { p_class = (fun _ -> []);
    p_field = (fun _ _-> []);
    p_method = (fun _ _ -> []);
    p_callers = (fun _ _ -> ClassMethSet.empty);
    p_pp = (fun _ _ _ -> []) }

let revert_callgraph program =
  let cg = get_callgraph program in
    List.fold_left
      (fun cmmap ((cs,ms,_),(ccs,cms)) ->
	 let ccmsig = make_cms ccs cms in
	 let cmset =
	   try ClassMethodMap.find ccmsig cmmap
	   with _ -> ClassMethSet.empty in
	   ClassMethodMap.add ccmsig (ClassMethSet.add (cs,ms) cmset) cmmap
      ) ClassMethodMap.empty cg

let get_callers rcg cs ms =
  let cmsig = make_cms cs ms in
    try ClassMethodMap.find cmsig rcg
    with _ -> ClassMethSet.empty

let get_program_info program p_class p_field p_method p_pp =
  { p_class = p_class;
    p_field = p_field;
    p_method = p_method;
    p_callers = get_callers (revert_callgraph program);
    p_pp = p_pp
  }

let rec get_relative_path frompackage topackage =
  match (frompackage,topackage) with
    | ([],[]) -> "./"
    | (fc :: t1, tc :: t2)->
	if (fc = tc) then get_relative_path t1 t2
	else
	  let s = ".." in
	  let l = List.map (fun _ -> s) frompackage in
	    (String.concat "/" l) ^ "/" ^ (String.concat "/" topackage) ^ "/"
    | (_ :: _, []) ->
	let s = ".." in
	let l = List.map (fun _ -> s) frompackage in
	  (String.concat "/" l) ^ "/"
    | ([], _ :: _) ->
	"./" ^ (String.concat "/" topackage) ^ "/"

let get_relative_file fromclass toclass =
  let p1 = cn_package fromclass
  and p2 = cn_package toclass
  and c = cn_simple_name toclass  in
    (get_relative_path p1 p2) ^ c ^ ".html"
    
let rec valuetype2html program t currentclass =
  match t with
    | TBasic _ -> [PCData (JPrint.value_type t)]
    | TObject o ->
	(match o with
	   | TClass cs ->
	       if (ClassMap.mem cs program.classes) then
		 [gen_hyperlink (get_relative_file currentclass cs)
		    (cn_name cs)]
	       else
		 [PCData (cn_name cs)]
	   | TArray a ->
	       let res = valuetype2html program a currentclass in
		 (match res with
		    | [PCData d] -> [PCData (d ^ "[]")]
		    | (CustomTag (bt,c,et)) :: tl ->
			(CustomTag (bt,c,et)) :: (PCData "[]") :: tl
		    | _ -> failwith "Error in valuetype2html function"
		 )
	)

let returntype2html program rt currentclass =
  match rt with
    | None -> [PCData "void"]
    | Some t -> valuetype2html program t currentclass

let get_class_name program cs =
  let ioc = get_node program cs in get_name ioc

let cn2anchorname = cn_name

let rec type2anchorstring t =
  match t with
    | TObject (TClass cn) -> cn_name cn
    | TObject (TArray a) -> (type2anchorstring a) ^ ".-"
	(* ] is prohibited... *)
    | _ -> JPrint.value_type ~jvm:true t

let rettype2anchorstring rt =
  match rt with
    | Some t -> type2anchorstring t
    | None -> "void"

let fs2anchorname cs fs =
  let fname = fs_name fs in
  let fdesc = fs_type fs in
  let fstype2string = type2anchorstring fdesc in
    (cn2anchorname cs) ^ ":" ^ fname ^ ":" ^ fstype2string
      
let ms2anchorname cs ms =
  let mname = ms_name ms in
  let mparameters = ms_args ms in
  let mreturntype = ms_rtype ms in
  let msname = Str.global_replace (Str.regexp_string "<") "-"
    (Str.global_replace (Str.regexp_string ">") "-" mname) in
    (cn2anchorname cs) ^ ":" ^ msname ^ ":"
    ^ (String.concat ""
	 (List.map type2anchorstring mparameters))
    ^ (rettype2anchorstring mreturntype)

let access2string access =
  match access with
    | `Default -> ""
    | `Public -> "public"
    | `Private -> "private"
    | `Protected -> "protected"

let fieldkind2string (fk : Javalib.field_kind) =
  match fk with
    | NotFinal -> ""
    | Final -> "final"
    | Volatile -> "volatile"

let fieldsignature2html program cs fs =
  let iocfield = get_field (get_node program cs) fs in
  let header =
    match iocfield with
      | InterfaceField _ -> "public static"
      | ClassField f ->
	  (access2string f.cf_access)
	  ^ (if f.cf_static then " static" else "")
	  ^ " " ^ (fieldkind2string f.cf_kind) in
  let fname = fs_name fs in
  let ftype = fs_type fs in
    (PCData (header ^ " ")) :: (valuetype2html program ftype cs)
    @ [PCData (" " ^ fname ^ ";")]

let make_methodsignature2html cs ms f =
  let cname = cn_name cs in
  let ms2string ms = JPrint.method_signature ms in
  let chomp_ret_type_htmlize msstring =
    ExtString.String.replace_chars
      (fun c ->
	 match c with
	   | '<' -> "&lt;"
	   | '>' -> "&gt;"
	   | _ -> String.make 1 c)
      msstring in
  let msstr = chomp_ret_type_htmlize (ms2string ms) in
  let mssimple = snd (ExtString.String.split msstr " ") in
    f cname mssimple msstr

let methodcallers2html cs ms info =
  let callers = info.p_callers cs ms in
    if (callers = ClassMethSet.empty) then []
    else
      let callerslist = ClassMethSet.elements callers in
      let hl = List.map
	(fun (ccs,cms) ->
	   let anchor = ms2anchorname ccs cms in
	   let href = (get_relative_file cs ccs) ^ "#" ^ anchor in
	     make_methodsignature2html ccs cms 
	       (fun name mssimple msstr ->
		  [gen_titled_hyperlink href (name ^ "." ^ mssimple) msstr]
	       )
	) callerslist in
	[gen_hidden_list hl]

let methodname2html cs ms info mname =
  let callers = info.p_callers cs ms in
    if (callers = ClassMethSet.empty) then
      [gen_span [PCData mname] []]
    else
      [gen_span [PCData mname]
	 [("class",methodname_class ^ " " ^ clickable_class);
	  ("onclick","showInfoList(this)")]]

let get_local_variable_ident i pp m =
  match m with
    | AbstractMethod _
    | ConcreteMethod {cm_implementation = Native} -> string_of_int i
    | ConcreteMethod {cm_implementation = Java code} ->
        let v = get_local_variable_info i pp (Lazy.force code) in
          match v with
            | None -> string_of_int i
            | Some (name,_) -> name

let get_local_variable_signature i pp m =
  match m with
    | AbstractMethod _
    | ConcreteMethod {cm_implementation = Native} -> None
    | ConcreteMethod {cm_implementation = Java code} ->
        let v = get_local_variable_info i pp (Lazy.force code) in
          match v with
            | None -> None
            | Some (_,signature) -> Some signature

let methodsignature2html program cs ms info =
  let meth = get_method (get_node program cs) ms in
  let mname = ms_name ms in
  let mparameters = ms_args ms in
  let mreturntype = ms_rtype ms in
  let header =
    match meth with
      | AbstractMethod am ->
	  (access2string am.am_access) ^ " abstract"
      | ConcreteMethod cm ->
	  (access2string cm.cm_access)
	  ^ (if cm.cm_static then " static" else "")
	  ^ (if cm.cm_final then " final" else "")
	  ^ (match cm.cm_implementation with Native -> " native" | _ -> "")
	  ^ (if cm.cm_synchronized then " synchronized" else "") in
  let list_concat l =
    match l with
      | [] -> []
      | hd :: tl ->
	  List.fold_left (fun x y -> x @ [PCData ", "] @ y) hd tl in
  let parameters2html () =
    list_concat (ExtList.List.mapi
		       (fun i x -> (valuetype2html program x cs)
			  @ [PCData (" "
				     ^ (get_local_variable_ident i 0 meth))])
		       mparameters) in
  let mname2html = methodname2html cs ms info in
    if (mname = "<clinit>") then
      let mname = mname2html "clinit" in
	(PCData (header ^ " ")) :: mname @ [PCData ("{};")]
    else if (mname = "<init>") then
      let mname = mname2html (cn_simple_name cs) in
	(PCData (header ^ " ")) :: mname
	@ [PCData "("]
	@ (parameters2html ())
	@ [PCData ");"]
    else
      let mname = mname2html mname in
	(PCData (header ^ " "))
	:: (returntype2html program mreturntype cs)
	@ (PCData (" ") :: mname) @ [PCData "("]
	@ (parameters2html ())
	@ [PCData ");"]

let iocsignature2html program cs =
  let ioc = get_node program cs in
    match ioc with
      | Class _ ->
	  [PCData ("Class " ^ (cn_name cs))]
      | Interface _ ->
	  [PCData ("Interface " ^ (cn_name cs))]

let field2html program cs fs annots =
  gen_field (fs2anchorname cs fs) (fieldsignature2html program cs fs) annots

let method2html program cs ms info insts =
  let annots = info.p_method cs ms in
  let method_signature = methodsignature2html program cs ms info in
  let callers = methodcallers2html cs ms info in
    gen_method (ms2anchorname cs ms) method_signature callers annots insts

let get_fields_indexes program cs =
  let ioc = get_node program cs in
  let l = ref [] in
    (match ioc with
       | Class c ->
	   FieldMap.iter (fun i _ -> l := i :: !l) c.c_info.c_fields
       | Interface i ->
	   FieldMap.iter (fun i _ -> l := i :: !l) i.i_info.i_fields
    ); !l

let staticlookup2html program cs ms pp callcs callms =
  let mlookups =
    try List.map cms_split
      (ClassMethodMap.key_elements (program.static_lookup_method cs ms pp))
    with _ -> [] in
  let mlookupshtml = List.map
    (fun (rcs,rms) ->
       let anchor = ms2anchorname rcs rms in
       let href = (get_relative_file cs rcs) ^ "#" ^ anchor in
	 make_methodsignature2html rcs rms
	   (fun name mssimple msstr ->
	      [gen_titled_hyperlink href (name ^ "." ^ mssimple) msstr]
	   )
    ) mlookups in
    DynamicParam ([make_methodsignature2html callcs callms
		     (fun name mssimple msstr ->
			gen_titled_span
			  [PCData (name ^ "." ^ mssimple)] [] msstr)],
		  match mlookupshtml with
		    | [] -> [[PCData "No reachable result."]]
		    | _ -> mlookupshtml)

let opcodeparam2param program cs ms pp op prmstr =
    match op with
      | OpNew ccs ->
	  let v = TObject (TClass ccs) in
	    SimpleParam (valuetype2html program v cs)
      | OpNewArray v ->
	    SimpleParam (valuetype2html program v cs)
      | OpAMultiNewArray (o,i) ->
	  let v = TObject o in
	    SimpleParam ((valuetype2html program v cs)
			 @ [PCData (" " ^ (string_of_int i))])
      | OpCheckCast o | OpInstanceOf o ->
	  let v = TObject o in
	    SimpleParam (valuetype2html program v cs)
      | OpGetStatic (ccs,fs) | OpPutStatic (ccs,fs)
      | OpGetField (ccs,fs) | OpPutField (ccs,fs) ->
	  let file = get_relative_file cs ccs in
	  let anchor = fs2anchorname ccs fs in
	  let href =
	    if (cn_equal cs ccs) then "#" ^ anchor
	    else file ^ "#" ^ anchor in
	  let fname = fs_name fs in
	  let ftype = fs_type fs in
	  let fieldtype = valuetype2html program ftype cs in
	    SimpleParam ([gen_titled_hyperlink href fname prmstr;
			  PCData " : "] @ fieldtype)
      | OpInvoke ((`Virtual (TClass ccs)),cms) ->
	  staticlookup2html program cs ms pp ccs cms
      | OpInvoke ((`Interface ccs),cms)
      | OpInvoke ((`Static ccs),cms)
      | OpInvoke ((`Special ccs),cms) ->
	    staticlookup2html program cs ms pp ccs cms
      | OpLoad (_,n) | OpStore (_,n) | OpRet n ->
	  let ioc = get_node program cs in
	  let meth = get_method ioc ms in
	  let prm = get_local_variable_ident n pp meth in
	  let signature = get_local_variable_signature n pp meth in
	    SimpleParam ((PCData prm)
			 :: match signature with
			   | None -> []
			   | Some s -> 
			       (PCData " : ")
			       :: (valuetype2html program s cs)
			)
      | _ ->
	  SimpleParam [PCData prmstr]

let opcode2inst program cs ms (pp : int) op (annots : string list) =
  let opdump = (JDump.opcode op) in
    try
      let (opcode,prm) = ExtString.String.split opdump " " in
	(pp,PCData opcode,opcodeparam2param program cs ms pp op prm,annots)
    with ExtString.Invalid_string ->
      (pp,PCData opdump,SimpleParam [],annots)

let ioc2html program cs info =
  let ioc = get_node program cs in
  let fields =
    List.fold_left
      (fun l fs ->
	 (field2html program cs fs (info.p_field cs fs)) :: l)
      [] (get_fields_indexes program cs) in
  let methods =
    MethodMap.fold
      (fun _ m l ->
	 let ms = get_method_signature m in
	 let insts =
	   match m with
	     | AbstractMethod _ -> []
	     | ConcreteMethod cm ->
		 (match cm.cm_implementation with
		    | Native -> []
		    | Java code ->
			let l = ref [] in
			let c = Lazy.force code in
			  Array.iteri
			    (fun pp op ->
			       match op with
				 | OpInvalid -> ()
				 | _ -> l := (pp,op) :: !l) c.c_code;
			  let instlist = List.rev !l in
			    List.map
			      (fun (pp,op) -> opcode2inst program cs ms pp op
				 (info.p_pp cs ms pp)) instlist
		 )
	 in
	   (method2html program cs ms info insts) :: l)
      (get_methods ioc) [] in
  let content = (List.rev fields) @ (List.rev methods) in
    gen_class (cn2anchorname cs)
      (iocsignature2html program cs) (info.p_class cs) content

let gen_class_document program cs info css js =
  let ioc = get_node program cs in
  let cname = cn_name (get_name ioc) in
  gen_html_document
    [(gen_html_head
	[(gen_html_title cname);
	 (gen_css_link css);
	 (gen_javascript_src js)]);
     (gen_html_body
	[ioc2html program cs info]
     )]

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
	       dirname ^ "/" ^ basename) (outputdir ^ "/" ^ hd) tl in
	  create_dir dirname

(* WARNING: do not edit the next line, the CSS comment will be
   replaced by the actual CSS. (cf. Makefile) *)
let css = "(* CSS *)"

let js = "function showInfoList(e){
    var siblings = e.parentNode.childNodes;
    var len = siblings.length;
    
    for(var i = 0; i < len; i++){
        var sibling = siblings[i];
    	if (sibling.nodeName == \"UL\"
	    && sibling.className == \"clickable\"){
	    var style = sibling.style;
	    if (style.visibility != \"visible\"){
		style.position = \"static\";
                style.visibility = \"visible\";
	    } else{
		style.visibility = \"hidden\";
                style.position = \"absolute\";
	    }
	}
    }
}"

let pp_print_program_to_html_files ?(css=css) ?(js=js) program outputdir info =
  let copy_file src dst =
    let outchan = open_out dst in
      output_string outchan src;
      close_out outchan
  and stylefile = "style.css"
  and jsfile = "actions.js"
  in
    copy_file css (outputdir ^ "/" ^ stylefile);
    copy_file js (outputdir ^ "/" ^ jsfile);
    ClassMap.iter
      (fun cs ioc ->
	 let cn = get_name ioc in
	 let package = cn_package cn
	 and cname = cn_simple_name cn in
	 let relative_css = (get_relative_path package []) ^ stylefile
	 and relative_js = (get_relative_path package []) ^ jsfile in
	 let doc = gen_class_document program cs info
	   relative_css relative_js in
	 let doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" in
	   create_package_dir outputdir package;
	   let out =
	     open_out (outputdir ^ "/" ^
			 String.concat "/" (package @ [cname ^ ".html"])) in
	     output_string out doctype;
	     print_html_tree doc out;
	     close_out out
      ) program.classes
