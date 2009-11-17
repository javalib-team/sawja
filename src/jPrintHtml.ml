(*
 * This file is part of SAWJA
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

(* TODO :
 * JPrintHTML should be more generic (functor ?) and able to
 * print any code representation. *)

open JBasics
open JCode
open Javalib
open JProgram
  
type info =

    { p_class : class_name -> string list;
      (** Prints class information that is printed inside the class,
	  along with other attributes of the class. *)
      p_field : class_name -> field_signature -> string list;
      (** Prints field information that is printed along with the
	  corresponding field. *)
      p_method : class_name -> method_signature -> string list;
      (** Prints method information that is printed inside the method,
	  along with other attributes of the method. *)
      p_pp : class_name -> method_signature -> int -> string list;
      (** Prints information associated to program points. The
	  information is printed after the instruction. *)
    }
      
let void_info =
  { p_class = (fun _ -> []);
    p_field = (fun _ _-> []);
    p_method = (fun _ _ -> []);
    p_pp = (fun _ _ _ -> []) }
    
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
  gen_custom_tag "span" (List.rev_append attributes ["title",title]) htmltree
    
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
      
type elem = | SimpleExpr of html_tree list
	    | DynamicExpr of (html_tree list) * ((html_tree list) list)
		 
let gen_inst inst_params =
  let (visible_parameters, hidden_parameters) =
    List.fold_right
      (fun prm (vl,hl) ->
	 match prm with
	   | SimpleExpr pl ->
	       (match pl with
		  | [] -> (vl,hl)
		  | _ ->
		      ((gen_span pl []) :: vl, hl)
	       )
	   | DynamicExpr (label,l) ->
	       let ve =
		 (gen_span label
		    [("class",parameter_class ^ " " ^ clickable_class);
		     ("onclick","showInfoList(this)")]) in
	       let he = gen_hidden_list l in
		 (ve :: vl, he :: hl)
      ) inst_params ([],[]) in
    gen_div (visible_parameters @ hidden_parameters)
      [("class",instruction_class)]
      
let gen_code insts =
  let ollist =
    let map =
      Ptmap.map
	(fun (insts,insts_annots) ->
	   (List.map 
	      (fun inst_params -> gen_inst inst_params) insts)
	   @ [gen_div [(gen_annots insts_annots)] []]
	) insts in
    let l = Ptmap.fold
      (fun pp insts l ->
	 l @ [(pp,gen_custom_tag "li" [("value",string_of_int pp)] insts)]
      ) map [] in
      snd (List.split (List.sort (fun (a,_) (b,_) -> compare a b) l)) in
    if (insts = Ptmap.empty) then PCData ""
    else gen_custom_tag "ol" [("class",code_class)] ollist
      
let add_anchor anchor_name anchor_info htmllist =
  if (anchor_name <> "") then
    (gen_anchor anchor_name anchor_info) :: htmllist
  else
    htmllist
      
let gen_method anchor_name method_signature callers method_annots insts =
  let meth_sig = gen_div (method_signature @ [gen_annots method_annots]
			  @ callers) [("class", method_signature_class)] in
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
    Format.pp_print_flush fmt ();
      
type info_internal = 
    {
      p_data : info;
      (** Prints information about the possible method callers. *)
      p_callers : class_name -> method_signature -> ClassMethSet.t option;
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
    
let htmlize s =
  ExtString.String.replace_chars
    (fun c ->
       match c with
	 | '<' -> "&lt;"
	 | '>' -> "&gt;"
	 | _ -> String.make 1 c) s

let methodcallers2html cs ms info =
  match info.p_callers cs ms with
    | None -> []
    | Some callers ->
	let callerslist = ClassMethSet.elements callers in
	let hl = List.map
	  (fun (ccs,cms) ->
	     let anchor = ms2anchorname ccs cms in
	     let href = (get_relative_file cs ccs) ^ "#" ^ anchor in
	     let ccname = cn_name ccs in
	     let cmname = htmlize (ms_name cms) in
	     let cmsig = htmlize (JPrint.method_signature cms) in
	       [gen_titled_hyperlink href (ccname ^ "." ^ cmname) cmsig]
	  ) callerslist in
	  [gen_hidden_list hl]
	  
let methodname2html cs ms info mname =
  match info.p_callers cs ms with
    | None ->
	[gen_span [PCData mname] []]
    | Some _ ->
	[gen_span [PCData mname]
	   [("class",methodname_class ^ " " ^ clickable_class);
	    ("onclick","showInfoList(this)")]]

let list_concat l =
  match l with
    | [] -> []
    | hd :: tl ->
	List.fold_left (fun x y -> x @ [PCData ", "] @ y) hd tl

let simple_elem s = SimpleExpr [PCData s]

let html_elem ht = SimpleExpr ht

let value_elem ?dim program cs v =
  match dim with
    | None ->
	SimpleExpr (valuetype2html program v cs)
    | Some n ->
	(SimpleExpr ((valuetype2html program v cs)
		      @ [PCData (" " ^ (string_of_int n))]))

let field_elem ?called_cname program cs ccs fs =
  let callcname = match called_cname with
    | Some x -> x
    | None -> cn_name ccs in
  let fname = fs_name fs in
    try
      let node = get_node program ccs in
      let rclist = JControlFlow.resolve_field fs node in
	match rclist with
	  | [c] -> let rcs = get_name c in
	    let rcname = match called_cname with
	      | Some x -> x
	      | None -> cn_name rcs in
	    let file = get_relative_file cs rcs in
	    let anchor = fs2anchorname rcs fs in
	    let href =
	      if (cn_equal cs rcs) then "#" ^ anchor
	      else file ^ "#" ^ anchor in
	      SimpleExpr [gen_hyperlink href (rcname ^ "." ^ fname)]
	  | _ ->
	      let flookuphtml = List.map
		(fun node ->
		   let rcs = get_name node in
		   let rcname = match called_cname with
		     | Some x -> x
		     | None -> cn_name rcs in
		   let file = get_relative_file cs rcs in
		   let anchor = fs2anchorname rcs fs in
		   let href =
		     if (cn_equal cs rcs) then "#" ^ anchor
		     else file ^ "#" ^ anchor in
		     [gen_hyperlink href (rcname ^ "." ^ fname)]
		) rclist in
		DynamicExpr ([gen_titled_span
				[PCData (callcname ^ "." ^ fname)] [] ""],
			     match flookuphtml with
			       | [] -> [[PCData "No reachable result."]]
			       | _ -> flookuphtml)
    with Not_found ->
      let callcname = match called_cname with
	| Some x -> x
	| None -> cn_name ccs in
	SimpleExpr [PCData (callcname ^ "." ^ fname)]
      
let invoke_elem ?called_cname program cs ms pp callcs callms =
  let mlookups =
    try List.map cms_split
      (ClassMethodSet.elements (program.static_lookup_method cs ms pp))
    with _ -> [] in
  let mlookupshtml = List.map
    (fun (rcs,rms) ->
       let anchor = ms2anchorname rcs rms in
       let href = (get_relative_file cs rcs) ^ "#" ^ anchor in
       let rcname = cn_name rcs in
       let rmname = htmlize (ms_name rms) in
       let rmsig = htmlize (JPrint.method_signature rms) in
	 [gen_titled_hyperlink href (rcname ^ "." ^ rmname) rmsig]
    ) mlookups in
  let callcname = match called_cname with
    | Some x -> x
    | None -> cn_name callcs in
  let callmname = htmlize (ms_name callms) in
  let callmsig = htmlize (JPrint.method_signature callms) in
    DynamicExpr ([gen_titled_span
		    [PCData (callcname ^ "." ^ callmname)] [] callmsig],
		 match mlookupshtml with
		   | [] -> [[PCData "No reachable result."]]
		   | _ -> mlookupshtml)

let method_args_elem program cs ms =
  let mparameters = ms_args ms in
  let prms =
    list_concat
      (List.map
	 (fun x -> (valuetype2html program x cs)) mparameters
      ) in
    html_elem ([PCData "("] @ prms @ [PCData ")"])

module type HTMLPrinter =
sig
  type code
    
  val print_program :
    ?css:string -> ?js:string -> ?info:info -> code program -> string -> unit
end

module type PrintInterface =
sig
  type instr
  type code
  val iter_code : (int -> instr -> unit) -> code Lazy.t -> unit
  val method_param_names : code program -> class_name -> method_signature
    -> string list option
  val inst_html : code program -> class_name -> method_signature -> int
    -> instr -> elem list
  val jcode_pp : ('a program -> int -> int) option
end
  
module Make (S : PrintInterface) =
struct
  type code = S.code
      
  let revert_callgraph program f =
    ClassMethodMap.fold
      (fun _ (c,cm) cmmap ->
	 match cm.cm_implementation with
	   | Native -> cmmap
	   | Java code ->
	       let cn = get_name c in
	       let ms = cm.cm_signature in
	       let rmmap = ref cmmap in
		 S.iter_code
		   (fun pp _ ->
		      let jcode_pp = f program pp in
			try
			  let cmset =
			    program.static_lookup_method cn ms jcode_pp in
			    ClassMethodSet.iter
			      (fun ccms ->
				 let rcmset =
				   try ClassMethodMap.find ccms !rmmap
				   with Not_found -> ClassMethSet.empty in
				   rmmap := ClassMethodMap.add ccms
				     (ClassMethSet.add (cn,ms) rcmset) !rmmap
			      ) cmset
			with Not_found -> ()
		   ) code;
		 !rmmap
      ) program.parsed_methods ClassMethodMap.empty
      
  let get_callers rcg cs ms =
    let cmsig = make_cms cs ms in
      try Some (ClassMethodMap.find cmsig rcg)
      with _ -> None
      
  let get_internal_info program info = {
    p_data = info;
    p_callers = match S.jcode_pp with
      | Some f -> get_callers (revert_callgraph program f)
      | None -> fun _ _ -> None
  }

  let methodparameters2html program cs ms =
    let mparameters = ms_args ms in
    let pnames = S.method_param_names program cs ms in
    let prms =
      list_concat
	(ExtList.List.mapi
	   (fun i x ->
	      let v = (valuetype2html program x cs) in
		match pnames with
		  | None -> v @ [PCData (" " ^ (string_of_int i))]
		  | Some names ->
		      v @ [PCData (" " ^ (List.nth names i))]
	   ) mparameters
	) in
      [PCData "("] @ prms @ [PCData ")"]

  let methodsignature2html program cs ms info =
    let meth = get_method (get_node program cs) ms in
    let mname = ms_name ms in
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
    let mname2html = methodname2html cs ms info in
    let mparams2html = methodparameters2html program cs ms in
      if (mname = "<clinit>") then
	let mname = mname2html "clinit" in
	  (PCData (header ^ " ")) :: mname @ [PCData ("{};")]
      else if (mname = "<init>") then
	let mname = mname2html (cn_simple_name cs) in
	  (PCData (header ^ " ")) :: mname
	  @ mparams2html
	  @ [PCData ";"]
      else
	let mname = mname2html mname in
	  (PCData (header ^ " "))
	  :: (returntype2html program mreturntype cs)
	  @ (PCData (" ") :: mname)
	  @ mparams2html
	  @ [PCData ";"]
	  
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
    let method_annots = info.p_data.p_method cs ms in
    let method_signature = methodsignature2html program cs ms info in
    let callers = methodcallers2html cs ms info in
      gen_method (ms2anchorname cs ms) method_signature callers
	method_annots insts
      
  let ioc2html program cs info =
    let ioc = get_node program cs in
    let fields =
      List.fold_left
	(fun l fs ->
	   (field2html program cs fs (info.p_data.p_field cs fs)) :: l)
	[] (FieldMap.key_elements (get_fields (get_node program cs))) in
    let methods =
      MethodMap.fold
	(fun _ m l ->
	   let ms = get_method_signature m in
	   let insts =
	     match m with
	       | AbstractMethod _ -> Ptmap.empty
	       | ConcreteMethod cm ->
		   (match cm.cm_implementation with
		      | Native -> Ptmap.empty
		      | Java code ->
			  let map = ref Ptmap.empty in
			    S.iter_code
			      (fun pp inst ->
				 let l =
				   try Ptmap.find pp !map
				   with _ -> [] in
				   map := Ptmap.add pp (inst :: l) !map
			      ) code;
			    Ptmap.mapi
			      (fun pp insts ->
				 let annots = (info.p_data.p_pp cs ms pp) in
				 let insts =
				   List.rev_map
				     (fun inst ->
					S.inst_html program cs ms pp inst) insts in
				   (insts, annots)
			      ) !map
		   )
	   in
	     (method2html program cs ms info insts) :: l)
	(get_methods ioc) [] in
    let content = List.rev_append fields methods in
      gen_class (cn2anchorname cs)
	(iocsignature2html program cs) (info.p_data.p_class cs) content
	
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
		 Filename.concat dirname basename
	      ) (Filename.concat outputdir hd) tl in
	    create_dir dirname
	      
  (* WARNING: do not edit the next line, the CSS comment will be
     replaced by the actual CSS. (cf. Makefile) *)
  let default_css = "(* CSS *)"
    
  let default_js = "function showInfoList(e){
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
    
  let print_program
      ?css ?js ?(info=void_info) program outputdir =
    let copy_file default src dst =
      let outchan = open_out dst in
	(match src with
	   | Some f ->
	       let inchan = open_in f in
		 (try
		    while (true) do
		      let line = input_line inchan in
			output_string outchan line
		    done
		  with End_of_file -> close_in inchan)
	   | None ->
	       output_string outchan default
	);
	close_out outchan
    and stylefile = "style.css"
    and jsfile = "actions.js"
    in
      if (Sys.is_directory outputdir) then
	(copy_file default_css css (Filename.concat outputdir stylefile);
	 copy_file default_js js (Filename.concat outputdir jsfile)
	)
      else invalid_arg "Last argument must be an existing directory";
      let info = get_internal_info program info in
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
	       	 open_out (Filename.concat outputdir
	       		     (List.fold_left
	       			Filename.concat "" (package @ [cname ^ ".html"])))
	       in
	       	 output_string out doctype;
	       	 print_html_tree doc out;
	       	 close_out out
	  ) program.classes;
end

module JCodePrinter = Make(
  struct
    type instr = JCode.jopcode
    type code = JCode.jcode

    let iter_code f lazy_code =
      let code = Lazy.force lazy_code in
	Array.iteri
	  (fun pp opcode ->
	     match opcode with
	       | OpInvalid -> ()
	       | _ -> f pp opcode
	  ) code.c_code

    let method_param_names program cn ms =
      let m = get_method (get_node program cn) ms in
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
      			    | None -> string_of_int i
      			    | Some (name,_) -> name
      		     ) (ms_args ms)
      		  )

      let inst_html program cs ms pp op =
	let inst_params = Javalib.JPrint.jopcode ~jvm:true op in
	let inst =
      	  try
      	    let n = (String.index inst_params ' ') + 1 in
      	      String.sub inst_params 0 n
      	  with Not_found -> inst_params in
	  match op with
	    | OpNew ccs ->
		let v = TObject (TClass ccs) in
	  	  [simple_elem inst; value_elem program cs v]
	    | OpNewArray v ->
		[simple_elem inst; value_elem program cs v]
	    | OpAMultiNewArray (o,i) ->
		let v = TObject o in
	  	  [simple_elem inst; value_elem ~dim:i program cs v]
	    | OpCheckCast o | OpInstanceOf o ->
		let v = TObject o in
	  	  [simple_elem inst; value_elem program cs v]
	    | OpGetStatic (ccs,fs) | OpPutStatic (ccs,fs)
	    | OpGetField (ccs,fs) | OpPutField (ccs,fs) ->
		let ftype = fs_type fs in
	  	  [simple_elem inst; field_elem program cs ccs fs;
	  	   simple_elem " : "; value_elem program cs ftype]
	    | OpInvoke ((`Virtual o),cms) ->
		let ccs = match o with
	  	  | TClass ccs -> ccs
	  	  | _ -> JBasics.java_lang_object in
	  	  [simple_elem inst;
	  	   invoke_elem program cs ms pp ccs cms;
	  	   method_args_elem program cs ms]
	    | OpInvoke ((`Interface ccs),cms) ->
		[simple_elem inst;
		 invoke_elem program cs ms pp ccs cms;
		 method_args_elem program cs ms]
	    | OpInvoke ((`Static ccs),cms) ->
		[simple_elem inst;
		 invoke_elem program cs ms pp ccs cms;
		 method_args_elem program cs ms]
	    | OpInvoke ((`Special ccs),cms) ->
		[simple_elem inst;
		 invoke_elem program cs ms pp ccs cms;
		 method_args_elem program cs ms]
	    | OpLoad (_,n) | OpStore (_,n) | OpRet n ->
		let m = get_method (get_node program cs) ms in
		let locname =
	  	  match m with
	  	    | AbstractMethod _
	  	    | ConcreteMethod {cm_implementation = Native} -> string_of_int n
	  	    | ConcreteMethod {cm_implementation = Java code} ->
	  		let v = get_local_variable_info n pp (Lazy.force code) in
	  		  match v with
	  		    | None -> string_of_int n
	  		    | Some (name,_) -> name in
	  	  [simple_elem (inst ^ " " ^ locname)]
	    | _ -> [simple_elem inst_params]
		
    let jcode_pp = Some (fun _ x -> x)
  end)

let print_program = JCodePrinter.print_program
  
module JBirPrinter = Make(
  struct
    type instr = JBir.instr
    type code = JBir.t

    let iter_code f lazy_code =
      try
	let code = Lazy.force lazy_code in
	  List.iter
	    (fun (pp,l) ->
	       List.iter (fun inst -> f pp inst) l
	    ) code.JBir.code
      with
	  _ -> ()
    let print_list_sep sep f l =
      let ml = List.map f l in
	String.concat sep ml

    let method_param_names program cn ms =
      let m = get_method (get_node program cn) ms in
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

    let inst_html program cs ms pp op =
      match op with
	| JBir.AffectStaticField (ccs,fs,e) ->
	    let p1 = field_elem program cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (JBir.print_expr e)) in
	      [p1;p2]
	| JBir.AffectField (e1,ccs,fs,e2) ->
	    let p1 = field_elem program ~called_cname:(JBir.print_expr e1)
	      cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (JBir.print_expr e2)) in
	      [p1;p2]
	| JBir.New (x,ccs,_,le) ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (JBir.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " JBir.print_expr le)) in
	      [p1;p2;p3]
	| JBir.NewArray (x,v,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (JBir.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "%s"
		 (print_list_sep ""
		    (fun e -> 
		       Printf.sprintf "[%s]" (JBir.print_expr e)) le)
	      ) in
	      [p1;p2;p3]
	| JBir.InvokeStatic (None,ccs,cms,le) ->
	    let p1 = invoke_elem program cs ms pp ccs cms in
	    let p2 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (JBir.print_expr) le)) in
	      [p1;p2]
	| JBir.InvokeStatic (Some x,ccs,cms,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s :=" (JBir.var_name_g x)) in
	    let p2 = invoke_elem program cs ms pp ccs cms in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (JBir.print_expr) le)) in
	      [p1;p2;p3]
	| JBir.InvokeVirtual (r,e1,k,cms,le) ->
	    let p2 =
	      (match k with
		 | JBir.VirtualCall o ->
		     let ccs = match o with
		       | TClass ccs -> ccs
		       | _ -> JBasics.java_lang_object in
		       invoke_elem ~called_cname:(JBir.print_expr e1) program
			 cs ms pp ccs cms
		 | JBir.InterfaceCall ccs ->
		     invoke_elem ~called_cname:(JBir.print_expr e1) program
		       cs ms pp ccs cms
	      ) in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " (JBir.print_expr) le)) in
	      (match r with
		 | None -> [p2;p3]
		 | Some x ->
		     let p1 = simple_elem
		       (Printf.sprintf "%s :="  (JBir.var_name_g x)) in
		       [p1;p2;p3]
	      )
	| JBir.InvokeNonVirtual (r,e1,ccs,cms,le) ->
	    let p1 = simple_elem
	      (match r with
		 | None -> (JBir.print_expr e1) ^ "."
		 | Some x -> Printf.sprintf "%s := %s." (JBir.var_name_g x)
		     (JBir.print_expr e1)
	      ) in
	    let p2 = invoke_elem program cs ms pp ccs cms in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " JBir.print_expr le)) in
	      [p1;p2;p3]
	| JBir.MayInit ccs ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem "mayinit" in
	    let p2 = value_elem program cs v in
	      [p1;p2]
	| JBir.Check (JBir.CheckCast (e,t)) ->
	    let p1 = simple_elem
	      (Printf.sprintf "checkcast %s:" (JBir.print_expr e)) in
	    let p2 = value_elem program cs (TObject t) in
	      [p1;p2]
	| _ -> [simple_elem (JBir.print_instr op)]

    let jcode_pp = Some (fun _ x -> x)
  end)

let print_jbir_program = JBirPrinter.print_program

module A3BirPrinter = Make(
  struct
    type instr = A3Bir.instr
    type code = A3Bir.t

    let iter_code f lazy_code =
      try
	let code = Lazy.force lazy_code in
	  List.iter
	    (fun (pp,l) ->
	       List.iter (fun inst -> f pp inst) l
	    ) code.A3Bir.a3_code
      with
	  _ -> ()
    let print_list_sep sep f l =
      let ml = List.map f l in
	String.concat sep ml

    let method_param_names program cn ms =
      let m = get_method (get_node program cn) ms in
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
			  let var = snd (List.nth code.A3Bir.a3_params n) in
			    A3Bir.var_name_g var
		       ) (ms_args ms)
		    )
	      with _ -> None

    let inst_html program cs ms pp op =
      match op with
	| A3Bir.AffectStaticField (ccs,fs,e) ->
	    let p1 = field_elem program cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (A3Bir.print_expr e)) in
	      [p1;p2]
	| A3Bir.AffectField (e1,ccs,fs,e2) ->
	    let p1 = field_elem program ~called_cname:(A3Bir.print_basic_expr e1)
	      cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (A3Bir.print_basic_expr e2)) in
	      [p1;p2]
	| A3Bir.New (x,ccs,_,le) ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (A3Bir.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " A3Bir.print_basic_expr le)) in
	      [p1;p2;p3]
	| A3Bir.NewArray (x,v,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (A3Bir.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "%s"
		 (print_list_sep ""
		    (fun e -> 
		       Printf.sprintf "[%s]" (A3Bir.print_basic_expr e)) le)
	      ) in
	      [p1;p2;p3]
	| A3Bir.InvokeStatic (None,ccs,cms,le) ->
	    let p1 = invoke_elem program cs ms pp ccs cms in
	    let p2 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (A3Bir.print_basic_expr) le)) in
	      [p1;p2]
	| A3Bir.InvokeStatic (Some x,ccs,cms,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s :=" (A3Bir.var_name_g x)) in
	    let p2 = invoke_elem program cs ms pp ccs cms in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (A3Bir.print_basic_expr) le)) in
	      [p1;p2;p3]
	| A3Bir.InvokeVirtual (r,e1,k,cms,le) ->
	    let p2 =
	      (match k with
		 | A3Bir.VirtualCall o ->
		     let ccs = match o with
		       | TClass ccs -> ccs
		       | _ -> JBasics.java_lang_object in
		       invoke_elem ~called_cname:(A3Bir.print_basic_expr e1) program
			 cs ms pp ccs cms
		 | A3Bir.InterfaceCall ccs ->
		     invoke_elem ~called_cname:(A3Bir.print_basic_expr e1) program
		       cs ms pp ccs cms
	      ) in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " (A3Bir.print_basic_expr) le)) in
	      (match r with
		 | None -> [p2;p3]
		 | Some x ->
		     let p1 = simple_elem
		       (Printf.sprintf "%s :="  (A3Bir.var_name_g x)) in
		       [p1;p2;p3]
	      )
	| A3Bir.InvokeNonVirtual (r,e1,ccs,cms,le) ->
	    let p1 = simple_elem
	      (match r with
		 | None -> (A3Bir.print_basic_expr e1) ^ "."
		 | Some x -> Printf.sprintf "%s := %s." (A3Bir.var_name_g x)
		     (A3Bir.print_basic_expr e1)
	      ) in
	    let p2 = invoke_elem program cs ms pp ccs cms in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " A3Bir.print_basic_expr le)) in
	      [p1;p2;p3]
	| A3Bir.MayInit ccs ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem "mayinit" in
	    let p2 = value_elem program cs v in
	      [p1;p2]
	| A3Bir.Check (A3Bir.CheckCast (e,t)) ->
	    let p1 = simple_elem
	      (Printf.sprintf "checkcast %s:" (A3Bir.print_basic_expr e)) in
	    let p2 = value_elem program cs (TObject t) in
	      [p1;p2]
	| _ -> [simple_elem (A3Bir.print_instr op)]

    let jcode_pp = Some (fun _ x -> x)
  end)

let print_a3bir_program = A3BirPrinter.print_program
