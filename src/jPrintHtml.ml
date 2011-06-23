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

open Javalib_pack
open JBasics
open JCode
open Javalib
open JProgram
open JPrintUtil

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
let methodname_class = "methodname clickable"
let field_class = "field"
let field_signature_class = "field_signature"
let method_class = "method"
let method_signature_class = "method_signature"
let code_class = "code"
let annot_class = "annot"
let instruction_class = "instruction"
let parameter_class = "parameter clickable"
let clickable_class = "clickable"
  
   
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
      | [] -> []
      | _ ->
	  [gen_custom_tag "ul" [("class",annot_class)] ullist]
	    
let gen_hidden_list hlist =
  let ullist =
    List.map
      (fun elt ->
	 gen_custom_tag "li" [] [gen_div elt []]
      ) hlist in
    gen_custom_tag "ul" [("class",clickable_class)] ullist
      
type elem = | SimpleExpr of xml_tree list
	    | DynamicExpr of (xml_tree list) * ((xml_tree list) list)
		
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
		    [("class",parameter_class);
		     ("onclick","showInfoList(this)")]) in
	       let he = gen_hidden_list l in
		 (ve :: vl, he :: hl)
      ) inst_params ([],[]) in
    gen_div (visible_parameters @ hidden_parameters)
      [("class",instruction_class)]
      
let gen_code insts =
  let ollist =
    List.rev_map
      (fun (pp,(insts,insts_annots)) ->
	 let insts_html =
	   List.fold_right
	     (fun inst_params l ->
		(gen_inst inst_params) :: l
	     ) insts (gen_annots insts_annots) in
	   match insts_html with
	     | [] ->
		 (gen_custom_tag "li" [("value",string_of_int pp)]
		    [PCData "<br/>"])
	     | _ ->
		 (gen_custom_tag "li" [("value",string_of_int pp)] insts_html)
      ) insts in
    match ollist with
      | [] -> PCData ""
      | _ -> gen_custom_tag "ol" [("class",code_class)] ollist
	  
let add_anchor anchor_name anchor_info htmllist =
  if (anchor_name <> "") then
    (gen_anchor anchor_name anchor_info) :: htmllist
  else
    htmllist
      
let gen_method anchor_name method_signature callers method_annots insts =
  let meth_sig = gen_div (method_signature @ (gen_annots method_annots)
			  @ callers) [("class", method_signature_class)] in
  let meth_body = [meth_sig; gen_code insts] in
    gen_div (add_anchor anchor_name "" meth_body)
      [("class", method_class)]
      
let gen_field anchor_name field_signature annots =
  let field_sig = gen_span field_signature
    [("class", field_signature_class)] in
  let field_body = field_sig :: (gen_annots annots) in
    gen_div (add_anchor anchor_name "" field_body) [("class",field_class)]
      
let gen_class anchor_name classname annots content =
  let class_name = gen_div classname [("class",classname_class)] in
  let class_body = (class_name :: (gen_annots annots) @ content) in
    gen_div (add_anchor anchor_name "" class_body)
      [("class", class_class)]
      

type info_internal = 
    {
      p_data : info;
      (** Prints information about the possible method callers. *)
      p_callers : class_name -> method_signature -> ClassMethodSet.t option;
    }
      
let get_relative_path frompath topath =
  let nbsep = ref 0 in
    String.iter (fun c -> if c = '/' then nbsep := !nbsep + 1) frompath;
    let s = ref "" in
      for i = 1 to !nbsep do
	s := !s ^ "../"
      done;
      !s ^ topath

let get_relative_file ?(strict=false) fromclass toclass =
  if (cn_equal fromclass toclass) then
    if strict then (cn_simple_name toclass) ^ ".html"
    else ""
  else
    let frompath = ExtString.String.map
      (fun c -> if c = '.' then '/' else c) (cn_name fromclass) in
    let topath = ExtString.String.map
      (fun c -> if c = '.' then '/' else c) (cn_name toclass) in
    let path = get_relative_path frompath topath in
      path ^ ".html"
      
  let rec valuetype2html program t currentclass =
  match t with
    | TBasic _ -> [PCData (JPrint.value_type t)]
    | TObject o ->
	(match o with
	   | TClass cs ->
	       begin
		 match program with
		     Some program when (ClassMap.mem cs program.classes) ->
		       [gen_hyperlink (get_relative_file ~strict:true currentclass cs)
			  (cn_name cs)]
		   | _ ->  [PCData (cn_name cs)]
	       end
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
  let msname = ms_name ms in
  let mparameters = ms_args ms in
  let mreturntype = ms_rtype ms in
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
	
let fieldsignature2html program ioc fs =
  let iocfield = Javalib.get_field ioc fs in
  let cs = Javalib.get_name ioc in
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

let methodcallers2html cs ms info =
  match info.p_callers cs ms with
    | None -> []
    | Some callers ->
	let hl = ClassMethodSet.fold
	  (fun clms l ->
	     let (ccs,cms) = cms_split clms in
	     let anchor = ms2anchorname ccs cms in
	     let href = (get_relative_file cs ccs) ^ "#" ^ anchor in
	     let ccname = cn_name ccs in
	     let cmname = ms_name cms in
	     let vtl = JPrint.value_type_list (ms_args cms) in
	     let cmsig = JPrint.method_signature cms in
	       [gen_titled_hyperlink href 
		  (ccname ^ "." ^ cmname ^vtl) cmsig] :: l
	  ) callers [] in
	  [gen_hidden_list hl]
	    
let methodname2html cs ms info mname =
  match info.p_callers cs ms with
    | None ->
	[gen_span [PCData mname] []]
    | Some _ ->
	[gen_span [PCData mname]
	   [("class",methodname_class);
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
      let program = 
	match program with
	    Some prog -> prog 
	  | _ -> raise Not_found
      in
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
  let mlookupshtml =
    try 
      let program = 
	match program with
	    Some prog -> prog
	  | _ -> raise Not_found
      in
	ClassMethodSet.fold
	  (fun cms l ->
	     let (rcs,rms) = cms_split cms in
	     let anchor = ms2anchorname rcs rms in
	     let href = (get_relative_file cs rcs) ^ "#" ^ anchor in
	     let rcname = cn_name rcs in
	     let rmname = ms_name rms in
	     let rmsig = JPrint.method_signature rms in
	       [gen_titled_hyperlink href (rcname ^ "." ^ rmname) rmsig] :: l
	  )
	  (program.static_lookup_method cs ms pp) []
    with _ -> [] in
  let callcname = match called_cname with
    | Some x -> x
    | None -> cn_name callcs in
  let callmname = ms_name callms in
  let callmsig = JPrint.method_signature callms in
    DynamicExpr ([gen_titled_span
		    [PCData (callcname ^ "." ^ callmname)] [] callmsig],
		 match mlookupshtml with
		   | [] -> [[PCData "No reachable result."]]
		   | _ -> mlookupshtml)

(** [method_args_elem p cs ms] [cs] must be current class_name from where call is done and [ms] the method_signature of the called method.*)      
let method_args_elem program cs ms =
  let mparameters = ms_args ms in
  let prms =
    list_concat
      (List.map
	 (fun x -> (valuetype2html program x cs)) mparameters
      ) in
    html_elem ([PCData "("] @ prms @ [PCData ")"])

(** [method_ret_elem p cs ms] [cs] must be current class_name from where call is done and [ms] the method_signature of the called method.*)      
let method_ret_elem program cs ms =
  let ms_ret = ms_rtype ms in
  let prms = 
    returntype2html program ms_ret cs
  in
    html_elem prms
      
module type HTMLPrinter =
sig
  type code
    
  val print_program :
    ?css:string -> ?js:string -> ?info:info -> code program -> string -> unit

  val print_class :
    ?css:string -> ?js:string -> ?info:info -> code Javalib.interface_or_class -> string -> unit
end
  
module type PrintInterface =
sig
  type instr
  type code
  val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit
  val method_param_names : code Javalib.interface_or_class -> method_signature
    -> string list option
  val inst_html : code program option -> code Javalib.interface_or_class -> method_signature -> int
    -> instr -> elem list
end
  
module Make (S : PrintInterface) =
struct
  type code = S.code
      
  let revert_callgraph program =
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
			try
			  let cmset =
			    program.static_lookup_method cn ms pp in
			    ClassMethodSet.iter
			      (fun ccms ->
				 let rcmset =
				   try ClassMethodMap.find ccms !rmmap
				   with Not_found -> ClassMethodSet.empty in
				   rmmap := ClassMethodMap.add ccms
				     (ClassMethodSet.add (make_cms cn ms) rcmset) !rmmap
			      ) cmset
			with Not_found -> ()
		   ) code;
		 !rmmap
      ) program.parsed_methods ClassMethodMap.empty
      
  let get_callers rcg cs ms =
    let cmsig = make_cms cs ms in
      try Some (ClassMethodMap.find cmsig rcg)
      with _ -> None
	
  let get_internal_info program info = 
    match program with
	None -> 
	  {
	    p_data = info;
	    p_callers =  fun _ _ -> None
	  }
      | Some program -> 
	  {
	    p_data = info;
	    p_callers =  get_callers (revert_callgraph program )
	  }

    
  let methodparameters2html program ioc cs ms =
    let mparameters = ms_args ms in
    let pnames = S.method_param_names ioc ms in
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
	
  let methodsignature2html program ioc cs ms info =
    let meth = Javalib.get_method ioc ms in
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
    let mparams2html = methodparameters2html program ioc cs ms in
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
	    
  let iocsignature2html ioc cs =
      match ioc with
	| JClass _ ->
	    [PCData ("Class " ^ (cn_name cs))]
	| JInterface _ ->
	    [PCData ("Interface " ^ (cn_name cs))]
	      
  let field2html program ioc fs annots =
    gen_field (fs2anchorname (Javalib.get_name ioc) fs) (fieldsignature2html program ioc fs) annots
      
  let method2html program ioc cs ms info insts =
    let method_annots = info.p_data.p_method cs ms in
    let method_signature = methodsignature2html program ioc cs ms info in
    let callers = methodcallers2html cs ms info in
      gen_method (ms2anchorname cs ms) method_signature callers
	method_annots insts
	
  let ioc2html program ioc info =
    let cs = Javalib.get_name ioc in
    let fields =
      FieldMap.fold
	(fun fs _ l ->
	   (field2html program ioc fs (info.p_data.p_field cs fs)) :: l)
	(Javalib.get_fields ioc) [] in
    let methods =
      MethodMap.fold
	(fun _ m methods ->
	   let ms = get_method_signature m in
	   let insts =
	     match m with
	       | AbstractMethod _ -> []
	       | ConcreteMethod cm ->
		   (match cm.cm_implementation with
		      | Native -> []
		      | Java code ->
			  let l = ref [] in
			    S.iter_code
			      (fun pp insts ->
				 let insts_html =
				   List.map
				     (fun inst ->
					S.inst_html program ioc ms pp inst
				     ) insts in
				 let insts_annots = 
				   info.p_data.p_pp cs ms pp 
				 in
				   l := (pp, (insts_html, insts_annots)) :: !l
			      ) code;
			    !l
		   )
	   in
	     (method2html program ioc cs ms info insts) :: methods)
	(Javalib.get_methods ioc) [] in
    let content = List.rev_append fields methods in
      gen_class (cn2anchorname cs)
	(iocsignature2html ioc cs) (info.p_data.p_class cs) content
	
  let gen_class_document program ioc info css js =
    let cname = cn_name (Javalib.get_name ioc) in
      gen_html_document
	[(gen_html_head
	    [(gen_html_title cname);
	     (gen_css_link css);
	     (gen_javascript_src js)]);
	 (gen_html_body
	    [ioc2html program ioc info]
	 )]
	
 
  (* WARNING: do not edit the next line, the CSS comment will be
     replaced by the actual CSS. (cf. Makefile) *)
  let default_css = "(* CSS *)"
    
  let default_js = "function showInfoList(e){\n"^
    "    var siblings = e.parentNode.childNodes;\n"^
    "    var len = siblings.length;\n"^
    "\n"^
    "    for(var i = 0; i < len; i++){\n"^
    "        var sibling = siblings[i];\n"^
    "    	if (sibling.nodeName == \"UL\"\n"^
    "	    && sibling.className == \"clickable\"){\n"^
    "	    var style = sibling.style;\n"^
    "	    if (style.visibility != \"visible\"){\n"^
    "		style.position = \"static\";\n"^
    "                style.visibility = \"visible\";\n"^
    "	    } else{\n"^
    "		style.visibility = \"hidden\";\n"^
    "                style.position = \"absolute\";\n"^
    "	    }\n"^
    "	}\n"^
    "    }\n"^
    "}"
    
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
    and doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
    in
      if (Sys.is_directory outputdir) then
	(copy_file default_css css (Filename.concat outputdir stylefile);
	 copy_file default_js js (Filename.concat outputdir jsfile)
	)
      else invalid_arg "Last argument must be an existing directory";
      let info = get_internal_info (Some program) info in
	ClassMap.iter
	  (fun cs node ->
	     let ioc = to_ioc node in
	     let package = cn_package cs
	     and cname = cn_simple_name cs in
	     let cpath = ExtString.String.map
	       (fun c -> if c = '.' then '/' else c) (cn_name cs) in
	     let relative_css = (get_relative_path cpath "") ^ stylefile
	     and relative_js = (get_relative_path cpath "") ^ jsfile in
	     let doc = gen_class_document (Some program) ioc info
	       relative_css relative_js in
	       create_package_dir outputdir package;
	       let out =
		 open_out (Filename.concat outputdir
	       		     (List.fold_left
	       			Filename.concat "" (package @ [cname ^ ".html"])))
	       in
		 output_string out doctype;
		 print_xml_tree doc out;
		 close_out out
	  ) program.classes

  let print_class
      ?css ?js ?(info=void_info) ioc outputdir =
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
    and doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
    in
      if (Sys.is_directory outputdir) then
	(copy_file default_css css (Filename.concat outputdir stylefile);
	 copy_file default_js js (Filename.concat outputdir jsfile)
	)
      else invalid_arg "Last argument must be an existing directory";
      let info = get_internal_info None info in
      let cs = Javalib.get_name ioc in
      let package = cn_package cs
      and cname = cn_simple_name cs in
      let cpath = ExtString.String.map
	(fun c -> if c = '.' then '/' else c) (cn_name cs) in
      let relative_css = (get_relative_path cpath "") ^ stylefile
      and relative_js = (get_relative_path cpath "") ^ jsfile in
      let doc = gen_class_document None ioc info
	relative_css relative_js in
	create_package_dir outputdir package;
	let out =
	  open_out (Filename.concat outputdir
	       	      (List.fold_left
	       		 Filename.concat "" (package @ [cname ^ ".html"])))
	in
	  output_string out doctype;
	  print_xml_tree doc out;
	  close_out out

end
  
module JCodePrinter = Make(
  struct
    type instr = JCode.jopcode
    type code = JCode.jcode

    include JCodeUtil
		  
    let inst_html program ioc ms pp op =
      let cs = Javalib.get_name ioc in
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
		 method_ret_elem program cs cms;
	  	 invoke_elem program cs ms pp ccs cms;
	  	 method_args_elem program cs cms]
	  | OpInvoke ((`Interface ccs),cms) ->
	      [simple_elem inst;
	       method_ret_elem program cs cms;
	       invoke_elem program cs ms pp ccs cms;
	       method_args_elem program cs cms]
	  | OpInvoke ((`Static ccs),cms) ->
	      [simple_elem inst;
	       method_ret_elem program cs cms;
	       invoke_elem program cs ms pp ccs cms;
	       method_args_elem program cs cms]
	  | OpInvoke ((`Special ccs),cms) ->
	      [simple_elem inst;
	       method_ret_elem program cs cms;
	       invoke_elem program cs ms pp ccs cms;
	       method_args_elem program cs cms]
	  | OpLoad (_,n) | OpStore (_,n) | OpRet n ->
	      let m = Javalib.get_method ioc ms in
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
  end)


let print_list_sep sep f l =
  let ml = List.map f l in
    String.concat sep ml
	  

  
module MakeJBirLikePrintInterface (BirLike: JBir.Internal.CodeInstrSig) = 
struct
  type instr = BirLike.instr
  type code = BirLike.t

  include IRUtil(BirLike)
  
  let inst_html program ioc ms pp op =
    let cs = Javalib.get_name ioc in
      match op with
	| BirLike.AffectStaticField (ccs,fs,e) ->
	    let p1 = field_elem program cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (BirLike.print_expr e)) in
	      [p1;p2]
	| BirLike.AffectField (e1,ccs,fs,e2) ->
	    let p1 = field_elem program ~called_cname:(BirLike.print_expr e1)
	      cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (BirLike.print_expr e2)) in
	      [p1;p2]
	| BirLike.New (x,ccs,_,le) ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (BirLike.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " BirLike.print_expr le)) in
	      [p1;p2;p3]
	| BirLike.NewArray (x,v,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (BirLike.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "%s"
		 (print_list_sep ""
		    (fun e -> 
		       Printf.sprintf "[%s]" (BirLike.print_expr e)) le)
	      ) in
	      [p1;p2;p3]
	| BirLike.InvokeStatic (None,ccs,cms,le) ->
	    let p1 = 
	      invoke_elem 
		program cs ms pp ccs cms 
	    in
	    let p2 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (BirLike.print_expr) le)) in
	      [p1;p2]
	| BirLike.InvokeStatic (Some x,ccs,cms,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s :=" (BirLike.var_name_g x)) in
	    let p2 = 
	      invoke_elem 
		program cs ms pp ccs cms 
	    in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (BirLike.print_expr) le)) in
	      [p1;p2;p3]
	| BirLike.InvokeVirtual (r,e1,k,cms,le) ->
	    let p2 =
	      (match k with
		 | JBir.VirtualCall o ->
		     let ccs = match o with
		       | TClass ccs -> ccs
		       | _ -> JBasics.java_lang_object in
		       invoke_elem ~called_cname:(BirLike.print_expr e1) program
			 cs ms pp ccs cms
		 | JBir.InterfaceCall ccs ->
		     invoke_elem ~called_cname:(BirLike.print_expr e1) program
		       cs ms pp ccs cms
	      ) in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " (BirLike.print_expr) le)) in
	      (match r with
		 | None -> [p2;p3]
		 | Some x ->
		     let p1 = simple_elem
		       (Printf.sprintf "%s :="  (BirLike.var_name_g x)) in
		       [p1;p2;p3]
	      )
	| BirLike.InvokeNonVirtual (r,e1,ccs,cms,le) ->
	    let p1 = simple_elem
	      (match r with
		 | None -> (BirLike.print_expr e1) ^ "."
		 | Some x -> Printf.sprintf "%s := %s." (BirLike.var_name_g x)
		     (BirLike.print_expr e1)
	      ) in
	    let p2 = 
	      invoke_elem 
		program cs ms pp ccs cms 
	    in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " BirLike.print_expr le)) in
	      [p1;p2;p3]
	| BirLike.MayInit ccs ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem "mayinit" in
	    let p2 = value_elem program cs v in
	      [p1;p2]
	| BirLike.Check (BirLike.CheckCast (e,t)) ->
	    let p1 = simple_elem
	      (Printf.sprintf "checkcast %s:" (BirLike.print_expr e)) in
	    let p2 = value_elem program cs (TObject t) in
	      [p1;p2]
	| _ -> [simple_elem (BirLike.print_instr op)]
end

module MakeA3BirLikePrintInterface (A3BirLike:A3Bir.Internal.CodeInstrSig) =
struct
  type instr = A3BirLike.instr
  type code = A3BirLike.t
      
  include IRUtil(A3BirLike)
  
  let inst_html program ioc ms pp op =
    let cs = Javalib.get_name ioc in
      match op with
	| A3BirLike.AffectStaticField (ccs,fs,e) ->
	    let p1 = field_elem program cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (A3BirLike.print_expr e)) in
	      [p1;p2]
	| A3BirLike.AffectField (e1,ccs,fs,e2) ->
	    let p1 = field_elem program ~called_cname:(A3BirLike.print_basic_expr e1)
	      cs ccs fs in
	    let p2 = simple_elem
	      (Printf.sprintf ":= %s" (A3BirLike.print_basic_expr e2)) in
	      [p1;p2]
	| A3BirLike.New (x,ccs,_,le) ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (A3BirLike.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " A3BirLike.print_basic_expr le)) in
	      [p1;p2;p3]
	| A3BirLike.NewArray (x,v,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s := new" (A3BirLike.var_name_g x)) in
	    let p2 = value_elem program cs v in
	    let p3 = simple_elem
	      (Printf.sprintf "%s"
		 (print_list_sep ""
		    (fun e -> 
		       Printf.sprintf "[%s]" (A3BirLike.print_basic_expr e)) le)
	      ) in
	      [p1;p2;p3]
	| A3BirLike.InvokeStatic (None,ccs,cms,le) ->
	    let p1 = invoke_elem program cs ms pp ccs cms in
	    let p2 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (A3BirLike.print_basic_expr) le)) in
	      [p1;p2]
	| A3BirLike.InvokeStatic (Some x,ccs,cms,le) ->
	    let p1 = simple_elem
	      (Printf.sprintf "%s :=" (A3BirLike.var_name_g x)) in
	    let p2 = invoke_elem program cs ms pp ccs cms in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " (A3BirLike.print_basic_expr) le)) in
	      [p1;p2;p3]
	| A3BirLike.InvokeVirtual (r,e1,k,cms,le) ->
	    let p2 =
	      (match k with
		 | A3Bir.VirtualCall o ->
		     let ccs = match o with
		       | TClass ccs -> ccs
		       | _ -> JBasics.java_lang_object in
		       invoke_elem ~called_cname:(A3BirLike.print_basic_expr e1) program
			 cs ms pp ccs cms
		 | A3Bir.InterfaceCall ccs ->
		     invoke_elem ~called_cname:(A3BirLike.print_basic_expr e1) program
		       cs ms pp ccs cms
	      ) in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)"
		 (print_list_sep ", " (A3BirLike.print_basic_expr) le)) in
	      (match r with
		 | None -> [p2;p3]
		 | Some x ->
		     let p1 = simple_elem
		       (Printf.sprintf "%s :="  (A3BirLike.var_name_g x)) in
		       [p1;p2;p3]
	      )
	| A3BirLike.InvokeNonVirtual (r,e1,ccs,cms,le) ->
	    let p1 = simple_elem
	      (match r with
		 | None -> (A3BirLike.print_basic_expr e1) ^ "."
		 | Some x -> Printf.sprintf "%s := %s." (A3BirLike.var_name_g x)
		     (A3BirLike.print_basic_expr e1)
	      ) in
	    let p2 = invoke_elem program cs ms pp ccs cms in
	    let p3 = simple_elem
	      (Printf.sprintf "(%s)" (print_list_sep ", " A3BirLike.print_basic_expr le)) in
	      [p1;p2;p3]
	| A3BirLike.MayInit ccs ->
	    let v = TObject (TClass ccs) in
	    let p1 = simple_elem "mayinit" in
	    let p2 = value_elem program cs v in
	      [p1;p2]
	| A3BirLike.Check (A3BirLike.CheckCast (e,t)) ->
	    let p1 = simple_elem
	      (Printf.sprintf "checkcast %s:" (A3BirLike.print_basic_expr e)) in
	    let p2 = value_elem program cs (TObject t) in
	      [p1;p2]
	| _ -> [simple_elem (A3BirLike.print_instr op)]
end

module JBirPrinter = Make(MakeJBirLikePrintInterface(JBir))

module A3BirPrinter = Make(MakeA3BirLikePrintInterface(A3Bir)) 

module MakeSSA(SSA:JBirSSA.Internal.CodeSig)
  (PI:PrintInterface with type instr=SSA.instr and type code = SSA.t) =
struct
  include PI
  let inst_html' = inst_html 
    
  let phi_nodes ioc ms =
    match Javalib.get_method ioc ms with
	ConcreteMethod cm -> 
	  (match cm.cm_implementation with
	       Java laz -> 
		 let code = Lazy.force laz in
		   fun pp -> code.SSA.phi_nodes.(pp), code.SSA.preds.(pp)
		   | Native -> fun _ -> [], Array.make 0 0)
      | AbstractMethod _ -> fun _ -> [], Array.make 0 0
	  
  let inst_html program ioc ms pp op = 
    let res = inst_html' program ioc ms pp op in
    let (phi_nodes,preds) = phi_nodes ioc ms pp in
      if phi_nodes = [] then res
      else
	(simple_elem 
	   (Printf.sprintf
	      "(preds(%d) := (%s)): "
	      pp
	      (JUtil.print_list_sep_map 
		 ", " string_of_int (Array.to_list preds))))
	::(List.fold_right
	     (fun  phi_n elts -> 
		let el = 
		  simple_elem (("  "^SSA.print_phi_node ~phi_simpl:false phi_n)^";") 
		in
		  el::elts
	     )
	     phi_nodes
	     res)
end

module JBirSSAPrinter = Make(MakeSSA(JBirSSA)(MakeJBirLikePrintInterface(JBirSSA)))

module A3BirSSAPrinter = Make(MakeSSA(A3BirSSA)(MakeA3BirLikePrintInterface(A3BirSSA)))
