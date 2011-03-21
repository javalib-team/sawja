open Javalib_pack
open JBasics
open JProgram
open Javalib 
open JPrintUtil

exception NoDebugInfo of class_name

(* Tags and attributes*)
let warn_tag = "warning"
let warn_pp_tag = "warning_pp"
let wmsg_tag = "msg"
let wpp_line = "line"
let wpp_pc = "pc"
let warn_pp_precise = "precise"
let wastnode = "astnode"
let wcname = "cname"
let wvtype = "jtype"
let wsenter = "synch_enter"
let wvname = "vname"

let desc_attr = "descriptor"

let meth_arg="arg_desc"
let meth_sig_on = "on"
let meth_arg_num = "arg"

let class_tag = "class"
let class_cname = "name"
let class_sf = "sourcefile"
let class_inner = "inner"
let class_anon = "anon"
let class_super = "super"

let info_tag = "info"
let ival_tag = "value"
let pp_tag = "pp"

let true_val = "true"
let false_val = "false"



let replace_all ~str ~sub ~by =
  let continue = ref true
  and s = ref str
  and i = ref 0 in
    while !continue do
      let (c,str) = ExtString.String.replace ~str:!s ~sub ~by in
	s := str;
	continue := c;
	incr i
    done;
    (!i,!s)

let replace_forb_ch s =
  snd 
    (replace_all
       ~str:(snd (replace_all ~str:s ~sub:"<" ~by:"^"))
       ~sub:">" ~by:"$")


let method_sig_desc ms =
  (replace_forb_ch (ms_name ms) ^":(")
  ^(List.fold_left
      (fun msg t -> 
	 msg^(JDumpBasics.type2shortstring t))
      ""
      (ms_args ms))
  ^(")"^JDumpBasics.rettype2shortstring (ms_rtype ms))


module AdaptedASTGrammar = 
struct
  type identifier = 
      SimpleName of string * value_type option
	(** It could be a variable identifier, field name, class name,
	  etc. Only use the shortest name possible (no package name
	  before class name, no class name before a field name, etc.).*)
  type expression = 
      (*| NullLiteral 
	| StringLiteral of string
	| TypeLiteral of identifier
	| OtherLiteral of float*)
      (* Others constants (impossible to differenciate int and bool in bytecode, ...)*)
    | Assignment of identifier
	(** Identifier must be the identifier of the left_side of
	    assignment (field's name or variable's name)*)
    | ClassInstanceCreation of class_name
    | ArrayCreation of value_type
    | MethodInvocation of class_name * method_signature (* ms ? *)
    | ArrayAccess of value_type option(* Ok if a[][] we could not know, optimistic (only one tab access on a line ?)*)
    | ArrayStore of value_type option(* It will be searched only in left_side of assignements*)
    | InstanceOf of object_type
    | Cast of object_type
	
  type statement = 
      If (*of InfixExpr option *) (* => same reason than for infix expr*)
	(** Includes all If 'like' statements (If, For, While, ConditionnalExpr, etc.) *)
    | Catch of class_name (*type given by handlers table*)
    | Finally
    | Switch 
    | Synchronized of bool(* ?Monitor exit corresponds to end parenthisis...*)
    | Return
    | Throw
	(*| AssertStat (*How to find them in bytecode: creation of a field
	  in class + creation of exception to throw*)*)
  type node_unit = 
      Statement of statement 
    | Expression of expression 
    | Name of identifier

  let vt_opt2attrs = 
    function
	None -> []
      | Some vt -> [wvtype,JPrint.value_type ~jvm:false vt]

  let ast_node2attributes = 
    function
	Statement st -> 
	  (match st with
	       If -> [wastnode,"If"]
	     | Catch cn -> [(wastnode,"Catch");(wcname,cn_name cn)]
	     | Finally -> [wastnode,"Finally"]
	     | Switch -> [wastnode,"Switch"]
	     | Synchronized enterb -> [(wastnode,"Synchronized");(wsenter,string_of_bool enterb)]
	     | Return -> [wastnode,"Return"]
	     | Throw -> [wastnode,"Throw"])
      | Expression e -> 
	  (match e with
	    | Assignment id -> 
		let id_attrs = 
		  match id with
		      SimpleName (name, vt_opt) -> 
			(wvname,name)::(vt_opt2attrs vt_opt)
		in
		  (wastnode,"Assignment")::id_attrs
	    | ClassInstanceCreation cn -> [(wastnode,"ClassInstanceCreation");(wcname,cn_name cn)]
	    | ArrayCreation vt -> (wastnode,"ArrayCreation")::(vt_opt2attrs (Some vt))
	    | MethodInvocation (cn,ms) -> [(wastnode,"MethodInvocation"); (wcname, cn_name cn);
					   (wvname,ms_name ms); (desc_attr, method_sig_desc ms)]
	    | ArrayAccess vt_opt -> (wastnode,"ArrayAccess")::(vt_opt2attrs vt_opt)
	    | ArrayStore vt_opt -> (wastnode,"ArrayStore")::(vt_opt2attrs vt_opt)
	    | InstanceOf ot -> (wastnode,"InstanceOf")::(vt_opt2attrs (Some(TObject ot)))
	    | Cast ot -> (wastnode,"Cast")::(vt_opt2attrs (Some(TObject ot))))
      | Name id -> 
	  (match id with
	       SimpleName (name,vt_opt) -> 
		 (wastnode,"SimpleName")::(wvname,name)::(vt_opt2attrs vt_opt))
end

type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string

type 'a warning_pp = 
    (*line * description *)
    LineWarning of string * 'a option
      (* idem + AST information ... *)
  | PreciseLineWarning of string * AdaptedASTGrammar.node_unit

type 'a plugin_info = 
    {
      p_infos : 
	(string list 
	 * string list FieldMap.t 
	 * method_info list MethodMap.t 
	 * string list Ptmap.t MethodMap.t) 
	ClassMap.t;
      (** infos that could be displayed for a class (one entry in ClassMap.t): 
	  (class_infos * fields_infos * methods_infos * pc_infos)*)

      p_warnings : 
	(string list 
	 * string list FieldMap.t 
	 * method_info list MethodMap.t 
	 * 'a warning_pp list Ptmap.t MethodMap.t) 
	ClassMap.t;
      (** warnings to display for a class (one entry in ClassMap.t): 
	  (class_warnings * fields_warnings * methods_warnings * pc_warnings)*)
    }

module type PluginPrinter =
sig
  type code
  type expr

  val print_class: expr plugin_info -> code interface_or_class -> string -> unit

  val print_program: expr plugin_info -> code program -> string -> unit
 
end

module type PrintInterface =
sig

  type instr
  type code
  type expr

  (** [get_source_line_number pc code] returns the source line number
      corresponding the program point pp of the method code m.*)
  val get_source_line_number : int -> code -> int option

  (** [inst_disp code pc] returns a string representation of instruction at program point [pc].*)
  val inst_disp : int -> code -> string

  (** Function to provide in order to display the source variable
      names in the method signatures. *)
  val method_param_names : code Javalib.interface_or_class -> method_signature
    -> string list option

  (** Allows to construct detailed warning but it requires good
      knowledge of org.eclipse.jdt.core.dom.AST representation. See
      existant implementation of to_plugin_warning or simply transform a
      warning_pp SimpleWarning in a PreciseWarning of warning.
  *)
    
  val to_plugin_warning : code jmethod -> expr warning_pp list Ptmap.t 
    -> expr warning_pp list Ptmap.t

end

let gen_warning_sig wlist = 
  List.map 
    (fun msg ->
       let attr = [(wmsg_tag,msg)] in
	 gen_simple_tag warn_tag attr)
    wlist

let gen_info_sig ilist = 
  List.map 
    (fun info -> gen_custom_tag info_tag [] [PCData info])
    ilist

let gen_warning_msig wlist = 
  List.map 
    (fun meth_i -> 
       let attr = 
	 match meth_i with
	   | MethodSignature msg -> [(meth_sig_on,"method");(wmsg_tag,msg)]
	   | Argument (num,msg) -> [(meth_sig_on,"argument");(meth_arg_num,string_of_int num);(wmsg_tag,msg)]
	   | Return msg -> [(meth_sig_on,"return");(wmsg_tag,msg)]
	   | This msg -> [(meth_sig_on,"this");(wmsg_tag,msg)]
       in
	 gen_simple_tag warn_tag attr)
    wlist

let gen_info_msig ilist = 
  List.map 
    (fun info -> 
       let attr, msg = 
	 match info with
	   | MethodSignature msg -> ([(meth_sig_on,"method")],msg)
	   | Argument (num,msg) -> ([(meth_sig_on,"argument");(meth_arg_num,string_of_int num)],msg)
	   | Return msg -> ([(meth_sig_on,"return")],msg)
	   | This msg -> ([(meth_sig_on,"this")],msg)
       in
	 gen_custom_tag info_tag attr [PCData msg])
    ilist

let gen_warning_pp line pc wlist  =
  let line = 
	 match line with
	     None -> -1
	   | Some l -> l
  in
  List.map
    (function wsig -> 
       let attrs = 
	 match wsig with
	     LineWarning (msg,_) -> 
	       [(warn_pp_precise, false_val);(wpp_pc,string_of_int pc)
	       ;(wpp_line,string_of_int line);(wmsg_tag,msg)]
	   | PreciseLineWarning (msg,ast_node) -> 
	       (warn_pp_precise, true_val)::(wpp_pc,string_of_int pc)
	       ::(wpp_line,string_of_int line)::(wmsg_tag,msg)
	       ::(AdaptedASTGrammar.ast_node2attributes ast_node)
       in
	 gen_simple_tag warn_pp_tag attrs)
    wlist


let gen_info_pp disp line pc ilist  =
  let line = 
	 match line with
	     None -> -1
	   | Some l -> l
  in
  let disp = replace_forb_ch disp in
  let attrs = [(wpp_pc,string_of_int pc);(wpp_line,string_of_int line);(ival_tag,disp)] in
  let infos = 
    List.map
      (fun info -> 
	 gen_custom_tag info_tag [] [PCData info])
      ilist
  in
    gen_custom_tag pp_tag attrs infos
      

let gen_field_tag fs warnings = 
  let attrs = 
    (desc_attr,(fs_name fs)^":"^(JDumpBasics.type2shortstring (fs_type fs)))::[]
  in
    gen_custom_tag "field" attrs warnings

let gen_method_tag ms warnings = 
  let attrs = 
    (desc_attr,method_sig_desc ms)::[]
  in
    gen_custom_tag "method" attrs warnings

let gen_info_method_tag mpn ms infos = 
  let attrs = 
    (desc_attr,method_sig_desc ms)
    ::("rtype",JPrint.return_type (ms_rtype ms))
    ::("name",replace_forb_ch (ms_name ms))::[]
  in
  let args = 
    ExtList.List.mapi
      (fun i vt -> 
	 let name =
	   match mpn with 
	       None -> []
	     | Some mpn -> 
		 try ("name",List.nth mpn i)::[] with _ -> []
	 in
	 let arg_attrs = 
	   ("num",string_of_int i)
	   ::("type",JPrint.value_type vt)
	   ::name
	 in
	   gen_simple_tag meth_arg arg_attrs
      )
      (ms_args ms)
  in
    gen_custom_tag "method" attrs (args@infos)

let gen_class_tag ioc treel = 
  let classname = get_name ioc in
  let attrs =
    let class_attrs = 
      let rec name_inner_anon inner_list =
	match inner_list with
	    [] -> [(class_cname,cn_name classname)]
	  | ic::r -> 
	      (match ic.ic_class_name with
		   None -> name_inner_anon r
		 | Some cn -> 
		     if (cn_equal classname cn)
		     then
		       (match ic.ic_source_name with
			    None -> 
			      let super = 
				match ioc with
				    JInterface _ -> java_lang_object
				  | JClass c -> 
				      (match c.c_super_class with
					   None -> java_lang_object
					 | Some csuper -> csuper)
			      in
			      [(class_cname,cn_name classname); (class_inner,true_val); 
			       (class_anon,true_val); (class_super,cn_name super)]
			  | Some scnin -> 
			      [(class_cname,scnin); (class_inner,true_val)])
		     else name_inner_anon r)
      in
	name_inner_anon (get_inner_classes ioc)
    in
    let sourcefile_attrs = 
      match get_sourcefile ioc with
	  Some name -> 
	    let pack = 
	      List.fold_left 
		(fun pack element -> pack ^ element ^ ".") 
		"" (cn_package classname)
	    in	      
	    let pname =  pack ^name in
	      (class_sf,pname)::class_attrs
	| None -> class_attrs
    in
      sourcefile_attrs
  in 
    gen_custom_tag class_tag attrs treel
      


module Make (S : PrintInterface) =
struct
  type code = S.code
  type expr = S.expr

  let ioc2xml_warn precise_warn info ioc = 
    let cn = get_name ioc in
      if ClassMap.mem cn info.p_warnings
      then
	begin
	  let wclass, wfields, wmeths, wpps = 
	    ClassMap.find cn info.p_warnings 
	  in
	  let cl_warn =
	    gen_warning_sig wclass
	  in
	  let fields_tags = 
	    FieldMap.fold
	      (fun fs wsiglist tree_list -> 
		 (gen_field_tag fs (gen_warning_sig wsiglist))::tree_list
	      )
	      wfields
	      []
	  in
	  let meths_tags = 
	    MethodMap.fold
	      (fun ms jm tlm -> 
		 let get_source_line pc =
		   match jm with
		       AbstractMethod _ -> None
		     | ConcreteMethod cm -> 
			 begin
			   match cm.cm_implementation with
			     | Native -> None
			     | Java lazcode -> 
				 S.get_source_line_number pc (Lazy.force lazcode)
				   
			 end
		 in
		 let tlm = 
		   let tlpp = 
		     try
		       let wpps_of_meth = 
			 precise_warn jm (MethodMap.find ms wpps) in
			 Ptmap.fold
			   (fun pc wpplist tree_list -> 
			      match wpplist with 
				  [] -> tree_list
				| _ -> (gen_warning_pp (get_source_line pc) pc wpplist)@tree_list
			   )
			   wpps_of_meth
			   []
		     with Not_found -> []
		   in
		   let wms_meths = 
		     try 
		       MethodMap.find ms wmeths
		     with Not_found -> []
		   in
		     match wms_meths, tlpp with
			 [],[] -> tlm
		       | _ -> 
			   (gen_method_tag ms 
			      ((gen_warning_msig wms_meths)@tlpp)
			   )::tlm 
		 in
		   tlm
	      )
	      (get_methods ioc)
	      []
	  in
	    cl_warn@fields_tags@meths_tags
	end
      else []
      
   let ioc2xml_info info ioc = 
    let cn = get_name ioc in
      if ClassMap.mem cn info.p_infos
      then
	begin
	  let iclass, ifields, imeths, ipps = 
	    ClassMap.find cn info.p_infos 
	  in
	  let class_tags = 
	    match iclass with
		[] -> []
	      | l -> gen_info_sig l
	  in
	  let fields_tags = 
	    FieldMap.fold
	      (fun fs infos tree_list ->
		 match infos with
		     [] -> tree_list
		   | _ -> (gen_field_tag fs (gen_info_sig infos))::tree_list
	      )
	      ifields
	      []
	  in
	  let meths_tags = 
	    MethodMap.fold
	      (fun ms jm tlm -> 
		 let code = 
		    match jm with
		       AbstractMethod _ -> None
		     | ConcreteMethod cm -> 
			 begin
			   match cm.cm_implementation with
			     | Native -> None
			     | Java lazcode -> 
				 Some (Lazy.force lazcode)
			 end
		 in
		 let get_source_line pc =
		   match code with
		       None -> None
		     | Some code -> 
			 S.get_source_line_number pc code
		 in
		 let pc_disp pc =
		    match code with
			None -> ""
		      | Some code -> 
			  S.inst_disp pc code
		 in
		 let tlm = 
		   let tlpp = 
		     try
		       let ipps_of_meth = (MethodMap.find ms ipps) in
			 Ptmap.fold
			   (fun pc ipplist tree_list -> 
			      match ipplist with 
				  [] -> tree_list
				| _ -> 
				    (gen_info_pp 
				       (pc_disp pc)
				       (get_source_line pc)
				       pc ipplist)
				    ::tree_list
			   )
			   ipps_of_meth
			   []
		     with Not_found -> []
		   in
		   let ims_meths = 
		     try 
		       MethodMap.find ms imeths
		     with Not_found -> []
		   in
		     match ims_meths, tlpp with
			 [],[] -> tlm
		       | _ -> 
			   (gen_info_method_tag  
			      (S.method_param_names ioc ms) 
			      ms 
			      ((gen_info_msig ims_meths)@tlpp))
			   ::tlm
		 in
		   tlm
	      )
	      (get_methods ioc)
	      []
	  in
	    class_tags@fields_tags@meths_tags
	end
      else []
	
  let gen_class_warn_doc precise_warn info ioc =
    let tree_list = ioc2xml_warn precise_warn info ioc in
      match tree_list with
	  [] -> None 
	| _ -> Some (gen_class_tag ioc tree_list)
	    
  let gen_class_info_doc info ioc =
    let tree_list = ioc2xml_info info ioc in
      match tree_list with
	  [] -> None
	| _ -> Some (gen_class_tag ioc tree_list)

  let print_info (info_p: 'a plugin_info) ioc outputdir = 
    let cs = get_name ioc in
    let package_and_source = 
      match get_sourcefile ioc with 
	  None -> assert false
	| Some filename ->  "info"::(cn_package cs @ [filename])
    and cname = cn_simple_name cs in
    let doc = gen_class_info_doc info_p ioc in
      match doc with
	  None -> ()
	| Some doc -> 
	    create_package_dir outputdir package_and_source;
	    let out =
	      open_out (Filename.concat outputdir
	       		  (List.fold_left
	       		     Filename.concat "" (package_and_source @ [cname ^ ".xml"])))
	    in
	      print_html_tree ~spc:3 doc out;
	      close_out out

  let print_warnings (info_p: 'a plugin_info) ioc outputdir = 
    let cs = get_name ioc in
    let package_and_source = 
      "warn"::(cn_package cs)
    and cname = cn_simple_name cs in
      (*let cpath = ExtString.String.map
	(fun c -> if c = '.' then '/' else c) (cn_name cs) in*)
    let doc = gen_class_warn_doc (S.to_plugin_warning) info_p ioc in
      match doc with
	  None -> ()
	| Some doc -> 
	    create_package_dir outputdir package_and_source;
	    let out =
	      open_out (Filename.concat outputdir
	       		  (List.fold_left
	       		     Filename.concat "" (package_and_source @ [cname ^ ".xml"])))
	    in
	      print_html_tree ~spc:3 doc out;
	      close_out out

  let print_class (info_p: 'a plugin_info) ioc outputdir = 
    print_warnings info_p ioc outputdir;
    print_info info_p ioc outputdir

  let print_program (info_p: 'a plugin_info) (p: S.code program) outputdir = 
    ClassMap.iter
      (fun _ node -> 
	 print_class info_p (to_ioc node) outputdir)
      p.classes

end

let get_code ioc ms = 
  match get_method ioc ms with
      AbstractMethod _ -> assert false
    | ConcreteMethod cm -> 
	begin
	  match cm.cm_implementation with
	      Native -> assert false
	    | Java laz -> 
		Lazy.force laz
	end


module JCodePrinter = Make(
  struct
    
    open JCode
    open AdaptedASTGrammar

    type code = jcode
    type instr = jopcode
    type expr = unit

    (* include JCodeUtil from JPrintUtil that contains common code
       with JPrintHtml*)
    include JCodeUtil
    
    let get_source_line_number pp code =
      get_source_line_number pp code

    let inst_disp pp code = 
      (* TODO: create a display function that uses names of variables ?*)
      JPrint.jopcode code.c_code.(pp)
 	  
    let find_ast_node code pc op = 
      let catch_or_finally = 
	List.fold_left
	  (fun nu eh -> 
	     if eh.e_handler = pc
	     then 
	       match eh.e_catch_type with
		   None -> Some (Statement
				   Finally)
		 | Some cn -> Some (Statement
				      (Catch cn))
	     else nu
	  )
	  None
	  code.c_exc_tbl
      in
	match catch_or_finally with
	    Some _ -> catch_or_finally
	  | None -> 
	      begin
		match op with
		  | OpNop | OpPop | OpPop2 | OpDup | OpDupX1 | OpDupX2 | OpDup2 | OpDup2X1 
		  | OpDup2X2 | OpSwap | OpAdd _ | OpSub _ | OpMult _ | OpDiv _ | OpRem _ 
		  | OpNeg _ | OpIShl | OpIShr | OpIAnd | OpIOr | OpIXor | OpIUShr | OpLShr
		  | OpLShl | OpLAnd | OpLOr | OpLXor | OpLUShr | OpI2L | OpI2F | OpI2D 
		  | OpL2I | OpL2F | OpL2D | OpF2I | OpF2L | OpF2D | OpD2I | OpD2L 
		  | OpD2F | OpI2B | OpI2C | OpI2S | OpCmp _ | OpGoto _ | OpJsr _ 
		  | OpRet _ | OpBreakpoint | OpInvalid | OpArrayLength
		  | OpConst _ -> None
		  | OpIf (_, _)
		  | OpIfCmp (_, _) ->  Some (Statement 
					       If)
		  | OpLoad (_jvmt,var_num) -> 
		      (match get_local_variable_info var_num pc code with
			   None -> None
			 | Some (name,vt) -> 
			     Some (Name
				     (SimpleName (name,Some vt))))
		  | OpArrayLoad _jvmt -> Some (Expression (ArrayAccess None))
		  | OpStore (_,var_num) 
		  | OpIInc (var_num,_) -> 
		      (match get_local_variable_info var_num pc code with
			   None -> None
			 | Some (name,vt) -> 
			     Some (Expression
				     (Assignment (SimpleName (name,Some vt)))))
		  | OpArrayStore _ -> Some (Expression (ArrayStore None))
		  | OpGetField (_cn, fs)
		  | OpGetStatic (_cn, fs) -> 
		      Some (Name
			      (SimpleName (fs_name fs,Some (fs_type fs))))
		  | OpPutStatic (_cn,fs) 
		  | OpPutField (_cn,fs) -> 
		      Some ( Expression
			       (Assignment 
				  (SimpleName (fs_name fs,Some (fs_type fs)))))
		  | OpTableSwitch _ 
		  | OpLookupSwitch _ -> Some (Statement Switch)
		  | OpReturn _ -> Some (Statement Return)
		  | OpInvoke (invtype, ms) -> 
		      let cn, ms = 
			(match invtype with
			     `Interface cn
			   | `Special cn
			   | `Static cn -> (cn,ms)
			   | `Virtual ot ->
			       begin
				 match ot with 
				     TClass cn -> (cn,ms)
				   | TArray _ -> (java_lang_object,ms)
			       end
			)
		      in
			if (ms_name ms) = "<init>"
			then 
			  (* TODO: not ok in case of super.<init> ... but it's special case no ?*)
			  Some (Expression
				  (ClassInstanceCreation cn))
			else
			  Some (Expression
				  (MethodInvocation (cn,ms)))
		  | OpNew cn -> Some (Expression
					(ClassInstanceCreation cn))
		  | OpNewArray vt -> Some (Expression
					     (ArrayCreation vt))
		  | OpAMultiNewArray (ot,_) -> Some (Expression
						       (ArrayCreation (TObject ot)))
		  | OpThrow -> Some (Statement
				       Throw)
		  | OpCheckCast ot -> Some (Expression
					      (Cast ot))
		  | OpInstanceOf ot-> Some (Expression 
					      (InstanceOf ot))
		  | OpMonitorEnter -> Some (Statement
					      (Synchronized true))
		  | OpMonitorExit -> Some (Statement
					     (Synchronized false))
	      end  


    let to_plugin_warning jm pp_warn_map = 
      match jm with
	  AbstractMethod _ -> pp_warn_map
	| ConcreteMethod cm -> 
	    begin
	      match cm.cm_implementation with
		  Native -> pp_warn_map
		| Java laz -> let code = Lazy.force laz in
		    Ptmap.mapi
		      (fun pc ppwlist -> 
			 let op = code.c_code.(pc) in
			   List.map 
			     (fun ppw -> 
				match ppw with
				    PreciseLineWarning _ -> ppw
				  | LineWarning (msg,_) as lw -> 
				      (match find_ast_node code pc op with
					   Some node -> PreciseLineWarning (msg,node)
					 | None -> lw)
			     )
			     ppwlist)
		      pp_warn_map
	    end
			 
  end)

module JBirPrinter = Make(
  struct
    open JBir
    open AdaptedASTGrammar

    type code = JBir.t
    type instr = JBir.instr
    type expr = JBir.expr

    include JBirUtil

    let get_source_line_number pp code =
      JBir.get_source_line_number pp code

    let inst_disp pp code = 
      JBir.print_instr code.JBir.code.(pp)
	
    let find_ast_node_of_expr =
      function 
	| JBir.Const _ -> None
	| JBir.Binop (ArrayLoad vt,_,_) -> Some (Expression (ArrayAccess (Some vt)))
	| JBir.Binop (_,_,_) -> None
	| JBir.Unop (JBir.InstanceOf ot,_) -> Some (Expression(InstanceOf ot))
	| JBir.Unop (JBir.Cast ot,_) -> Some (Expression(Cast ot))
	| JBir.Unop (_,_) -> None
	| JBir.Var (vt,var) -> 
	    Some (Name (SimpleName (var_name_g var,Some vt)))
	| JBir.Field (_,_,fs) 
	| JBir.StaticField (_,fs) -> 
	    Some (Name (SimpleName (fs_name fs,Some (fs_type fs))))

    let find_ast_node =
      function
	 | JBir.Goto _ 
	 | JBir.MayInit _ 
	 | JBir.Nop -> None
	 | JBir.AffectField (_,_,fs,_)
	 | JBir.AffectStaticField (_,fs,_) -> 
	     Some ( Expression
		      (Assignment 
			 (SimpleName (fs_name fs,Some (fs_type fs)))))
	 | JBir.Ifd ((_cmp,_e1,_e2), _pc) -> 
	     Some (Statement 
		     If)
	 | JBir.Return _ -> 
	     Some (Statement Return)
	 | JBir.Throw _ -> 
	     Some (Statement Throw)
	 | JBir.AffectVar (v,e) -> 
	     Some (Expression
		     (Assignment 
			(SimpleName (JBir.var_name_g v,Some (JBir.type_of_expr e)))))
	 | JBir.MonitorEnter _e -> 
	     Some (Statement
		     (Synchronized true))
	 | JBir.MonitorExit _e -> 
	     Some (Statement
		     (Synchronized false))
	 | JBir.NewArray (_v,vt,_le) -> 
	     Some (Expression
		     (ArrayCreation vt))
	 | JBir.New (_v,cn,_vtl,_le) -> 
	     Some (Expression
		     (ClassInstanceCreation cn))
	 | JBir.AffectArray (_e1,_e2,e3) -> 
	     Some (Expression (ArrayStore (Some (JBir.type_of_expr e3))))		  
	 | JBir.InvokeVirtual (_,_e,vk,ms,_le) ->
	     let cn = 
	       match vk with
		   VirtualCall ot -> 
		     begin
		       match ot with 
			   TClass cn -> cn
			 | TArray _ -> java_lang_object
		     end
		 | InterfaceCall cn -> cn
	     in
	       Some (Expression
		       (MethodInvocation (cn,ms)))
	 | JBir.InvokeStatic (_,cn,ms,_le) 
	 | JBir.InvokeNonVirtual (_,_,cn,ms,_le) -> 
	     Some (Expression
		     (MethodInvocation (cn,ms)))
	 | JBir.Check _ -> None



    let to_plugin_warning jm pp_warn_map = 
      let get_precise_warn_generation code pc op lw msg =
	let try_catch_or_finally _unit = 
	  let rec iter_handlers =
	      function
		  [] -> lw
		| eh::r -> 
		    if eh.e_handler = pc
		    then 
		      match eh.e_catch_type with
			  None -> PreciseLineWarning (msg,Statement Finally)
			| Some cn -> PreciseLineWarning (msg, Statement (Catch cn))
		    else iter_handlers r
	  in
	    iter_handlers code.exc_tbl
	in
	(match find_ast_node op with
	     Some node -> PreciseLineWarning (msg,node)
	   | None -> try_catch_or_finally ())
      in
	match jm with
	    AbstractMethod _ -> pp_warn_map
	  | ConcreteMethod cm -> 
	      begin
		match cm.cm_implementation with
		    Native -> pp_warn_map
		  | Java laz -> let code = Lazy.force laz in
		      Ptmap.mapi
			(fun pc ppwlist -> 
			   let op = code.code.(pc) in
			     List.map 
			       (fun ppw -> 
				  match ppw with
				      PreciseLineWarning _ -> ppw
				    | LineWarning (msg,None) as lw -> 
					get_precise_warn_generation code pc op lw msg
				    | LineWarning (msg,Some expr) as lw -> 
					(match find_ast_node_of_expr expr with
					     Some node -> PreciseLineWarning (msg,node)
					   | None -> 
					       get_precise_warn_generation code pc op lw msg)
			       )
			       ppwlist)
			pp_warn_map
	      end
  end)

module A3BirPrinter = Make(
  struct

    open A3Bir
    open AdaptedASTGrammar

    type code = A3Bir.t
    type instr = A3Bir.instr
    type expr = A3Bir.expr

    include A3BirUtil
      
    let get_source_line_number pp code =
      A3Bir.get_source_line_number pp code

    let inst_disp pp code = 
      A3Bir.print_instr code.A3Bir.code.(pp)
	
    let find_ast_node_of_expr =
      function 
	| A3Bir.BasicExpr(Var (vt,var)) -> 
	    Some (Name (SimpleName (var_name_g var,Some vt)))
	| A3Bir.BasicExpr(Const _) -> None
	| A3Bir.Binop (ArrayLoad vt,_,_) -> Some (Expression (ArrayAccess (Some vt)))
	| A3Bir.Binop (_,_,_) -> None
	| A3Bir.Unop (A3Bir.InstanceOf ot,_) -> Some (Expression(InstanceOf ot))
	| A3Bir.Unop (A3Bir.Cast ot,_) -> Some (Expression(Cast ot))
	| A3Bir.Unop (_,_) -> None
	| A3Bir.Field (_,_,fs) 
	| A3Bir.StaticField (_,fs) -> 
	    Some (Name (SimpleName (fs_name fs,Some (fs_type fs))))

    let find_ast_node =
      function
	| A3Bir.Goto _ 
	| A3Bir.MayInit _ 
	| A3Bir.Nop -> None
	| A3Bir.AffectField (_,_,fs,_)
	| A3Bir.AffectStaticField (_,fs,_) -> 
	    Some ( Expression
		     (Assignment 
			(SimpleName (fs_name fs,Some (fs_type fs)))))
	| A3Bir.Ifd ((_cmp,_e1,_e2), _pc) -> 
	    Some (Statement 
		    If)
	| A3Bir.Return _ -> 
	    Some (Statement Return)
	| A3Bir.Throw _ -> 
	    Some (Statement Throw)
	| A3Bir.AffectVar (v,e) -> 
	    Some (Expression
		    (Assignment 
		       (SimpleName (A3Bir.var_name_g v,Some (A3Bir.type_of_expr e)))))
	| A3Bir.MonitorEnter _e -> 
	    Some (Statement
		    (Synchronized true))
	| A3Bir.MonitorExit _e -> 
	    Some (Statement
		    (Synchronized false))
	| A3Bir.NewArray (_v,vt,_le) -> 
	    Some (Expression
		    (ArrayCreation vt))
	| A3Bir.New (_v,cn,_vtl,_le) -> 
	    Some (Expression
		    (ClassInstanceCreation cn))
	| A3Bir.AffectArray (_e1,_e2,e3) -> 
	    Some (Expression (ArrayStore (Some (A3Bir.type_of_basic_expr e3))))		  
	| A3Bir.InvokeVirtual (_,_e,vk,ms,_le) ->
	    let cn = 
	      match vk with
		  VirtualCall ot -> 
		    begin
		      match ot with 
			  TClass cn -> cn
			| TArray _ -> java_lang_object
		    end
		| InterfaceCall cn -> cn
	    in
	      Some (Expression
		      (MethodInvocation (cn,ms)))
	| A3Bir.InvokeStatic (_,cn,ms,_le) 
	| A3Bir.InvokeNonVirtual (_,_,cn,ms,_le) -> 
	    Some (Expression
		    (MethodInvocation (cn,ms)))
	| A3Bir.Check _ -> None



    let to_plugin_warning jm pp_warn_map = 
      let get_precise_warn_generation code pc op lw msg =
	let try_catch_or_finally _unit = 
	  let rec iter_handlers =
	    function
		[] -> lw
	      | eh::r -> 
		  if eh.e_handler = pc
		  then 
		    match eh.e_catch_type with
			None -> PreciseLineWarning (msg,Statement Finally)
		      | Some cn -> PreciseLineWarning (msg, Statement (Catch cn))
		  else iter_handlers r
	  in
	    iter_handlers code.exc_tbl
	in
	  (match find_ast_node op with
	       Some node -> PreciseLineWarning (msg,node)
	     | None -> try_catch_or_finally ())
      in
	match jm with
	    AbstractMethod _ -> pp_warn_map
	  | ConcreteMethod cm -> 
	      begin
		match cm.cm_implementation with
		    Native -> pp_warn_map
		  | Java laz -> let code = Lazy.force laz in
		      Ptmap.mapi
			(fun pc ppwlist -> 
			   let op = code.code.(pc) in
			     List.map 
			       (fun ppw -> 
				  match ppw with
				      PreciseLineWarning _ -> ppw
				    | LineWarning (msg,None) as lw -> 
					get_precise_warn_generation code pc op lw msg
				    | LineWarning (msg,Some expr) as lw -> 
					(match find_ast_node_of_expr expr with
					     Some node -> PreciseLineWarning (msg,node)
					   | None -> 
					       get_precise_warn_generation code pc op lw msg)
			       )
			       ppwlist)
			pp_warn_map
	      end

  end)

module JBirSSAPrinter = Make(
  struct

    type code = JBirSSA.t
    type instr = JBirSSA.instr
    type expr = JBirSSA.expr

    include JBirSSAUtil
      
    let get_source_line_number pp code =
      JBirSSA.get_source_line_number pp code

    let inst_disp pp code = 
      JBirSSA.print_instr code.JBirSSA.code.(pp)
	  
    (* TODO: implements AST desc ... See how to be generic for all JBir
       representations (we may have to define a common extern interface,
       since we only have access to the functor)*)
    let to_plugin_warning _ioc warn = warn

  end)

module A3BirSSAPrinter = Make(
  struct

    type code = A3BirSSA.t
    type instr = A3BirSSA.instr
    type expr = A3BirSSA.expr

    include A3BirSSAUtil

    let get_source_line_number pp code =
      A3BirSSA.get_source_line_number pp code
	
    let inst_disp pp code = 
      A3BirSSA.print_instr code.A3BirSSA.code.(pp)
    	
    (* TODO: implements AST desc ... Same thing as JBirSSA ...*)
    let to_plugin_warning _ioc warn = warn

  end)

