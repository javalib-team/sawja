open Javalib_pack
open JBasics
open JProgram
open Javalib 
open JPrintUtil

exception NoDebugInfo of class_name


module AdaptedASTGrammar = 
struct
  type identifier = 
      SimpleName of string 
	(** It could be a variable identifier, field name, class name,
	  etc. Only use the shortest name possible (no package name
	  before class name, no class name before a field name, etc.).*)
  type expression = 
    (*| NullLiteral 
    | StringLiteral of string
    | TypeLiteral of identifier
    | OtherLiteral of float*)
	(* Others constants (impossible to differenciate int and bool in bytecode, ...)*)
    | Assignment of value_type option * identifier
	(** Identifier must be the identifier of the left_side of
	    assignment (field's name or variable's name)*)
    | ClassInstanceCreation of class_name
    | ArrayCreation of value_type
    | MethodInvocation of class_method_signature (* ms ? *)
    | ArrayAccess of value_type (* Ok if a[][] we could not know, optimistic (only one tab access on a line ?)*)
    | ArrayStore of value_type (* It will be searched only in left_side of assignements*)
    (*| InfixExpression of infix_operator (* ? => no because we do not know if it appears in source ...*)*)
    | InstanceOf of identifier option
    | Cast of identifier
	
  type statement = 
      If (*of InfixExpr option *) (* => same reason than for infix expr*)
	(** Includes all If 'like' statements (If, For, While, ConditionnalExpr, etc.) *)
    | Catch of identifier (*type given by handlers table*)
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

  let node_unit2attributes = 
    function
	Statement st -> 
	  (match st with
	       If -> "IfStatement"
	     | Catch _ -> "CatchClause"
	     | Finally -> "TryStatement"
	     | Switch -> "SwitchStatement"
	     | Synchronized _ -> "SynchronizedStatement"
	     | Return -> "ReturnStatement"
	     | Throw -> "ThrowStatement")
      | Expression e -> 
	  (match e with
	    | Assignment _ -> "Assignment"
	    | ClassInstanceCreation _ -> "ClassInstanceCreation"
	    | ArrayCreation _ -> "ArrayCreation"
	    | MethodInvocation _ -> "MethodInvocation"
	    | ArrayAccess _ -> "ArrayAccess"
	    | ArrayStore _ -> "ArrayStore ??"
	    | InstanceOf _ -> "InstanceOfExpression"
	    | Cast _ -> "CastExpression")
      | Name id -> 
	  (match id with
	       SimpleName _ -> "SimpleName")
end

type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string

type warning_pp = 
    (*line * description *)
    LineWarning of string
      (* idem + AST information ... *)
  | PreciseLineWarning of string * AdaptedASTGrammar.node_unit

type plugin_info = 
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
	 * warning_pp list Ptmap.t MethodMap.t) 
	ClassMap.t;
      (** warnings to display for a class (one entry in ClassMap.t): 
	  (class_warnings * fields_warnings * methods_warnings * pc_warnings)*)
    }

module type PluginPrinter =
sig
  type code

  val print_class: plugin_info -> code interface_or_class -> string -> unit

  val print_program: plugin_info -> code program -> string -> unit
 
end

module type PrintInterface =
sig

  type instr
  type code

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
    
  val to_plugin_warning : code jmethod -> warning_pp list Ptmap.t 
    -> warning_pp list Ptmap.t

end

let warn_tag = "warning"
let warn_pp_tag = "warning_pp"
let wmsg_tag = "msg"
let meth_arg="arg_desc"
let meth_sig_on = "on"
let meth_arg_num = "arg"
let wpp_line = "line"
let wpp_pc = "pc"
let info_tag = "info"
let ival_tag = "value"
let pp_tag = "pp"

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
	     LineWarning msg -> 
	       [(wpp_pc,string_of_int pc);(wpp_line,string_of_int line);(wmsg_tag,msg)]
	   | PreciseLineWarning (msg,_ast_node) -> 
	       (wpp_pc,string_of_int pc)::(wpp_line,string_of_int line)::(wmsg_tag,msg)
	       ::[]
	       (*::ast_node2attributes*)
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
    ("descriptor",(fs_name fs)^":"^(JDumpBasics.type2shortstring (fs_type fs)))::[]
  in
    gen_custom_tag "field" attrs warnings


let method_sig_desc ms =
  (replace_forb_ch (ms_name ms) ^":(")
  ^(List.fold_left
      (fun msg t -> 
	 msg^(JDumpBasics.type2shortstring t))
      ""
      (ms_args ms))
  ^(")"^JDumpBasics.rettype2shortstring (ms_rtype ms))

let gen_method_tag ms warnings = 
  let attrs = 
    ("descriptor",method_sig_desc ms)::[]
  in
    gen_custom_tag "method" attrs warnings

let gen_info_method_tag mpn ms infos = 
  let attrs = 
    ("descriptor",method_sig_desc ms)
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
  let sourcefile_att = 
    match get_sourcefile ioc with
	Some name -> 
	  let pack = 
	    List.fold_left 
	      (fun pack element -> pack ^ element ^ ".") 
	      "" (cn_package classname)
	  in	      
	  let pname =  pack ^name in
	  [("name",cn_name classname);("sourcefile",pname)]
      | None -> [("name",cn_name (get_name ioc))]
  in 
    gen_custom_tag "class" sourcefile_att treel
      


module Make (S : PrintInterface) =
struct
  type code = S.code

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

  let print_info (info_p: plugin_info) ioc outputdir = 
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

  let print_warnings (info_p: plugin_info) ioc outputdir = 
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

  let print_class (info_p: plugin_info) ioc outputdir = 
    print_warnings info_p ioc outputdir;
    print_info info_p ioc outputdir

  let print_program (info_p: plugin_info) (p: S.code program) outputdir = 
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

    type code = JCode.jcode
    type instr = JCode.jopcode

    (* include JCodeUtil from JPrintUtil that contains common code
       with JPrintHtml*)
    include JCodeUtil
    
    let get_source_line_number pp code =
      JCode.get_source_line_number pp code

    let inst_disp pp code = 
      (* TODO: create a display function that uses names of variables ?*)
      JPrint.jopcode code.JCode.c_code.(pp)
 	  
    (* TODO: implements AST desc ...*)
    let to_plugin_warning _jm pp_warn_map = pp_warn_map
      (*match jm with
	  AbstractMethod _ -> pp_warn_map
	| ConcreteMethod cm -> 
	    begin
	      match cm.cm_implementation with
		  Native -> pp_warn_map
		| Java laz -> let code = Lazy.force laz in
		    Ptmap.mapi
		      (fun _pc ppwlist -> 
			 List.map 
			   (fun ppw -> 
			      match ppw with
				  PreciseWarning _ -> ppw
				| LineWarning _ -> ppw )
			   ppwlist)
		      pp_warn_map
	    end*)
			 
	

  end)

module JBirPrinter = Make(
  struct

    type code = JBir.t
    type instr = JBir.instr

    include JBirUtil
      
    let get_source_line_number pp code =
      JBir.get_source_line_number pp code

    let inst_disp pp code = 
      JBir.print_instr code.JBir.code.(pp)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module A3BirPrinter = Make(
  struct

    type code = A3Bir.t
    type instr = A3Bir.instr

    include A3BirUtil
      
    let get_source_line_number pp code =
      A3Bir.get_source_line_number pp code

    let inst_disp pp code = 
      A3Bir.print_instr code.A3Bir.code.(pp)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module JBirSSAPrinter = Make(
  struct

    type code = JBirSSA.t
    type instr = JBirSSA.instr

    include JBirSSAUtil
      
    let get_source_line_number pp code =
      JBirSSA.get_source_line_number pp code

    let inst_disp pp code = 
      JBirSSA.print_instr code.JBirSSA.code.(pp)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module A3BirSSAPrinter = Make(
  struct

    type code = A3BirSSA.t
    type instr = A3BirSSA.instr

    include A3BirSSAUtil

    let get_source_line_number pp code =
      A3BirSSA.get_source_line_number pp code
	
    let inst_disp pp code = 
      A3BirSSA.print_instr code.A3BirSSA.code.(pp)
    	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

