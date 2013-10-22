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
open JBasics
open JProgram
open Javalib 
open JPrintUtil


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
let html_info_tag = "html"

let info_tag = "info"
let ival_tag = "value"
let pp_tag = "pp"

let true_val = "true"
let false_val = "false"

let method_sig_desc ms =
  ((ms_name ms) ^":(")
  ^(List.fold_left
      (fun msg t -> 
	 msg^(JDumpBasics.type2shortstring t))
      ""
      (ms_args ms))
  ^(")"^JDumpBasics.rettype2shortstring (ms_rtype ms))

type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string

type 'a warning_pp = string * 'a option

type 'a plugin_info = 
    {
      mutable p_infos : 
	(string list 
	 * string list FieldMap.t 
	 * (method_info list * string list Ptmap.t) MethodMap.t) 
	ClassMap.t;

      mutable p_warnings : 
	(string list 
	 * string list FieldMap.t 
	 * (method_info list * 'a warning_pp list Ptmap.t) MethodMap.t) 
	ClassMap.t;
    }


type ('c,'f,'m,'p) iow = ('c list 
			   * 'f list FieldMap.t 
			   * ('m list * 'p list Ptmap.t) MethodMap.t)

let find_class_infos cn map = 
  try ClassMap.find cn map
  with Not_found -> 
    ([],FieldMap.empty,MethodMap.empty)

let find_field_infos fs map = 
  try FieldMap.find fs map with Not_found -> []

let find_meth_infos ms map = 
  try MethodMap.find ms map with Not_found -> ([],Ptmap.empty)

let find_pp_infos ms pc map = 
  let (mil,pmap) = 
    try MethodMap.find ms map
    with Not_found -> ([],Ptmap.empty)
  in
  let ppinfos = 
    try Ptmap.find pc pmap 
    with Not_found -> []
  in
    ((mil,pmap),ppinfos)

let add_class_iow i cn map =
  let (ci,fi,mi) = find_class_infos cn map in
    ClassMap.add cn (i::ci,fi,mi) map

let add_field_iow i cn fs map = 
  let (ci,fi,mi) = find_class_infos cn map in
  let fil = find_field_infos fs fi in
  let fi = FieldMap.add fs (i::fil) fi in
    ClassMap.add cn (ci,fi,mi) map

let add_meth_iow i cn ms map = 
  let (ci,fi,mi) = find_class_infos cn map in
  let (mil,ptmap) = find_meth_infos ms mi in
  let mi = MethodMap.add ms (i::mil,ptmap) mi in
    ClassMap.add cn (ci,fi,mi) map

let add_pp_iow i cn ms pc map = 
  let (ci,fi,mi) = find_class_infos cn map in
  let ((mil,pmap),ppil) = find_pp_infos ms pc mi in
  let mi = MethodMap.add ms (mil,(Ptmap.add pc (i::ppil) pmap)) mi in
    ClassMap.add cn (ci,fi,mi) map


module NewCodePrinter = 
struct

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
      | MethodInvocationNonVirtual of class_name * method_signature (* ms ? *)
      | MethodInvocationVirtual of object_type * method_signature 
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

    let ot2attr ot = 
      (wvtype,JPrint.object_type ~jvm:true ot)

    let vt_opt2attrs = 
      function
	  None -> []
	| Some vt -> [wvtype,JPrint.value_type ~jvm:true vt]

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
	       | MethodInvocationNonVirtual (cn,ms) -> [(wastnode,"MethodInvocationNonVirtual"); (wcname, cn_name cn);
							(wvname,ms_name ms); (desc_attr, method_sig_desc ms)]
	       | MethodInvocationVirtual (ot,ms) -> [(wastnode,"MethodInvocationVirtual"); (ot2attr ot);
						     (wvname,ms_name ms); (desc_attr, method_sig_desc ms)]
	       | ArrayAccess vt_opt -> (wastnode,"ArrayAccess")::(vt_opt2attrs vt_opt)
	       | ArrayStore vt_opt -> (wastnode,"ArrayStore")::(vt_opt2attrs vt_opt)
	       | InstanceOf ot -> [(wastnode,"InstanceOf");(ot2attr ot)]
	       | Cast ot -> [(wastnode,"Cast");(ot2attr ot)])
	| Name id -> 
	    (match id with
		 SimpleName (name,vt_opt) -> 
		   (wastnode,"SimpleName")::(wvname,name)::(vt_opt2attrs vt_opt))
  end

  type 'a precise_warning_pp = 
      LineWarning of 'a warning_pp
	(**warning description * optional precision depending of code
	   representation (used for PreciseLineWarning generation)*)
    | PreciseLineWarning of string * AdaptedASTGrammar.node_unit
	(** same as LineWarning * AST information **)

  module type PluginPrinter =
  sig
    type code
    type expr

    val empty_infos: expr plugin_info

    val print_class: ?html_info:bool -> expr plugin_info -> code interface_or_class -> string -> unit

    val print_program: ?html_info:bool -> expr plugin_info -> code program -> string -> unit
      
  end

  module type PrintInterface =
  sig

    type p_code
    type p_expr

    (** [get_source_line_number pc code] returns the source line number
	corresponding the program point pp of the method code m.*)
    val get_source_line_number : int -> p_code -> int option

    (** [inst_disp code pc] returns a string representation of instruction at program point [pc].*)
    val inst_disp : int -> p_code -> string

    (** Function to provide in order to display the source variable
	names in the method signatures. *)
    val method_param_names : p_code Javalib.interface_or_class -> method_signature
      -> string list option

    (** Allows to construct detailed warning but it requires good
	knowledge of org.eclipse.jdt.core.dom.AST representation. See
	existant implementation of to_plugin_warning or simply transform a
	warning_pp SimpleWarning in a PreciseWarning of warning.
    *)
      
    val to_plugin_warning : p_code jmethod -> p_expr precise_warning_pp list Ptmap.t 
      -> p_expr precise_warning_pp list Ptmap.t

  end

  let gen_warning_sig wlist = 
    List.map 
      (fun msg ->
	 let attr = [(wmsg_tag,msg)] in
	   gen_simple_tag warn_tag attr)
      wlist

  let gen_info_sig fhtml ilist = 
    List.map 
      (fun info -> 
	 let info_xml = 
	   fhtml info
	 in
	   gen_custom_tag info_tag [] [info_xml]
      )
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

  let gen_info_msig fhtml ilist = 
    List.map 
      (fun info -> 
	 let attr, msg = 
	   match info with
	     | MethodSignature msg -> ([(meth_sig_on,"method")],msg)
	     | Argument (num,msg) -> ([(meth_sig_on,"argument");(meth_arg_num,string_of_int num)],msg)
	     | Return msg -> ([(meth_sig_on,"return")],msg)
	     | This msg -> ([(meth_sig_on,"this")],msg)
	 in
	   gen_custom_tag info_tag attr [(fhtml msg)])
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


  let gen_info_pp fhtml disp line pc ilist  =
    let line = 
      match line with
	  None -> -1
	| Some l -> l
    in
    let attrs = [(wpp_pc,string_of_int pc);(wpp_line,string_of_int line);(ival_tag,disp)] in
    let infos = 
      List.map
	(fun info -> 
	   gen_custom_tag info_tag [] [(fhtml info)])
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
      ::("name",ms_name ms)::[]
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

  let gen_class_tag ?(html=false) ioc treel = 
    let classname = get_name ioc in
    let attrs =
      let others_attrs = 
	if html
	then [(html_info_tag,true_val)]
	else []
      in
      let class_attrs = 
	let rec name_inner_anon inner_list =
	  match inner_list with
	      [] -> (class_cname,cn_name classname)::others_attrs
	    | ic::r -> 
		(match ic.ic_class_name with
		     None -> name_inner_anon r
		   | Some cn -> 
		       if (cn_equal classname cn)
		       then
			 (match ic.ic_source_name with
			      None -> 
				(class_cname,cn_name classname):: 
				  (class_inner,true_val):: 
				  (class_anon,true_val)::others_attrs
			    | Some _ -> 
				(class_cname,cn_name classname)::
				  (class_inner,true_val)::others_attrs)
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
    type code = S.p_code
    type expr = S.p_expr

    let (empty_infos:expr plugin_info) = {
      p_infos = ClassMap.empty;
      p_warnings = ClassMap.empty
    }

    let ioc2xml_warn precise_warn info ioc = 
      let cn = get_name ioc in
	if ClassMap.mem cn info.p_warnings
	then
	  begin
	    let wclass, wfields, wmeth_pp = 
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
			   precise_warn jm (snd (MethodMap.find ms wmeth_pp)) in
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
			 fst (MethodMap.find ms wmeth_pp)
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
	  
    let ioc2xml_info fhtml info ioc = 
      let cn = get_name ioc in
	if ClassMap.mem cn info.p_infos
	then
	  begin
	    let iclass, ifields, imeth_pp = 
	      ClassMap.find cn info.p_infos 
	    in
	    let class_tags = 
	      match iclass with
		  [] -> []
		| l -> gen_info_sig fhtml l
	    in
	    let fields_tags = 
	      FieldMap.fold
		(fun fs infos tree_list ->
		   match infos with
		       [] -> tree_list
		     | _ -> (gen_field_tag fs (gen_info_sig fhtml infos))::tree_list
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
			 let ipps_of_meth = snd (MethodMap.find ms imeth_pp) in
			   Ptmap.fold
			     (fun pc ipplist tree_list -> 
				match ipplist with 
				    [] -> tree_list
				  | _ -> 
				      (gen_info_pp 
					 fhtml
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
			 fst (MethodMap.find ms imeth_pp)
		       with Not_found -> []
		     in
		       match ims_meths, tlpp with
			   [],[] -> tlm
			 | _ -> 
			     (gen_info_method_tag  
				(S.method_param_names ioc ms) 
				ms 
				((gen_info_msig fhtml ims_meths)@tlpp))
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
	      
    let gen_class_info_doc html info ioc =
      let fhtml = if html then 
	((*TODO: check it is valid html for plugin*)
	  fun s -> CData s)
      else
	(fun s -> PCData s)
      in
      let tree_list = ioc2xml_info fhtml info ioc in
	match tree_list with
	    [] -> None
	  | _ -> Some (gen_class_tag ~html:html ioc tree_list)

    let print_info html (info_p: 'a plugin_info) ioc outputdir = 
      let cs = get_name ioc in
      let package_and_source = 
	match get_sourcefile ioc with 
	    None -> 
	      (* Try with class name as a source name.*)
	      "info"::(cn_package cs @ [(cn_simple_name cs)^".java"])
	  | Some filename ->  "info"::(cn_package cs @ [filename])
      and cname = cn_simple_name cs in
      let doc = gen_class_info_doc html info_p ioc in
	match doc with
	    None -> ()
	  | Some doc -> 
	      create_package_dir outputdir package_and_source;
	      let out =
		open_out (Filename.concat outputdir
	       		    (List.fold_left
	       		       Filename.concat "" (package_and_source @ [cname ^ ".xml"])))
	      in
		print_xml_tree ~spc:3 doc out;
		close_out out

    let print_warnings (info_p: 'a plugin_info) ioc outputdir = 
      let cs = get_name ioc in
      let package_and_source = 
	"warn"::(cn_package cs)
      and cname = cn_simple_name cs in
	(*let cpath = ExtString.String.map
	  (fun c -> if c = '.' then '/' else c) (cn_name cs) in*)
      let pp2pp_precise c map = 
	(S.to_plugin_warning)
	  c
	  (Ptmap.map 
	     (fun lpp -> 
		List.map (fun (s,prec) -> LineWarning (s,prec)) lpp)
	     map)
      in
      let doc = gen_class_warn_doc pp2pp_precise info_p ioc in
	match doc with
	    None -> ()
	  | Some doc -> 
	      create_package_dir outputdir package_and_source;
	      let out =
		open_out (Filename.concat outputdir
	       		    (List.fold_left
	       		       Filename.concat "" (package_and_source @ [cname ^ ".xml"])))
	      in
		print_xml_tree ~spc:3 doc out;
		close_out out

    let print_class ?(html_info=false) (info_p: 'a plugin_info) ioc outputdir = 
      print_warnings info_p ioc outputdir;
      print_info html_info info_p ioc outputdir

    let print_program ?(html_info=false) (info_p: 'a plugin_info) (p: S.p_code program) outputdir = 
      ClassMap.iter
	(fun _ node -> 
	   print_class ~html_info:html_info info_p (to_ioc node) outputdir)
	p.classes

  end


end

open NewCodePrinter

module JCodePrinter = Make(
  struct
    
    open JCode
    open AdaptedASTGrammar

    type p_code = jcode
    type p_expr = unit

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
		      begin
			match invtype with
			    `Interface cn
			  | `Special cn
			  | `Static cn -> 
			      Some (Expression
				      (MethodInvocationNonVirtual (cn,ms)))
			  | `Virtual ot -> 
			      Some (Expression
				      (MethodInvocationVirtual (ot,ms)))
		      end
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



