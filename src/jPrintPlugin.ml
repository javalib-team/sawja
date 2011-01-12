open Javalib_pack
open JBasics
open JProgram
open Javalib 
open JPrintUtil

exception NoDebugInfo of class_name

(*type attribute = Attr of string * string

type tag = 
    Element of string * attribute list * tag list
  | PCData of string


type xml_doc = 
   {
     p_class : class_name -> tag;
     (** Root balise will be closed after adding p_field, p_methods
	 and p_pp balises *)
     p_field : class_name -> field_signature -> tag;
     (** Prints field information that is printed along with the
	 corresponding field. *)
      p_method : class_name -> method_signature -> tag;
      (** Prints method information that is printed inside the method,
	  along with other attributes of the method. *)
      p_pp : class_name -> method_signature -> int -> tag;
      (** Prints information associated to program points. The
	  information is printed after the instruction. *)

      p_warnings : tag ClassMap.t * tag ClassMethodMap.t * tag ClassFieldMap.t * tag Ptmap.t ClassMap.t;
   }
*)

(** priority * type * type_description *)
type warning_type = WarnType of int * string * string 

(* DO NOT DIFFERENCIATE INFO AND ANNOT
type info_tag = 
    [ `Info of string ]

type info_or_annot_tag =
    [ `Info of string
    | `Annot of string * info_or_annot_tag option ]

type method_tag = 
  | MethodSignature of info_or_annot_tag
  | Argument of int * info_or_annot_tag
  | Return of info_tag
  | PreThis of info_tag
   | PostThis of info_tag
*)  

type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string

type attribute = string * string


type warning_pp = 
    (*line * type * short_desc * long_desc *)
    LineWarning of int * string * string * string option
      (* idem + AST information ... *)
  | PreciseWarning of int * string * string * string option * attribute list 

type warning_sig = 
    (** type * msg * detailed msg*)
    string * string * string option
      
type warning_meth_sig = 
    (** type * msg * detailed msg*)
    string * method_info * string option

type plugin_info = 
    {
      p_class : class_name -> string list;

      p_field : class_name -> field_signature -> string list;

      p_method : class_name -> method_signature -> method_info list;

      p_pp : class_name -> method_signature -> int -> string list;

      p_warnings : 
	(warning_sig list * warning_sig list FieldMap.t 
	 * warning_meth_sig list MethodMap.t * warning_pp list Ptmap.t MethodMap.t) 
	ClassMap.t;

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

  val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit

  (** instr -> display -> line*)
  val inst_disp : code interface_or_class -> method_signature -> int
    -> instr -> string * int

  (** Function to provide in order to display the source variable
      names in the method signatures. *)
  val method_param_names : code Javalib.interface_or_class -> method_signature
    -> string list option

  (** Allows to construct detailed warning but it requires good
      knowledge of org.eclipse.jdt.core.dom.AST representation. See
      existant implementation of to_plugin_warning or simply transform a
      warning_pp SimpleWarning in a PreciseWarning of warning.

      CAUTION: For efficiency implements with store ioc and then fun
      warning_pp -> ....*)
    
  val to_plugin_warning : code interface_or_class -> warning_pp -> warning_pp

end

let warn_tag = "warning"
let wtype_tag = "type"
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
    (fun wsig -> 
       let typ, msg, long_msg = wsig in
       let attr = [(wtype_tag,typ);(wmsg_tag,msg)] in
	 match long_msg with 
	     None -> gen_simple_tag warn_tag attr
	   | Some long_msg-> gen_custom_tag warn_tag attr [PCData (long_msg)])
    wlist

let gen_info_sig ilist = 
  List.map 
    (fun info -> gen_custom_tag info_tag [] [PCData info])
    ilist

let gen_warning_msig wlist = 
  List.map 
    (fun wsig -> 
       let typ, meth_i, long_msg = wsig in
       let attr = 
	 (wtype_tag,typ)::
	   match meth_i with
	     | MethodSignature msg -> [(meth_sig_on,"method");(wmsg_tag,msg)]
	     | Argument (num,msg) -> [(meth_sig_on,"argument");(meth_arg_num,string_of_int num);(wmsg_tag,msg)]
	     | Return msg -> [(meth_sig_on,"return");(wmsg_tag,msg)]
	     | This msg -> [(meth_sig_on,"this");(wmsg_tag,msg)]
       in
	 match long_msg with 
	     None -> gen_simple_tag warn_tag attr
	   | Some long_msg-> gen_custom_tag warn_tag attr [PCData (long_msg)])
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

let gen_warning_pp precise_warn pc wlist  =
  List.map
    (fun wsig -> 
       let (attrs,long_msg) = 
	 match precise_warn wsig with
	     LineWarning (l,typ,msg,long_msg) -> 
	       [(wtype_tag,typ);(wpp_pc,string_of_int pc);(wpp_line,string_of_int l);(wmsg_tag,msg)]
		 ,long_msg
	   | PreciseWarning (l,typ,msg,long_msg,attrs) -> 
	       (wtype_tag,typ)::(wpp_pc,string_of_int pc)
	       ::(wpp_line,string_of_int l)::(wmsg_tag,msg)::attrs
		 ,long_msg
       in
	 match long_msg with 
	     None -> gen_simple_tag warn_tag attrs
	   | Some long_msg-> gen_custom_tag warn_tag attrs [PCData (long_msg)])
    wlist


let gen_info_pp inst_disp ioc ms pc inst ilist  =
  let disp, line = inst_disp ioc ms pc inst in
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
	let meths_tags, pp_tags = 
	  MethodMap.fold
	    (fun ms _ (tlm,tlpp) -> 
	       let tlm = 
		 try 
		   (gen_method_tag ms (gen_warning_msig (MethodMap.find ms wmeths)))::tlm 
		 with Not_found -> tlm
	       and tlpp = 
		 try 
		   Ptmap.fold
		     (fun pc wpplist tree_list -> 
			match wpplist with 
			    [] -> tree_list
			  | _ -> (gen_warning_pp precise_warn pc wpplist)@tree_list
		     )
		     (MethodMap.find ms wpps)
		     tlpp
		 with Not_found -> tlpp
	       in
		 (tlm,tlpp)
	    )
	    (get_methods ioc)
	    ([],[])
	in
	  cl_warn@fields_tags@meths_tags@pp_tags
      end
    else []


let gen_class_tag ioc treel = 
  let sourcefile_att = 
    match get_sourcefile ioc with
	Some name -> [("name",cn_name (get_name ioc));("sourcefile",name)]
      | None -> [("name",cn_name (get_name ioc))]
  in 
    gen_custom_tag "class" sourcefile_att treel
      
let gen_class_warn_doc 
    (precise_warn: warning_pp -> warning_pp) 
    info 
    (ioc:'a Javalib_pack.Javalib.interface_or_class)
    =
  gen_class_tag ioc (ioc2xml_warn precise_warn info ioc)
  

module Make (S : PrintInterface) =
struct
  type code = S.code

  (* TODO: si pas d'info sur aucun PP d'une methode, ne pas dumper
     les PP on affichera alors juste la signature de la méthode dans
     le plugin ... Faire pareil pour si aucun info pour une méthode
     ou champs => DONE *)
  let ioc2xml_info info ioc = 
    let cn = get_name ioc in
    let cl_warn = 
      match info.p_class cn with
	  [] -> []
	| l -> gen_info_sig l
    in
    let fields_tags = 
      FieldMap.fold
	(fun fs _ tree_list ->
	   let infos = info.p_field cn fs in
	     match infos with
		 [] -> tree_list
	       | _ -> (gen_field_tag fs (gen_info_sig infos))::tree_list
	)
	(get_fields ioc)
	[]
    in
    let meths_tags = 
      MethodMap.fold
	(fun ms jm tlm -> 
	   let tlpp = 
	     match jm with
		 AbstractMethod _ -> []
	       | ConcreteMethod cm -> 
		   begin
		     match cm.cm_implementation with
		       | Native -> []
		       | Java code ->
			   let tree_list = ref [] 
			   and info_pp = ref false in
			     S.iter_code
			       (fun pp insts ->
				  List.iter
				    (fun inst -> 
				       let infos = info.p_pp cn ms pp in
					 begin
					   match infos with
					       [] -> ()
					     | _ -> info_pp := true
					 end;
					 tree_list:=
					   (gen_info_pp S.inst_disp ioc ms pp inst infos)
					 ::(!tree_list))
				    insts
			       )
			       code;
			     if !info_pp
			     then
			       !tree_list
			     else
			       []
		   end
	   in
	   let infos = info.p_method cn ms in
	     match (tlpp,infos) with
		 [],[] -> 
		   tlm
	       | _,_ ->
		   (* TODO: give details on how print method sig (access,
		      modifier, annot ?)*)
		   (gen_info_method_tag  (S.method_param_names ioc ms) ms ((gen_info_msig infos)@tlpp))
		   ::tlm

	)
	(get_methods ioc)
	[]
    in
      cl_warn@fields_tags@meths_tags

  let gen_class_info_doc info ioc =
    gen_class_tag ioc (ioc2xml_info info ioc)
      
      
  let print_info (info_p: plugin_info) ioc outputdir = 
    let cs = get_name ioc in
    let package_and_source = 
      match get_sourcefile ioc with 
	  None -> assert false
	| Some filename ->  "info"::(cn_package cs @ [filename])
    and cname = cn_simple_name cs in
    let doc = gen_class_info_doc info_p ioc in
      create_package_dir outputdir package_and_source;
      let out =
	open_out (Filename.concat outputdir
	       	    (List.fold_left
	       	       Filename.concat "" (package_and_source @ [cname ^ ".xml"])))
      in
	print_html_tree ~spc:3 doc out;
	close_out out

  let print_only_warnings (info_p: plugin_info) ioc outputdir = 
    let cs = get_name ioc in
    let package_and_source = 
      "warn"::(cn_package cs)
    and cname = cn_simple_name cs in
      (*let cpath = ExtString.String.map
	(fun c -> if c = '.' then '/' else c) (cn_name cs) in*)
    let doc = gen_class_warn_doc (S.to_plugin_warning ioc) info_p ioc in
      create_package_dir outputdir package_and_source;
      let out =
	open_out (Filename.concat outputdir
	       	    (List.fold_left
	       	       Filename.concat "" (package_and_source @ [cname ^ ".xml"])))
      in
	print_html_tree ~spc:3 doc out;
	close_out out

  let print_class (info_p: plugin_info) ioc outputdir = 
    print_only_warnings info_p ioc outputdir;
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

    include JCodeUtil
      
    let inst_disp ioc ms pp op = 
      let code = get_code ioc ms in
	(* TODO: create a display function that uses names of variables ?*)
      let disp = JPrint.jopcode op 
      and line = 
	match JCode.get_source_line_number pp code with
	    None -> raise (NoDebugInfo (get_name ioc))
	  | Some l -> l
      in
	(disp, line)
	  
    (* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module JBirPrinter = Make(
  struct

    type code = JBir.t
    type instr = JBir.instr

    include JBirUtil
      
    let inst_disp ioc ms pp op = 
      let code = get_code ioc ms in
      let disp = JBir.print_instr op 
      and line = 
	match JBir.get_source_line_number pp code with
	    None -> raise (NoDebugInfo (get_name ioc))
	  | Some l -> l
      in
	(disp, line)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module A3BirPrinter = Make(
  struct

    type code = A3Bir.t
    type instr = A3Bir.instr

    include A3BirUtil
      
    let inst_disp ioc ms pp op = 
      let code = get_code ioc ms in
      let disp = A3Bir.print_instr op 
      and line = 
	match A3Bir.get_source_line_number pp code with
	    None -> raise (NoDebugInfo (get_name ioc))
	  | Some l -> l
      in
	(disp, line)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module JBirSSAPrinter = Make(
  struct

    type code = JBirSSA.t
    type instr = JBirSSA.instr

    include JBirSSAUtil
      
    let inst_disp ioc ms pp op = 
      let code = get_code ioc ms in
      let disp = JBirSSA.print_instr op 
      and line = 
	match JBirSSA.get_source_line_number pp code with
	    None -> raise (NoDebugInfo (get_name ioc))
	  | Some l -> l
      in
	(disp, line)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

module A3BirSSAPrinter = Make(
  struct

    type code = A3BirSSA.t
    type instr = A3BirSSA.instr

    include A3BirSSAUtil
      
    let inst_disp ioc ms pp op = 
      let code = get_code ioc ms in
      let disp = A3BirSSA.print_instr op 
      and line = 
	match A3BirSSA.get_source_line_number pp code with
	    None -> raise (NoDebugInfo (get_name ioc))
	  | Some l -> l
      in
	(disp, line)
	  
(* TODO: implements AST desc ...*)
    let to_plugin_warning _ioc warn = warn

  end)

