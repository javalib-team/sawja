module type S = sig
  module Var:Var.S

  (* one domain for each type of variable *)
  module Global:Domain.S
  module IOC:Domain.S
  module Field:Domain.S
  module Method:Domain.S
  module PP:Domain.S

  type analysisID = (* TODO : we could do better automatically *)
      [ `GlobalAnalysis of Global.analysisID
      | `IOCAnalysis of IOC.analysisID
      | `FieldAnalysis of Field.analysisID
      | `MethodAnalysis of Method.analysisID
      | `PPAnalysis of PP.analysisID]

  (* type of the analyses' identifiers. Automatically computed from
     the list of analyses. *)
  type analysisDomain = (* data for one particular analysis *)
      [
      | `GlobalDomain of Global.analysisDomain
      | `IOCDomain of IOC.analysisDomain
      | `FieldDomain of Field.analysisDomain
      | `MethodDomain of Method.analysisDomain
      | `PPDomain of PP.analysisDomain ]
  type abData = (* data for all analyses for one particular slot *)
      [
      | `Global of Global.t
      | `IOC of IOC.t
      | `Field of Field.t
      | `Method of Method.t
      | `PP of PP.t]

  type t
  val bot : unit -> t
  val pprint : Format.formatter -> t -> unit
  val get_pinfo : 'a JProgram.program -> t -> JPrintHtml.info -> JPrintHtml.info

  val join : ?modifies:bool ref -> t -> Var.t -> analysisDomain -> unit
  val join_ad : ?modifies:bool ref -> abData -> analysisDomain -> abData

  val get : t -> Var.t -> abData
  val get_global : t -> Var.var_global -> Global.t
  val get_IOC : t -> Var.var_ioc -> IOC.t
  val get_field : t -> Var.var_field -> Field.t
  val get_method : t -> Var.var_method -> Method.t
  val get_PP : t -> Var.var_pp -> PP.t

  val get_ab_global : abData -> Global.t
  val get_ab_field : abData -> Field.t
  val get_ab_method : abData -> Method.t
  val get_ab_IOC : abData -> IOC.t
  val get_ab_pp : abData -> PP.t

end


(* TODO : Domain est une composition des domaines des differentes
   analyses, il faudrait donc faire le foncteur qui permet de composer
   des domaines plus simple (et d'accéder à leur valeurs) *)
(* TODO : il manque beaucoup de fonction à AB_STATE *)
module Make
  (Var:Var.S)
  (GlobalDomain:Domain.S)
  (IOCDomain:Domain.S)
  (FieldDomain:Domain.S)
  (MethodDomain:Domain.S)
  (PPDomain:Domain.S) =
struct

  module Var = Var
  module Global=GlobalDomain
  module IOC=IOCDomain
  module Field=FieldDomain
  module Method=MethodDomain
  module PP=PPDomain

  type analysisID = (* TODO : we could do better automatically *)
      [ `GlobalAnalysis of GlobalDomain.analysisID
      | `IOCAnalysis of IOCDomain.analysisID
      | `FieldAnalysis of FieldDomain.analysisID
      | `MethodAnalysis of MethodDomain.analysisID
      | `PPAnalysis of PPDomain.analysisID]
  type analysisDomain = (* data for one particular analysis *)
      [
      | `GlobalDomain of Global.analysisDomain
      | `IOCDomain of IOC.analysisDomain
      | `FieldDomain of Field.analysisDomain
      | `MethodDomain of Method.analysisDomain
      | `PPDomain of PP.analysisDomain ]
  type abData = (* data for all analyses for one particular slot *)
      [
      | `Global of Global.t
      | `IOC of IOC.t
      | `Field of Field.t
      | `Method of Method.t
      | `PP of PP.t]

  module HashGlobalVar =
    Hashtbl.Make(struct
                   type t = Var.var_global
                   let hash = Var.hash_global
                   let equal = Var.equal_global end)
  module HashIOCVar =
    Hashtbl.Make(struct
                   type t = Var.var_ioc
                   let hash = Var.hash_ioc
                   let equal = Var.equal_ioc end)
  module HashFieldVar =
    Hashtbl.Make(struct
                   type t = Var.var_field
                   let hash = Var.hash_field
                   let equal = Var.equal_field end)
  module HashMethodVar =
    Hashtbl.Make(struct
                   type t = Var.var_method
                   let hash = Var.hash_method
                   let equal = Var.equal_method end)
  module HashPPVar =
    Hashtbl.Make(struct
                   type t = Var.var_pp
                   let hash = Var.hash_pp
                   let equal = Var.equal_pp end)
  type t = {global_data:Global.t HashGlobalVar.t;
	    ioc_data:IOC.t HashIOCVar.t;
	    field_data:Field.t HashFieldVar.t;
	    method_data:Method.t HashMethodVar.t;
	    pp_data:PP.t HashPPVar.t;}

  let bot _ =
    {global_data = HashGlobalVar.create Sys.max_array_length;
     ioc_data = HashIOCVar.create Sys.max_array_length;
     field_data = HashFieldVar.create Sys.max_array_length;
     method_data = HashMethodVar.create Sys.max_array_length;
     pp_data = HashPPVar.create Sys.max_array_length}

  let pp_hashmap fmt iter map name ppkey ppcontent =
    Format.pp_open_hvbox fmt 0;
    Format.pp_print_string fmt (name^": ");
    Format.pp_print_break fmt 1 2;
    Format.pp_print_string fmt "[";
    Format.pp_open_hvbox fmt 0;
    iter
      (fun k c ->
	 Format.fprintf fmt "(@[<hv 1>%a@,|->%a@])@ "
	   ppkey k
	   ppcontent c)
      map;
    Format.pp_print_string fmt "]";
    Format.pp_close_box fmt ();
    Format.pp_close_box fmt ()

  let pprint_global fmt global_data =
    pp_hashmap fmt
      HashGlobalVar.iter
      global_data
      "Global"
      Var.pprint_global
      Global.pprint

  let pprint_ioc fmt ioc_data =
    pp_hashmap fmt
      HashIOCVar.iter
      ioc_data
      "IOC"
      Var.pprint_ioc
      IOC.pprint

  let pprint_field fmt field_data =
    pp_hashmap fmt
      HashFieldVar.iter
      field_data
      "Field"
      Var.pprint_field
      Field.pprint

  let pprint_method fmt method_data =
    pp_hashmap fmt
      HashMethodVar.iter
      method_data
      "Method"
      Var.pprint_method
      Method.pprint

  let pprint_pp fmt pp_data =
    pp_hashmap fmt
      HashPPVar.iter
      pp_data
      "PP"
      Var.pprint_pp
      PP.pprint

  let pprint fmt abst =
    Format.pp_open_hvbox fmt 0;
    pprint_global fmt abst.global_data;
    Format.pp_print_space fmt ();
    pprint_ioc fmt abst.ioc_data;
    Format.pp_print_space fmt ();
    pprint_field fmt abst.field_data;
    Format.pp_print_space fmt ();
    pprint_method fmt abst.method_data;
    Format.pp_print_space fmt ();
    pprint_pp fmt abst.pp_data;
    Format.pp_close_box fmt ()

  open JProgram

  module ClassNameSet = Set.Make(struct
                                   type t = JBasics.class_name
                                   let compare = compare end)

  (* TODO : look for a more efficient implementation *)
  let get_pinfo : 'a program -> t -> JPrintHtml.info -> JPrintHtml.info =
    fun _program abst old ->
      let ioc_info = Hashtbl.create (HashIOCVar.length abst.ioc_data)
      and field_info = Hashtbl.create (HashFieldVar.length abst.field_data)
      and method_info = Hashtbl.create (HashMethodVar.length abst.method_data)
      and pp_info = Hashtbl.create (HashPPVar.length abst.pp_data)
      and ioc_has_info = ref ClassNameSet.empty
      in
	HashIOCVar.iter
	  (function `IOC (c,ioc) -> function data ->
             let print_class fmt =
	       Format.fprintf fmt "@[<hv 2>[%a:@,%a]@]"
		 Var.Context.pprint c
		 IOC.pprint data
	     in
               ioc_has_info := ClassNameSet.add ioc !ioc_has_info;
	       Hashtbl.add ioc_info ioc print_class)
	  abst.ioc_data;
	HashFieldVar.iter
	  (function `Field (c,ioc,fs) -> function data ->
	     let print_field fmt =
	       Format.fprintf fmt "@[<hv 2>[%a:@,%a]@]"
		 Var.Context.pprint c
		 Field.pprint data
	     in
               ioc_has_info := ClassNameSet.add ioc !ioc_has_info;
	       Hashtbl.add field_info (ioc,fs) print_field)
	  abst.field_data;
	HashMethodVar.iter
	  (function `Method (c,ioc,ms) -> function data ->
	     let print_method fmt =
	       Format.fprintf fmt "@[<hv 2>[%a:@,%a]@]"
		 Var.Context.pprint c
		 Method.pprint data
	     in
               ioc_has_info := ClassNameSet.add ioc !ioc_has_info;
	       Hashtbl.add method_info (ioc,ms) print_method)
	  abst.method_data;
	HashPPVar.iter
	  (function `PP (c,ioc,ms,pc)-> function data ->
	     let print_pp fmt =
	       Format.fprintf fmt "@[<hv 2>[%a:@,%a]@]"
		 Var.Context.pprint c
		 PP.pprint data
	     in
	       Hashtbl.add pp_info (ioc,ms,pc) print_pp)
	  abst.pp_data;
	(* TODO : we should update f_class, f_field and f_method
	   depending on the results we have *)
	{JPrintHtml.p_class =
	    (fun cn ->
	       old.JPrintHtml.p_class cn
               @ (List.map
		    (function pprint ->
		       pprint Format.str_formatter;
                       Format.flush_str_formatter ())
		    (Hashtbl.find_all ioc_info cn)));
	 JPrintHtml.p_field =
	    (fun cn fs ->
	       old.JPrintHtml.p_field cn fs
               @ (List.map
		    (function pprint ->
		       pprint Format.str_formatter;
                       Format.flush_str_formatter ())
		    (Hashtbl.find_all field_info (cn,fs))));
	 JPrintHtml.p_method =
	    (fun cn ms ->
	       old.JPrintHtml.p_method cn ms
               @ (List.map
		    (function pprint ->
		       pprint Format.str_formatter;
                       Format.flush_str_formatter ())
		    (Hashtbl.find_all method_info (cn,ms))));
	 JPrintHtml.p_pp =
	    (fun cn ms i ->
	       old.JPrintHtml.p_pp cn ms i
               @ (List.map
		    (fun pprint ->
		       pprint Format.str_formatter;
                       Format.flush_str_formatter ())
		    (Hashtbl.find_all pp_info (cn,ms,i))));
	}


  let get_global t var_global =
    try HashGlobalVar.find t.global_data var_global
    with Not_found -> Global.bot

  let get_IOC t var_IOC =
    try HashIOCVar.find t.ioc_data var_IOC
    with Not_found -> IOC.bot

  let get_field t var_field =
    try HashFieldVar.find t.field_data var_field
    with Not_found -> Field.bot

  let get_method t var_method =
    try HashMethodVar.find t.method_data var_method
    with Not_found -> Method.bot

  let get_PP t var_PP =
    try HashPPVar.find t.pp_data var_PP
    with Not_found -> PP.bot

  let get_ab_global = function
    | `Global f -> f
    | _ -> invalid_arg "get_ab_global"
  let get_ab_IOC = function
    | `IOC f -> f
    | _ -> invalid_arg "get_ab_IOC"
  let get_ab_method = function
    | `Method f -> f
    | _ -> invalid_arg "get_ab_method"
  let get_ab_field = function
    | `Field f -> f
    | _ -> invalid_arg "get_ab_field"
  let get_ab_pp = function
    | `PP f -> f
    | _ -> invalid_arg "get_ab_pp"

  let get t var = match var with
    | `Global _ as var ->
	`Global (get_global t var)
    | `IOC _ as var ->
	begin
	  try `IOC (HashIOCVar.find t.ioc_data var)
	  with Not_found -> `IOC IOC.bot
	end
    | `Field _ as var ->
	begin
	  try `Field (HashFieldVar.find t.field_data var)
	  with Not_found -> `Field Field.bot
	end
    | `Method _ as var ->
	begin
	  try `Method (HashMethodVar.find t.method_data var)
	  with Not_found -> `Method Method.bot
	end
    | `PP _ as var ->
	begin
	  try `PP (HashPPVar.find t.pp_data var)
	  with Not_found -> `PP PP.bot
	end


  (* TODO: a modify function in hashtbl would improve performances *)
  let join_ad ?(modifies=ref false) (abs:abData) (data:analysisDomain) : abData =
    match abs, data with
      | `Global abs, `GlobalDomain v ->
          `Global (Global.join_ad ~modifies abs v)
      | `IOC abs, `IOCDomain v ->
	  `IOC (IOC.join_ad ~modifies abs v)
      | `Field abs, `FieldDomain v ->
	  `Field (Field.join_ad ~modifies abs v)
      | `Method abs, `MethodDomain v ->
	  `Method (Method.join_ad ~modifies abs v)
      | `PP abs, `PPDomain v ->
          `PP (PP.join_ad ~modifies abs v)
      | _ , _ ->
	  failwith ("type error: failure when trying to join incompatible"
		    ^" data type (the var type does not match the data type)")

  (* TODO: a modify function in hashtbl would improve performances *)
  let join_ad' ?(modifies=ref false) t (var:Var.t) (data:analysisDomain) : abData =
    match var, data with
      | `Global _ as var, `GlobalDomain v ->
	  let old_v =
	    try HashGlobalVar.find t.global_data var
	    with Not_found -> Global.bot
	  in `Global (Global.join_ad ~modifies old_v v)
      | `IOC _ as var, `IOCDomain v ->
	  let old_v =
	    try HashIOCVar.find t.ioc_data var
	    with Not_found -> IOC.bot
	  in `IOC (IOC.join_ad ~modifies old_v v)
      | `Field _ as var, `FieldDomain v ->
	  let old_v =
	    try HashFieldVar.find t.field_data var
	    with Not_found -> Field.bot
	  in `Field (Field.join_ad ~modifies old_v v)
      | `Method _ as var, `MethodDomain v ->
	  let old_v =
	    try HashMethodVar.find t.method_data var
	    with Not_found -> Method.bot
	  in `Method (Method.join_ad ~modifies old_v v)
      | `PP _ as var, `PPDomain v ->
          let old_v =
	    try HashPPVar.find t.pp_data var
            with Not_found -> PP.bot
	  in `PP (PP.join_ad ~modifies old_v v)
      | _ , _ ->
	  failwith ("type error: failure when trying to join incompatible"
		    ^" data type (the var type does not match the data type)")

  (* TODO: a modify function in hashtbl would improve performances *)
  let join ?(modifies=ref false) t var data =
    match var, join_ad' ~modifies t var data with
      | `Global _ as var, (`Global new_v) ->
	  if !modifies then HashGlobalVar.replace t.global_data var new_v
      | `IOC _ as var, (`IOC new_v) ->
	  if !modifies then HashIOCVar.replace t.ioc_data var new_v
      | `Field _ as var, (`Field new_v) ->
	  if !modifies then HashFieldVar.replace t.field_data var new_v
      | `Method _ as var, (`Method new_v) ->
	  if !modifies then HashMethodVar.replace t.method_data var new_v
      | `PP _ as var, (`PP new_v) ->
          if !modifies then HashPPVar.replace t.pp_data var new_v
      | _  ->
	  failwith ("type error: failure when trying to join incompatible"
		    ^" data type (the var type does not match the data type)")


end
