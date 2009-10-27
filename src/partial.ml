open JBasics
open Javalib

type t = {
  super_classes : class_name list ClassMap.t;
  classpath : class_path
}

(* let make cp cns = *)

let rec get_super_classes classpath map cn =
  try
    (ClassMap.find cn map,map)
  with
    | Not_found -> begin
	let ioc = get_class classpath cn in
	  match ioc with
	    | JClass c ->
		(match c.c_super_class with
		   | None -> ([],ClassMap.add cn [] map)
		   | Some sc ->
		       let (ss_classes,map) =
			 get_super_classes classpath map sc in
		       let super_classes = cn :: ss_classes in
			 (super_classes,ClassMap.add cn super_classes map))
	    | JInterface _ -> get_super_classes classpath map java_lang_object
      end

let make classpath cn_list =
  {
    super_classes = List.fold_left
		      (fun map cn -> snd (get_super_classes classpath map cn))
		      ClassMap.empty
		      cn_list;
    classpath = classpath
  }

let add ph cn =
  let (_,map) = get_super_classes ph.classpath ph.super_classes cn in
    { super_classes = map; classpath = ph.classpath }

exception UnknownClass of class_name
let is_sub_class ph cn1 cn2 =
  cn_equal cn1 cn2 ||
    try List.exists (cn_equal cn1) (ClassMap.find cn1 ph.super_classes)
    with Not_found -> raise (UnknownClass cn1)
