(*
 * This file is part of SAWJA
 * Copyright (c)2013 Pierre Vittet (INRIA)
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
open Printf
open JType

type reference = 
  | Null 
  | Ref of int


type parser_value =
  | VInt of int32
  | VChar of int
  | VShort of int
  | VBool of int
  | VByte of int
  | VLong of int64
  | VFloat of float
  | VDouble of float
  | VObject of (class_name * reference)
  | VArray of value_type * reference

type class_el ={
  cl_name: class_name;
  cl_static_fields: parser_value FieldMap.t;
  cl_instances: parser_value FieldMap.t Ptmap.t; (*The int key is the instance id.*)
}



type parsed_heap={
  hp_class: class_el ClassMap.t;
  hp_array: parser_value array Ptmap.t ObjectMap.t;
}


let vtype_of_parser_value v =
  match v with
    | VInt _ -> TBasic `Int
    | VChar _ -> TBasic `Char
    | VShort _ -> TBasic `Short
    | VBool _ -> TBasic `Bool
    | VByte _ -> TBasic `Byte
    | VLong _ -> TBasic `Long
    | VFloat _ -> TBasic `Float
    | VDouble _ -> TBasic `Double
    | VObject (cn,_) -> TObject (TClass cn)
    | VArray (vt, _) -> TObject (TArray vt)

(* ############Class oriented stuff ############ *)

(* map which for a ref gives its dynamic class.*)
let dynTypeOfref = ref Ptmap.empty

let raw_2_ref raw_val = 
  match raw_val with 
    | v when (Int64.compare v Int64.zero)=0 -> Null
    | _ -> Ref (Int64.to_int raw_val)

type field = 
{
  fname: string;
  fvalue: parser_value;
}

type instance = int * field list (*( instance id * field list )*)


let fs_of_field field = make_fs field.fname (vtype_of_parser_value field.fvalue)

let gen_instance id fields = (Int64.to_int id, fields) 

let gen_class name static_fields instances =
  let cn = make_cn name in
  let static_f_map = 
    List.fold_left
      (fun map field -> 
         let fs = fs_of_field field in
           FieldMap.add fs field.fvalue map
      ) 
      FieldMap.empty
      static_fields in
  let instances_map =
    List.fold_left
      (fun map (inst_id, fields) ->
         (*feed the dynTypeOfref map*)
         dynTypeOfref:=Ptmap.add inst_id (TClass cn) !dynTypeOfref; 
         let fieldsMap = 
           List.fold_left
             (fun fmap field ->
                FieldMap.add (fs_of_field field) field.fvalue fmap
             )
             FieldMap.empty
             fields
         in
         Ptmap.add inst_id fieldsMap map
      )
      Ptmap.empty
      instances in
  {
    cl_name= cn;
    cl_static_fields = static_f_map;
    cl_instances = instances_map;
  }

let add_class map cl =
  ClassMap.add cl.cl_name cl map

let get_dyn_type ref = 
  try 
    Ptmap.find ref !dynTypeOfref
  with Not_found ->
    Printf.printf "%d not found\n" ref;
    raise Not_found

type instance_ar = {
  ista_id : int;
  ista_size:int;
  ista_el:(int * int64) list; (* (index * raw value) list *)
}

let gen_instance_ar inst_id nb_el lst_el =
  {
    ista_id = Int64.to_int inst_id;
    ista_size = nb_el;
    ista_el = lst_el ;
  }

let add_instance_ar inst map = Ptmap.add inst.ista_id inst map

(*string -> value_type *)
let rec c_name_2_value c_name ar_depth = 
  (*str should not represent an array. *)
  let simple_str_2_value_type str=
    match str with
      | "int" -> TBasic `Int
      | "short" -> TBasic `Short
      | "char" -> TBasic `Char
      | "bool" -> TBasic `Bool
      | "byte" -> TBasic `Byte
      | "long" -> TBasic `Long
      | "float" -> TBasic `Float
      | "double" -> TBasic `Double
      | _ -> TObject (TClass (make_cn str))
  in
    if (ar_depth > 0)
    then TObject(TArray (c_name_2_value c_name (ar_depth -1)))
    else simple_str_2_value_type c_name 


let raw_2_parser_value c_name ar_depth raw_val = 
  let simple_str_2_parser_type c_name =
    match c_name with
      | "int" -> VInt (Int64.to_int32 raw_val)
      | "short" -> VShort (Int64.to_int raw_val)
      | "char" -> VChar (Int64.to_int raw_val)
      | "bool" -> VBool (Int64.to_int raw_val)
      | "byte" -> VByte (Int64.to_int raw_val)
      | "long" -> VLong raw_val
      | "float" -> VFloat (Int64.float_of_bits raw_val)
      | "double" -> VDouble (Int64.float_of_bits raw_val)
      | _ -> VObject (make_cn c_name, (raw_2_ref raw_val))
  in
    if ar_depth > 0 
    then 
      (
        let ref = raw_2_ref raw_val in
        let vtype = c_name_2_value c_name (ar_depth-1) in
          match ref with 
            | Null -> 
                  dynTypeOfref:= Ptmap.add 0 (TArray vtype) !dynTypeOfref;
                VArray (vtype, ref)
            | Ref ref_int ->
                  dynTypeOfref:= Ptmap.add ref_int (TArray vtype) !dynTypeOfref;
                  VArray (vtype, ref)
      )
    else simple_str_2_parser_type c_name 


(*Init java array with default value (depending of its type)*)
let init_java_array ar_size c_name ar_depth =
  Array.make ar_size (raw_2_parser_value c_name (ar_depth-1) Int64.zero)

let gen_class_ar c_name ar_depth inst_map =
  let type_instance_ar c_name ar_depth inst_map = 
    Ptmap.map
      (fun inst ->
         dynTypeOfref := Ptmap.add inst.ista_id 
                           (TArray (c_name_2_value c_name (ar_depth-1)))
                              !dynTypeOfref;
         List.fold_left
           (fun ar (idx, raw_val) ->
              ar.(idx)<- raw_2_parser_value c_name (ar_depth-1) raw_val;
              ar
           )
           (init_java_array inst.ista_size c_name ar_depth)
           inst.ista_el
      )
      inst_map
  in
 (TArray (c_name_2_value c_name (ar_depth-1)), type_instance_ar c_name ar_depth inst_map)


let finalize_class_ar (obj, inst_map) map =
  ObjectMap.add obj inst_map map

(* ############ Printer ############ *)

let reference_2_string r = 
  match r with
    | Null -> "Null"
    | Ref v -> sprintf "%d" v 

let parser_value2string v =
  match v with
    | VInt i -> sprintf "int %s" (Int32.to_string i)  
    | VChar c -> sprintf "char %d" c
    | VShort s -> sprintf "short %d" s
    | VBool b -> sprintf "bool %d" b
    | VByte b -> sprintf "byte %d" b
    | VLong l -> sprintf "long %s" (Int64.to_string l)
    | VFloat f -> sprintf "float %s" (Int64.to_string (Int64.bits_of_float f))
    | VDouble d -> sprintf "double %s" (Int64.to_string (Int64.bits_of_float d)) 
    | VObject (_,ref) -> sprintf "object %s" (reference_2_string ref)
    | VArray (_,ref) -> sprintf "[] %s" (reference_2_string ref)

let fields_to_string sf =
  FieldMap.fold
    (fun fs f_val str ->
       sprintf "%s\n\t\t %s %s = %s"
         str
         (JDumpBasics.value_signature (fs_type fs))
         (fs_name fs)
         (parser_value2string f_val)
    )
    sf
    ""

let instances_to_string ins = 
  Ptmap.fold
    (fun inst_id fmap str ->
       sprintf "%s %d {\n %s}\n" str inst_id (fields_to_string fmap)
    )
    ins
    ""


let class_to_string cl = 
  sprintf "static fields: \n \t%s \n instance: \n \t%s" 
    (fields_to_string cl.cl_static_fields)
    (instances_to_string cl.cl_instances)


let array_to_string ar = 
  (Array.fold_left
    (fun str el ->
       sprintf "%s: \t\t%s\n" str (parser_value2string el)
    )
    "["
    ar)^"]"


let array_inst_to_string ar =
  Ptmap.fold
    (fun inst_id ar str ->
       sprintf "%s %d: [%s]\n" str inst_id (array_to_string ar)
    )
    ar
    ""


let array_class_to_string ar =
  ObjectMap.fold
    (fun obj pmap str->
       sprintf "%s %s %s\n" str (JPrint.object_type obj) (array_inst_to_string pmap)
    )
    ar
    ""

let heap_to_string hp = 
  sprintf "classes: %s\n arrays: %s\n" 
    (ClassMap.fold 
       (fun cn cl_el str ->
          sprintf "%s\n%s{\n%s}\n" str (cn_name cn) 
            (class_to_string cl_el))
       hp.hp_class
       ""
    )
    (array_class_to_string hp.hp_array)
