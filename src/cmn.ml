(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
 * Copyright (c)2016 David Pichardie (ENS Rennes)
 * Copyright (c)2016 Laurent Guillo (CNRS)
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

(* Variables *)

type unindexed_var =
  | OriginalVar of int * string option (* register number, name (debug if available) *)
  | TempVar of int
  | CatchVar of int (* Variable catched by an exception handler. *)
  | BranchVar of int * int
  | BranchVar2 of int * int
  | SSAVar of var * int
and var = int * unindexed_var

let var_equal ((i1,_):var) ((i2,_):var) = i1==i2

let varname =  "$bcvar"
let tempname =  "$irvar"
let branchvarname =  "$T"
let branchvarname2 =  "$T'"

let var_name_debug (_,v) =
  match v with
    | OriginalVar (_,s) -> s
    | _ -> None

let catch_var (_,x) =
  match x with
    | CatchVar _ -> true
    | _ -> false

let temp_var (_,x) =
  match x with
    | TempVar _ -> true
    | SSAVar ((_,TempVar _),_) -> true
    | _ -> false

let ssa_var (_,x) =
  match x with
    | SSAVar _ -> true
    | _ -> false

let rec unindexed_var_name v = 
  match v with
    | OriginalVar (j,_) -> Printf.sprintf  "%s%d" varname j
    | TempVar i -> Printf.sprintf "%s%d" tempname i
    | CatchVar i -> Printf.sprintf "CatchVar%d" i
    | BranchVar (i,j) -> Printf.sprintf "%s%d_%d" branchvarname j i
    | BranchVar2 (i,j) -> Printf.sprintf "%s%d_%d" branchvarname2 j i
    | SSAVar ((_,v),i) -> Printf.sprintf "%s_%d" (unindexed_var_name v) i

let var_name (_,v) = unindexed_var_name v
		       
let rec unindexed_var_name_g x =
  match x with
    | OriginalVar (_,Some s) -> s
    | SSAVar ((_,v),i) -> Printf.sprintf "%s_%d" (unindexed_var_name_g v) i
    | _ -> unindexed_var_name x

let var_name_g (_,v) = unindexed_var_name_g v

let rec unindexed_bc_num v =
  match v with
    | OriginalVar (j,_) -> Some j
    | SSAVar ((_,v),_) -> unindexed_bc_num v
    |  _ -> None

let bc_num (_,v) = unindexed_bc_num v

let index (i,_) = i

let var_orig  (_,v) = 
  match v with
    | OriginalVar _ -> true
    | _ -> false

let var_origin (_,x) =
  match x with
    | SSAVar (v,_) -> v
    | _ -> failwith "var_origin: must be called on SSA vars"

let var_ssa_index (_,x) = 
  match x with
    | SSAVar (_,i) -> i
    | _ -> failwith "var_ssa_index: must be called on SSA vars"
	
let make_var_ssa v i =
  match snd v with
    | SSAVar _ -> failwith "make_var_ssa: must not be called on SSA vars"
    | _ -> SSAVar (v,i)

(*  let var_ssa  (_,v) = 
    match v with
    | CatchVar _ -> true
    | _ -> false
*)

module DicoVarMap = Map.Make(struct type t=unindexed_var let compare = compare end)

type dictionary =
    { mutable var_map : var DicoVarMap.t;
      mutable var_next : int }

let make_dictionary () =
  { var_map = DicoVarMap.empty;
    var_next = 0}

let make_var (d:dictionary) : unindexed_var -> var =
  function v ->
    try
      DicoVarMap.find v d.var_map
    with Not_found -> 
      let new_v = (d.var_next,v) in
	d.var_map <- DicoVarMap.add v new_v d.var_map;
	d.var_next <- 1+ d.var_next;
	new_v

let make_array_var d =
  let dummy = (-1,(TempVar (-1))) in
  let t = Array.make d.var_next dummy in
    DicoVarMap.iter (fun _  v -> t.(fst v) <- v) d.var_map;
    t

(* allows to restart a dictionary for ssa *)
let make_dictionary_from (vars : var array) =
  { var_map = Array.fold_left
                (fun dico var -> let u_v = snd var in
                                       DicoVarMap.add u_v var dico)
                DicoVarMap.empty vars;
    var_next = Array.length vars}


module VarSet = GenericSet.Make(struct type t = var let get_hash = fst end)
module VarMap = GenericMap.Make(struct type t = var let get_hash = fst end)

type exception_handler = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : class_name option;
  e_catch_var : var
}

let generic_exception_edges code exc_tbl = 
  JUtil.foldi 
    (fun i _ l ->
       List.rev_append
	 (List.map 
	    (fun e -> (i,e))
	    (List.filter (fun e -> e.e_start <= i && i < e.e_end) exc_tbl))
	 l)
    [] 
    code

let print_handler exc = 
  Printf.sprintf "      [%d-%d] -> %d (%s %s)" exc.e_start exc.e_end exc.e_handler
    (match exc.e_catch_type with
       | None -> "_"
       | Some cl -> JPrint.class_name cl)
    (var_name_g exc.e_catch_var)  

(* Basic types *)


(* This is the same type as JCode.jconst except that we now consider byte and
* short as integer (as they are coded on 32 bits.).*)
type const = [
| `ANull
| `Int of int32
| `Long of int64
| `Float of float
| `Double of float
| `String of JBasics.jstr
| `Class of JBasics.object_type
]


type conv = I2L | I2F | I2D   | L2I | L2F | L2D   | F2I | F2L | F2D  | D2I | D2L | D2F  | I2B | I2C | I2S

type unop =
  | Neg of jvm_basic_type
  | Conv of conv
  | ArrayLength
  | InstanceOf of JBasics.object_type
  | Cast of JBasics.object_type

type comp =  DG | DL | FG | FL | L

type binop =
  | ArrayLoad of JBasics.value_type
  | Add of JBasics.jvm_basic_type
  | Sub of JBasics.jvm_basic_type
  | Mult of JBasics.jvm_basic_type
  | Div of JBasics.jvm_basic_type
  | Rem of JBasics.jvm_basic_type
  | IShl | IShr  | IAnd | IOr  | IXor | IUshr
  | LShl | LShr | LAnd | LOr | LXor | LUshr
  | CMP of comp

type virtual_call_kind =
  | VirtualCall of object_type
  | InterfaceCall of class_name
      
(* Type transformation *)

let basic_to_num = function
  | `Int2Bool -> `Int
  | `Long -> `Long
  | `Double -> `Double
  | `Float -> `Float


let jconst2const = function
  | `ANull -> `ANull
  | `Long c -> `Long c
  | `Float c ->  `Float c
  | `Double c -> `Double c
  | `Byte c
  | `Short c  -> `Int (Int32.of_int c)
  | `Int c -> `Int c
  | `String c -> `String c
  | `Class c -> `Class c


(* Printing functions *)

let print_const = function
  | `ANull -> "null"
  | `Int i -> Printf.sprintf "%ld" i
  | `Long i -> Printf.sprintf "%Ld" i
  | `Float f -> Printf.sprintf "%f" f
  | `Double f -> Printf.sprintf "%f" f
  | `Byte n -> Printf.sprintf "%d" n
  | `Short a -> Printf.sprintf "%d " a
  | `String s -> Printf.sprintf "'%s'" (jstr_pp s)
  | `Class c -> Printf.sprintf "%s" (JDumpBasics.object_value_signature c)

let print_unop = function
  | Neg t -> Printf.sprintf "%cNeg" (JDumpBasics.jvm_basic_type t)
  | Conv conv ->
      begin
	match conv with
	  | I2L -> "I2L"  | I2F -> "I2F"  | I2D -> "I2D"
	  | L2I -> "L2I"  | L2F -> "L2F"  | L2D -> "L2D"
	  | F2I -> "F2I"  | F2L -> "F2L"  | F2D -> "F2D"
	  | D2I -> "D2I"  | D2L -> "D2L"  | D2F -> "D2F"
	  | I2B -> "I2B"  | I2C -> "I2C"  | I2S -> "I2S"
      end
  | ArrayLength -> "ArrayLength"
  | InstanceOf ot -> Printf.sprintf "InstanceOf %s" (Javalib.JPrint.object_type ot)
  | Cast _ -> assert false

let print_binop = function
  | ArrayLoad _ -> Printf.sprintf "ArrayLoad"
  | Add t -> Printf.sprintf "%cAdd" (JDumpBasics.jvm_basic_type t)
  | Sub t -> Printf.sprintf "%cSub" (JDumpBasics.jvm_basic_type t)
  | Mult t -> Printf.sprintf "%cMult" (JDumpBasics.jvm_basic_type t)
  | Div t -> Printf.sprintf "%cDiv" (JDumpBasics.jvm_basic_type t)
  | Rem t -> Printf.sprintf "%cRem" (JDumpBasics.jvm_basic_type t)
  | IShl -> "IShl"  | IShr -> "IShr"  | LShl -> "LShl"
  | LShr -> "LShr"  | IAnd -> "And"  | IOr -> "IOr"
  | IXor -> "IXor"  | IUshr -> "IUshr"  | LAnd -> "LAnd"
  | LOr -> "LOr"  | LXor -> "LXor"  | LUshr -> "LUshr"
  | CMP c -> Printf.sprintf "CMP %s"
      (match c with
	   DG -> "DG"
	 | DL -> "DL"
	 | FG -> "FG"
	 | FL -> "FL"
	 | L -> "L"
      )

let print_typ t =
  let bt2ss = function
    | `Long -> "J"
    | `Float -> "F"
    | `Double -> "D"
    | `Int -> "I"
    | `Short -> "S"
    | `Char -> "C"
    | `Byte -> "B"
    | `Bool -> "Z"
  in
  let rec ot2ss = function
    | TClass c -> cn_name c
    | TArray t -> "["^ vt2ss t
  and vt2ss = function
    | TBasic t -> bt2ss t
    | TObject t -> ot2ss t
  in vt2ss t


  type phi_node = {
    def : var;
    use : var array;
  }

  let print_phi_node ?(phi_simpl=true) phi =
    if phi_simpl
    then
      Printf.sprintf "%s := PHI{%s}"
	(var_name_g phi.def)
	(JUtil.print_list_sep
	   "," var_name_g (VarSet.elements (VarSet.of_array phi.use)))
    else
      Printf.sprintf "%s := PHI(%s)"
	(var_name_g phi.def)
	(JUtil.print_list_sep 
	   "," var_name_g (Array.to_list phi.use))
	
  let print_phi_nodes ?(phi_simpl=true) l =
    JUtil.print_list_sep "; " (print_phi_node ~phi_simpl:phi_simpl) l

  let app_phi_nodes phi_simpl pc preds l acc=
    if l = [] then acc
    else 
      let head = 
	if phi_simpl
	then
	  ""
	else
	  Printf.sprintf
	    "%3s (preds(%d) := (%s))"
	    ""
	    pc
	    (JUtil.print_list_sep 
	       ", " string_of_int (Array.to_list preds))
      in
	head
	::(List.fold_right
	     (fun phi nacc -> 
		(Printf.sprintf
		   "%4s %s;"
		   ""
		   (print_phi_node ~phi_simpl:phi_simpl phi))::nacc)
	     l
	     acc)


exception No_memory_ssa_info_here
type memory_ssa_info = {
  mem_ssa_in: int -> int;
  mem_ssa_out: int -> int;
  mem_ssa_phi: int -> int * int array
}

