(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
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

module type VarSig = 
sig
  type var
  val var_equal: var -> var -> bool
  val var_name_debug: var -> string option
  val var_name: var -> string
  val var_name_g: var -> string
  val bc_num: var -> int option
end

(* Variables *)
module Var = 
struct
  type unindexed_var =
    | OriginalVar of int * string option (* register number, name (debug if available) *)
    | TempVar of int
    | CatchVar of int
    | BranchVar of int * int
    | BranchVar2 of int * int

  type var = int * unindexed_var

  let var_equal ((i1,_):var) ((i2,_):var) = i1==i2

  let varname =  "$bcvar"
  let tempname =  "$irvar"
  let branchvarname =  "$T"
  let branchvarname2 =  "$T'"

  let var_name_debug (_,v) =
    match v with
      | OriginalVar (_,s) -> s
      | _ -> None

  let var_name (_,v) = 
    match v with
      | OriginalVar (j,_) -> Printf.sprintf  "%s%d" varname j
      | TempVar i -> Printf.sprintf "%s%d" tempname i
      | CatchVar i -> Printf.sprintf "CatchVar%d" i
      | BranchVar (i,j) -> Printf.sprintf "%s%d_%d" branchvarname j i
      | BranchVar2 (i,j) -> Printf.sprintf "%s%d_%d" branchvarname2 j i
	  
  let var_name_g x =
    match var_name_debug x with
      | Some s -> s
      | None -> var_name x

  let bc_num (_,v) = 
    match v with
      | OriginalVar (j,_) -> Some j
      |  _ -> None

  let index (i,_) = i

  let var_orig  (_,v) = 
    match v with
      | OriginalVar _ -> true
      | _ -> false

  let var_ssa  (_,v) = 
    match v with
      | TempVar _ 
      | CatchVar _ -> true
      | _ -> false

  let compare_originalvar x y = 
    match (x, y) with
      | (OriginalVar (x,_),OriginalVar (y,_)) -> compare x y
      | (x,y) -> compare x y

  module VarMap = Map.Make(struct type t=unindexed_var let compare = compare_originalvar end)

  type dictionary =
      { mutable var_map : var VarMap.t;
	mutable var_next : int }

  let make_dictionary () =
    { var_map = VarMap.empty;
      var_next = 0}

  let make_var (d:dictionary) : unindexed_var -> var =
    function v ->
      try
	VarMap.find v d.var_map
      with Not_found -> 
	let new_v = (d.var_next,v) in
	  d.var_map <- VarMap.add v new_v d.var_map;
	  d.var_next <- 1+ d.var_next;
	  new_v

  let make_array_var d =
    let dummy = (-1,(TempVar (-1))) in
    let t = Array.make d.var_next dummy in
      VarMap.iter (fun _  v -> t.(fst v) <- v) d.var_map;
      t

end

module type ExceptionSig = sig
  type var_e
  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : class_name option;
    e_catch_var : var_e
  }
  val exception_edges': 'a array -> exception_handler list -> (int * exception_handler) list
  val print_handler : exception_handler -> string
end

module Exception (Var_e: VarSig) = struct
  (* Exceptions *)
  type var_e = Var_e.var
  type exception_handler = {
    e_start : int;
    e_end : int;
    e_handler : int;
    e_catch_type : class_name option;
    e_catch_var : Var_e.var
  }

  let exception_edges' code exc_tbl = 
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
      (Var_e.var_name_g exc.e_catch_var)  
end

module ExceptionNormalVar = Exception (Var)

module Common = struct
  (* Basic types *)

  type mode = Normal | Flat | Addr3 

  type const =
      [ `ANull
      | `Byte of int
      | `Class of object_type
      | `Double of float
      | `Float of float
      | `Int of int32
      | `Long of int64
      | `Short of int
      | `String of string ]

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


  (* Type transformation *)

  let basic_to_num = function
    | `Int2Bool -> `Int
    | `Long -> `Long
    | `Double -> `Double
    | `Float -> `Float


  (* Printing functions *)

  let print_const = function
    | `ANull -> "null"
    | `Int i -> Printf.sprintf "%ld" i
    | `Long i -> Printf.sprintf "%Ld" i
    | `Float f -> Printf.sprintf "%f" f
    | `Double f -> Printf.sprintf "%f" f
    | `Byte n -> Printf.sprintf "%d" n
    | `Short a -> Printf.sprintf "%d " a
    | `Class c -> Printf.sprintf "%s" (JDumpBasics.object_value_signature c)
    | `String s -> Printf.sprintf "'%s'" s

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
      | TClass _ -> "O"
      | TArray t -> "["^ vt2ss t
    and vt2ss = function
      | TBasic t -> bt2ss t
      | TObject t -> ot2ss t
    in vt2ss t


  (* Print utilities ... *)

  let rec print_list_sep_rec sep pp = function
    | [] -> ""
    | x::q -> sep^(pp x)^(print_list_sep_rec sep pp q)

  let rec print_list_sep_list_rec sep pp = function
    | [] -> []
    | x::q -> (sep^(pp x))::(print_list_sep_list_rec sep pp q)

  let print_list_sep sep pp = function
    | [] -> ""
    | x::q -> (pp x)^(print_list_sep_rec sep pp q)

  let print_list_sep_list sep pp = function
    | [] -> []
    | x::q -> (pp x)::(print_list_sep_list_rec sep pp q)

  let print_field ?(long_fields=false) c f =
    if long_fields then
      Printf.sprintf "<%s:%s>" (JPrint.class_name c) (fs_name f)
    else (fs_name f)

  let bracket b s =
    if b then s else Printf.sprintf "(%s)" s

end

