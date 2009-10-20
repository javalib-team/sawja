open JBasics
open JCode


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
      
type comp =  DG | DL | FG | FL | L 

type typ = Ref | Num 

type var =
  | OriginalVar of int * string option  (* register number, name (debug if available) *)
  | TempVar of int * int option
  | BranchVar of int * int
  | BranchVar2 of int * int
      
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

let varname =  "$bcvar"
let tempname =  "$irvar"
let branchvarname =  "$T"
let branchvarname2 =  "$T'"

let var_name_debug = function
  | OriginalVar (_,s) -> s 
  | _ -> None

let var_name = function
  | OriginalVar (j,_) -> Printf.sprintf  "%s%d" varname j
  | TempVar (i,None) -> Printf.sprintf "%s%d" tempname i
  | TempVar (i,Some j) -> Printf.sprintf "%s%d_%d" tempname i j
  | BranchVar (i,j) -> Printf.sprintf "%s%d_%d" branchvarname j i
  | BranchVar2 (i,j) -> Printf.sprintf "%s%d_%d" branchvarname2 j i

let var_name_g x = 
  match var_name_debug x with
    | Some s -> s
    | None -> var_name x

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

let print_typ = function
  | Ref -> "ref"
  | Num -> "num"

(* Tests if two variable expressions denote the same variable *)
(* todo : compare reg number then strings, true is conservative *)
let var_equal tvar svar =
    match tvar, svar with 
      | OriginalVar (n,s1), OriginalVar (m,s2) ->  m = n && s1 = s2
      | x, y  -> x=y

let var_orig = function 
  | OriginalVar _ -> true
  | _ -> false

(* type statistics = { *)
(*     mutable nb_jump_with_non_empty_stacks : int; *)
(*     mutable nb_back_jump_with_non_empty_stacks : int; *)
(*     mutable nb_store_is_var_in_stack : int; *)
(*     mutable nb_incr_is_var_in_stack : int; *)
(*     mutable nb_arraystore_is_array_access_in_stack : int; *)
(*     mutable nb_putfield_is_field_in_stack : int; *)
(*     mutable nb_putstatic_is_static_in_stack : int; *)
(*     mutable nb_method_call_with_modifiable_in_stack : int; *)
(*     mutable nb_store : int; *)
(*     mutable nb_incr : int; *)
(*     mutable nb_putfield : int; *)
(*     mutable nb_arraystore : int; *)
(*     mutable nb_putstatic : int; *)
(*     mutable nb_method_call : int; *)
(*     mutable nb_tempvar : int; *)
(*     mutable nb_tempvar_branch : int; *)
(*     mutable nb_tempvar_removed : int; *)
(*     mutable nb_tempvar_method_effect : int; *)
(*     mutable nb_tempvar_putfield : int; *)
(*     mutable nb_tempvar_arraystore : int; *)
(*     mutable nb_tempvar_side_effect : int; *)
(*     mutable nb_tempvar_flat : int; *)
(*     mutable nb_tempvar_3a : int; *)
(*   } *)
