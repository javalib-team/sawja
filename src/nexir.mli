open Javalib_pack
(*open Sawja_pack*)
open JBasics
open A3Bir


(* le code ci-dessous provient de asite.ml *)
type non_virtual_method_call = 
  | CallStatic of class_method_signature
  | CallSpecial of class_method_signature

type virtual_method_call = 
  | CallVirtual of class_method_signature
  | CallInterface of class_method_signature

(* fin du code provenant de asite.ml *)		       
type return_kind =
  | NormalReturn
  | ExceptionalReturn		     
       
type pc = int

module Formula : sig

(*  type expr =
    | Const of A3Bir.const
    | Var of A3Bir.tvar
    | GetStaticField of JBasics.class_field_signature
    | GetField of expr * JBasics.class_field_signature
    (*    | ArrayLoad of expr * expr*)
    | Unop of unop * expr
    | Binop of A3Bir.binop * expr * expr*)

  type condition =
    | BinCond of [ `Eq | `Le | `Lt | `Ne | `Gt | `Ge] *  expr *  expr
    | IsInstanceOf of expr * object_type
    | IsNotInstanceOf of expr * object_type (* TODO: add comments and take care of null *)
    | IsArrayElementInstanceOf of expr * expr (** (element variable, array variable) *)
    | IsNotArrayElementInstanceOf of expr * expr (** (element variable, array variable) *)

    type formula = 
    | Atom of condition
    | BoolVar of tvar
    | And of formula * formula
    | Or of formula * formula

        
  type check = 
    | CheckNullPointer of A3Bir.tvar
    | CheckArrayStore of A3Bir.tvar * A3Bir.tvar 
    | CheckNegativeArraySize of A3Bir.tvar
    | CheckCast of A3Bir.tvar * JBasics.object_type 
    | CheckArithmetic of A3Bir.tvar 
    | CheckArrayLowerBound of A3Bir.tvar * A3Bir.tvar
    | CheckArrayUpperBound of A3Bir.tvar * A3Bir.tvar

				    
  type should_be_verified =
    | False
    | ToBeChecked of check
    | MethodAssertion of class_method_signature
end
		   
open Formula	    
type instrCfg =
  | Nop
  | AssignVar of A3Bir.var * expr
      (*r [AssignVar (x,e)] assigns var [x] with the result of 
	  the evaluation of expression [e] *) 
  | AffectField of tvar * Javalib_pack.JBasics.class_name * Javalib_pack.JBasics.field_signature * tvar
  | AffectStaticField of Javalib_pack.JBasics.class_name * Javalib_pack.JBasics.field_signature * tvar
  | ArrayStore of A3Bir.tvar * A3Bir.tvar * A3Bir.tvar 
  | Assume of Formula.formula  * should_be_verified
  | AllocVar of A3Bir.var * JBasics.class_name * JBasics.value_type list * A3Bir.tvar list
  | AllocArray of A3Bir.var * JBasics.value_type * A3Bir.tvar list
  | CheckLink of Javalib_pack.JCode.jopcode
  | MayInit of Javalib_pack.JBasics.class_name
  | MonitorEnter of A3Bir.tvar
  | MonitorExit of A3Bir.tvar
  | NonVirtualCall of A3Bir.var * A3Bir.var * non_virtual_method_call * A3Bir.tvar list
   (* [NonVirtualCall e  r cms args] performs a method call to
	the method found in [cms], using arguments [args]. 
        If themethod ends with an exception, this exception is put in [e], 
        otherwise if it ends normally, the result is put in [r].  
        If the methods does not return any value
	because its return type is simply [void] then we assign
	[r] to [null].  *)
  | VirtualCall of A3Bir.var * A3Bir.var * A3Bir.tvar * virtual_method_call * A3Bir.tvar list
      (*r [VirtualCall e r x cn ms args] performs a virtual
	method call using the class of the reference in [x] and the
	method signature [(cn,ms)].  The method lookup may fail
	because of interfaces.  *)
  | UncaughtExceptionAfterMethodCall
      (* r [UncaughtExceptionAfterMethodCall x] special trick for exceptions thrown
	 by a method call *)
  | NormalReturnAfterMethodCall
      (* r [NormalReturnAfterMethodCall x] special trick for exceptions thrown
	   by a method call *)
(* LG :   | StubOf of stubs   *)

	    
type t = {
  params : tvar list;
  code : (instrCfg*pc) list array;
  start_pc : pc;
  normal_return : var ;
  normal_end_pc : pc;      (** where each edge that model normal returns should go *)
  exceptional_return : var;
  exceptional_end_pc : pc; (** where each edge that model uncaught exceptions should go *)
  error_pc : pc;           (** where each edge that model errors should go *)
  cfg2Bir : pc Ptmap.t; (** Map which for a cfg pc give its original pc in
    A3Bir. A pc might not be found if it was not present at A3Bir level. *) 
  (* Some properties:
      -- [ code.(normal_end_pc) = [] ]
      -- for every [pc1], [pc2] such that [code.(pc)] contains [(Return NormalReturn,pc2)],
         [pc2] must be equal to [normal_end_pc]
   *)
}
       

(*val transform : A3Bir.t JProgram.program -> A3Bir.t Javalib.concrete_method -> A3Bir.t -> t 
 (*val transform : A3Bir.t JProgram.program -> Javalib_pack.JBasics.method_signature -> A3Bir.t -> t *)*)


val transform : A3Bir.t -> t 

val cfg2dot : string ->  JBasics.class_method_signature -> t -> unit


