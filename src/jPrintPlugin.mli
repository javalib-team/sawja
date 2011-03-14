
(** Printer for Eclipse plugin for SAWJA *)

open Javalib_pack
open JBasics
open Javalib
open JProgram


(** This module allows to generate information data for the Eclipse
    plugin for SAWJA. It allows to add warning on source code in the
    JDT (Java Development Toolkit) of Eclipse and to attach
    information on the analysis state on the code in order to help
    Java programmer or for debugging purpose on analysis.*)

 
(** {2 Program information.} *)



(** This module is an adapted and simplified version of
    org.eclipse.jdt.core.dom.AST grammar, it allows to produce
    detailed warnings in order to try to find the exact node concerned
    in Java source code*)
module AdaptedASTGrammar :
sig
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
	(** Corresponds to assignement instructions ('*store' (except
	    array), 'put*field').Identifier must be the identifier of
	    the left_side of assignment (field's name or variable's
	    name)*)
    | ClassInstanceCreation of class_name
	(** Corresponds to a 'new' instruction and <init> method calls*)
    | ArrayCreation of value_type
	(** Corresponds to '*newarray' instructions *)
    | MethodInvocation of class_method_signature 
	(** Corresponds to 'invoke*' instructions*)
    | ArrayAccess of value_type 
	(** Corresponds to 'arrayload' instructions with type of array*)
    | ArrayStore of value_type 
	(** Corresponds to 'arraystore' instructions with type of
	    array, only difference with ArrayAccess is that it will be
	    searched only in left_side of assignements*)
	(*| InfixExpression of infix_operator (* ? => no because we do not know if it appears in source ...*)*)
    | InstanceOf of identifier option
	(** Corresponds to 'instanceof' instructions*)
    | Cast of identifier
	(** Corresponds to 'checkcast' instructions*)
  type statement = 
      If
	(** Corresponds to 'if+goto' instructionsIncludes all If 'like' statements (If, For, While, ConditionnalExpr, etc.) *)
    | Catch of identifier (*type given by handlers table*)
	(** Corresponds to handlers entrypoints with a filtered type of exception (not finally clause) *)
    | Finally 
	(** Corresponds to a finally handlers entrypoints *)
    | Switch 
	(** Corresponds to 'tableswitch' and 'lookupswitch' instructions*)
    | Synchronized of bool
	(** Corresponds to 'monitor*' instructions, with true value
	    for 'monitorenter' and false for 'monitorexit'*)
    | Return
	(** Corresponds to 'return' instruction.*)
    | Throw
	(** Corresponds to 'throw' instruction.*)
	(*| AssertStat (*How to find them in bytecode: creation of a field
	  in class + creation of exception to throw*)*)
  type node_unit = 
      Statement of statement 
    | Expression of expression 
    | Name of identifier

end

(** Information on a method signature*)
type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string

(** Warning on a program point, 2 types are allowed*)
type warning_pp = 
    LineWarning of string
      (**warning description *)
  | PreciseLineWarning of string * AdaptedASTGrammar.node_unit 
      (** same as LineWarning * AST information *)

(** This type represents warnings and information that will
    be displayed with the Java source code. *)
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

(** {2 Building a Printer for any program representation.} *)


module type PrintInterface =
sig

  type instr
  type code

  (** [get_source_line_number pc code] returns the source line number corresponding the program point pp of the method code m.*)
  val get_source_line_number : int -> code -> int option

  (*  [iter_code f code] iter on code and apply [f] on [pc instr_list]. *)
  (*val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit*)

  (** instr -> display * line*)
  val inst_disp : int -> code -> string

  (** Function to provide in order to display the source variable
      names in the method signatures. *)
  val method_param_names : code Javalib.interface_or_class -> method_signature
    -> string list option

  (** Allows to construct detailed warning but it requires good
      knowledge of org.eclipse.jdt.core.dom.AST representation. See
      existant implementation of to_plugin_warning or simply return the same Ptmap.t.
*)    
  val to_plugin_warning : code jmethod ->  warning_pp list Ptmap.t 
    -> warning_pp list Ptmap.t

end

module type PluginPrinter =
sig
  type code

  (** [print_class info ioc outputdir] generates plugin's
      information files for the interface or class [ioc] in the output
      directory [outputdir], given the plugin's information [info].
      @raise Invalid_argument if the name corresponding to [outputdir]
      is a file.*)
  val print_class: plugin_info -> code interface_or_class -> string -> unit

  (** [print_program info program outputdir] generates plugin's
      information files for the program [p] in the output directory
      [outputdir], given the plugin's information [info].  @raise
      Invalid_argument if the name corresponding to [outputdir] is a
      file. *)
  val print_program: plugin_info -> code program -> string -> unit
    
end

module Make (S : PrintInterface) : PluginPrinter

(** {2 Built printers for Sawja program representations.} *)

module JCodePrinter : PluginPrinter with type code = JCode.jcode

module JBirPrinter : PluginPrinter with type code = JBir.t

module A3BirPrinter : PluginPrinter with type code = A3Bir.t

module JBirSSAPrinter : PluginPrinter with type code = JBirSSA.t

module A3BirSSAPrinter : PluginPrinter with type code = A3BirSSA.t

