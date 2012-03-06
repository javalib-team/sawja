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

(** Primitives for generating warnings and information for the Sawja Eclipse Plugin *)

open Javalib_pack
open JBasics
open Javalib
open JProgram


(** This module contains primitives that facilitate the creation of informative
  data for the Sawja Eclipse Plugin. It is possible to add warnings on source
  code in the JDT (Java Development Toolkit) of Eclipse and to attach
  information on the state of an analysis to the source code, either to provide
  guidance to the Java programmer, or for analysis debugging purpose.

  To generate this data:

  - Use the printer for the code representation used by your analysis to
  retrieve an empty {!plugin_info} data structure
  ({!NewCodePrinter.PluginPrinter.empty_infos}).

  - Fill the {!plugin_info} data structure with the necessary warnings and
  information (utility functions are provided for this purpose)
    
  - Use the printer for the code representation used by your analysis and print
  the final data structure ({!NewCodePrinter.PluginPrinter.print_class} or
  {!NewCodePrinter.PluginPrinter.print_program}) in the output path given by
  {!ArgPlugin.plugin_output}

  N.B.: Information contained by [p_infos] in {!plugin_info} can be given as
  structured (HTML) code, in which case it must be specified to the print
  function with the [html_info] optional parameter. Conversely, warnings
  contained by [p_warnings] can only be plain text.
*)


(** {2 Data structures for analysis information.} *)

(** Information on a method signature*)
type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string


(** Warning on a program point. The type variable 'a can be used if the warning
  location needs to be more precise than an instruction program point (e.g.:
  {!JBir.expr} for {!JBir} representation), see the specific code representation
  printer for the exact type.*)
type 'a warning_pp = string * 'a option

(** This type represents warnings and information that will be
    displayed in the Java source code. Information will be displayed
    on all class files that have a source file attached but warnings
    can only be displayed on source files of a Java project in the
    Eclipse environment (e.g. warnings on class files of library
    cannot be displayed).*)
type 'a plugin_info = 
    {
      mutable p_infos : 
	(string list 
	 * string list FieldMap.t 
	 * (method_info list * string list Ptmap.t)
	 MethodMap.t) 
	ClassMap.t;
      (** class-related information (one entry in ClassMap.t): (class_infos *
          field_infos FieldMap.t * (method_infos * pc_infos) MethodMap.t*)

      mutable p_warnings : 
	(string list 
	 * string list FieldMap.t 
	 * (method_info list * 'a warning_pp list Ptmap.t)
	 MethodMap.t) 
	ClassMap.t;
      (** class-related warnings (one entry in ClassMap.t): (class_infos *
        field_infos FieldMap.t * (method_infos * pc_infos) MethodMap.t*)
    }

(**{3 Utility functions to fill the {!plugin_info} data structure}*)

(** [iow] (info or warning) is a type compatible with [p_infos] or [p_warnings]
  fields of {!plugin_info}*)
type ('c,'f,'m,'p) iow = ('c list 
			  * 'f list FieldMap.t 
			  * ('m list * 'p list Ptmap.t) MethodMap.t)

(** [add_class_iow iow cn cmap] adds the info or warning [iow] for the class
  [cn] to [cmap].*)
val add_class_iow : 'c -> class_name -> ('c,'f,'m,'p) iow ClassMap.t -> ('c,'f,'m,'p) iow ClassMap.t

(** [add_field_iow iow cn fs cmap] adds the info or warning [iow] for the field
  [fs] of the class [cn] to [cmap].*)
val add_field_iow : 'f -> class_name -> field_signature -> 
  ('c,'f,'m,'p) iow ClassMap.t -> ('c,'f,'m,'p) iow ClassMap.t

(** [add_meth_iow iow cn ms cmap] adds the info or warning [iow] for the method
  [ms] of the class [cn] to [cmap].*)
val add_meth_iow : 'm -> class_name -> method_signature -> 
  ('c,'f,'m,'p) iow ClassMap.t -> ('c,'f,'m,'p) iow ClassMap.t

(** [add_pp_iow iow cn ms pc cmap] adds the info or warning [iow] for the program
  point [pc] in the method [ms] of the class [cn] to [cmap].*)
val add_pp_iow : 'p -> class_name -> method_signature -> int -> 
  ('c,'f,'m,'p) iow ClassMap.t -> ('c,'f,'m,'p) iow ClassMap.t

(** {2 Printers for Sawja program representations.} *)

(** Must be used to create a printer for another code representation
    than those included in Sawja*)
module NewCodePrinter :
sig

  (** This module is an adapted and simplified version of
      org.eclipse.jdt.core.dom.AST grammar, it allows to produce
      detailed warnings in order to try to find the exact node concerned
      in Java source code*)
  module AdaptedASTGrammar :
  sig
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
	  (** Corresponds to assignement instructions ('*store' (except
	      array), 'put*field').Identifier must be the identifier of
	      the left_side of assignment (field's name or variable's
	      name)*)
      | ClassInstanceCreation of class_name
	  (** Corresponds to a 'new' instruction and <init> method calls*)
      | ArrayCreation of value_type
	  (** Corresponds to '*newarray' instructions *)
      | MethodInvocationNonVirtual of class_name * method_signature (* ms ? *)
	  (** Corresponds to 'invokestatic', 'invokespecial' or 'invokeinterface' instructions*)
      | MethodInvocationVirtual of object_type * method_signature 
	  (** Corresponds to 'invokevirtual' instruction only*)
      | ArrayAccess of value_type option
	  (** Corresponds to 'arrayload' instructions with optional exact type of value accessed (not array type!)*)
      | ArrayStore of value_type option
	  (** Corresponds to 'arraystore' instructions with type of
	      array, only difference with ArrayAccess is that it will be
	      searched only in left_side of assignements*)
	  (*| InfixExpression of infix_operator (* ? => no because we do not know if it appears in source ...*)*)
      | InstanceOf of object_type
	  (** Corresponds to 'instanceof' instructions*)
      | Cast of object_type
	  (** Corresponds to 'checkcast' instructions*)
    type statement = 
	If
	  (** Corresponds to 'if+goto' instructionsIncludes all If 'like' statements (If, For, While, ConditionnalExpr, etc.) *)
      | Catch of class_name (*type given by handlers table*)
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

  (** {2 Building a Printer for any program representation.} *)

  (** Precise warning on a program point: [LineWarning of]
      {!warning_pp} is automatically generated from the data structure
      for warnings from {!plugin_info}. This warning could be
      transformed in a [PreciseLineWarning] in the
      {!PrintInterface.to_plugin_warning} function provided for the
      specific code representation.*)
  type 'a precise_warning_pp = 
      LineWarning of 'a warning_pp
	(**warning description * optional precision depending of code
	   representation (used for PreciseLineWarning generation)*)
    | PreciseLineWarning of string * AdaptedASTGrammar.node_unit
	(** same as LineWarning * AST information **)

  (** Module that must be implemented to create a {!PluginPrinter}
      for a new code representation than those included in Sawja.*)
  module type PrintInterface =
  sig
    
    (** The code representation (equivalent of {!Javalib_pack.JCode.jcode}, {!JBir.t}, etc.*)
    type p_code

      (** Type that could be use to provide an information more
	  precise than an instruction for warnings on program points
	  (e.g.: [unit] for {!Javalib_pack.JCode}, {!JBir.expr} for {!JBir}, etc.)*)
    type p_expr

    (** [get_source_line_number pc code] returns the source line number corresponding the program point pp of the method code m.*)
    val get_source_line_number : int -> p_code -> int option

    (** [inst_disp pc code] returns a string representation of the
	instruction [pc] in [code].*)
    val inst_disp : int -> p_code -> string

    (** Function to provide in order to display the source variable
	names in the method signatures. *)
    val method_param_names : p_code Javalib.interface_or_class -> method_signature
      -> string list option

    (** This function should transform a {!precise_warning_pp} from a
	[LineWarning] to a [PreciseLineWarning] but it requires good
	knowledge of org.eclipse.jdt.core.dom.AST representation. See
	existant implementation of to_plugin_warning or simply return
	the same Ptmap.t (in this case the precision is the Java
	source line).  *)
    val to_plugin_warning : p_code jmethod ->  p_expr precise_warning_pp list Ptmap.t 
      -> p_expr precise_warning_pp list Ptmap.t

  end

  (** Final module signature of the Printer, it allows to print
      information for a class or a complete program.*)
  module type PluginPrinter =
  sig

    (** The code representation (equivalent of {!Javalib_pack.JCode.jcode}, {!JBir.t}, etc.*)
    type code
      
    (** Type that could be use to provide an information more precise
	than an instruction for warnings on program points (e.g.:
	[unit] for {!Javalib_pack.JCode}, {!JBir.expr} for {!JBir},
	etc.)*)
    type expr

    (** An empty plugin information data structure*)
    val empty_infos: expr plugin_info

    (** [print_class ?html info ioc outputdir] generates plugin's
	information files for the interface or class [ioc] in the output
	directory [outputdir], given the plugin's information [info]. If
	[html] is given and true then string data in
	{!plugin_info}[.p_infos] (only) must be valid html (between <div>
	tags). @raise Invalid_argument if the name corresponding to
	[outputdir] is a file.*)
    val print_class: ?html_info:bool -> expr plugin_info -> code interface_or_class -> string -> unit

    (** [print_program ?html info program outputdir] generates plugin's
	information files for the program [p] in the output directory
	[outputdir], given the plugin's information [info]. If [html] is
	given and true then string data in {!plugin_info}[.p_infos] (only)
	must be valid html (between <div> tags). @raise Invalid_argument
	if the name corresponding to [outputdir] is a file. *)
    val print_program: ?html_info:bool -> expr plugin_info -> code program -> string -> unit
      
  end

    (** Functor building a printer for the Sawja Eclipse Plugin
    given the {!PrintInterface} for a specific code represenation.*)
  module Make (S : PrintInterface) : PluginPrinter 
    with type code = S.p_code
    and type expr = S.p_expr

end

(** Printer for the {!Javalib_pack.JCode} code representation.*)
module JCodePrinter : NewCodePrinter.PluginPrinter with type code = JCode.jcode and type expr = unit

(*
(** Printer for the {!JBir} code representation.*)
module JBirPrinter : NewCodePrinter.PluginPrinter with type code = JBir.t and type expr = JBir.expr

(** Printer for the {!A3Bir} code representation.*)
module A3BirPrinter : NewCodePrinter.PluginPrinter with type code = A3Bir.t and type expr = A3Bir.expr

(** Printer for the {!JBirSSA} code representation.*)
module JBirSSAPrinter : NewCodePrinter.PluginPrinter with type code = JBirSSA.t and type expr = JBirSSA.expr

(** Printer for the {!A3BirSSA} code representation.*)
module A3BirSSAPrinter : NewCodePrinter.PluginPrinter with type code = A3BirSSA.t and type expr = A3BirSSA.expr
*)
