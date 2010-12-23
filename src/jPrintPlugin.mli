
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

(** Information on method signature*)
type method_info = 
  | MethodSignature of string
  | Argument of int * string
  | Return of string
  | This of string


type attribute = string * string

(** Warning on program point*)
type warning_pp = 
    (*line * type * short_desc * long_desc *)
    LineWarning of int * string * string * string option
      (* idem + AST information ... *)
  | PreciseWarning of int * string * string * string option * attribute list 

(** Warning on class or field signature*)
type warning_sig = 
    (** type * msg * detailed msg*)
    string * string * string option
      
(** Warning on method signature *)
type warning_meth_sig = 
    (** type * msg * detailed msg*)
    string * method_info * string option


(** This type represents warnings and information that will
    be displayed on with the Java source code. *)
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

(** {2 Building a Printer for any program representation.} *)


module type PrintInterface =
sig

  type instr
  type code

  val iter_code : (int -> instr list -> unit) -> code Lazy.t -> unit

  (** instr -> display * line*)
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

module type PluginPrinter =
sig
  type code

  val print_class: plugin_info -> code interface_or_class -> string -> unit

  val print_program: plugin_info -> code program -> string -> unit
 
end

module Make (S : PrintInterface) : PluginPrinter

(** {2 Built printers for Sawja program representations.} *)

module JCodePrinter : PluginPrinter with type code = JCode.jcode

module JBirPrinter : PluginPrinter with type code = JBir.t

module A3BirPrinter : PluginPrinter with type code = A3Bir.t

module JBirSSAPrinter : PluginPrinter with type code = JBirSSA.t

module A3BirSSAPrinter : PluginPrinter with type code = A3BirSSA.t

