(*
 * This file is part of SAWJA
 * Copyright (c)2011 Vincent Monfort (INRIA)
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

(** ArgPlugin is a wrapper to module Arg that normalize arguments for
    the Sawja Eclipse Plugin.*)

(** ArgPlugin is a wrapper to module Arg that normalize arguments for
    the Sawja Eclipse Plugin. The final executable must use
    ArgPlugin to could be "installed" and executed by the Sawja
    Eclipse Plugin.*)

(** The concrete type describing the behavior associated with a keyword*)
type spec = 
  | ClassPath of (string -> unit)
      (** [ClassPath f] with [f] the function to retrieve the classpath
	  argument. In Eclipse, by default, ClassPath will be automatically put to the
	  complete class path of the Java project.*)
  | Path of (string -> unit)
      (** [Path f] with [f] the function to retrieve the path
	  argument. Path should represents a path to a folder or file.*)
  | ClassFiles of (string list -> unit)
      (** [ClassFiles f] with [f] the function to retrieve the class
	  name list argument. In Eclipse, by default, ClassFiles will
	  automatically contain all the classes files of the Java project.*)
  | ClassFile of (string -> unit)
      (** [ClassFile f] with [f] the function to retrieve the class name
	  argument. In Eclipse this argument must be given by user in
	  properties of analysis for each Java project.*)
  | Choice of ((string list) * (string -> unit) * string)
      (** [Choice (list, f, default)] provides an exclusive choice on
	  a list of symbols, [list] is the list of symbol, [f] the
	  function to retrieve the choosen symbol and [def] the
	  default choice. The character ';' is forbidden for the
	  symbols. raise Invalid_argument, on {!parse} execution, if [def] is not a symbol
	  of [list].  *)
  | String of ((string -> unit) * string option)
      (** [String (f, default)] with [f] function to retrieve string
	  argument and [default] the optional default string value
	  (default value is only used in Eclipse, for executable it's
	  only a description)*)
  | Boolean of ((bool -> unit) * bool option)
      (** [Boolean (f, default)] with [f] function to retrieve boolean
	  argument and [default] the optional default boolean value
	  (default value is only used in Eclipse, for executable it's
	  only a description)*)
  | NotPlugin of Arg.spec
      (** [NotPlugin spec] with [spec] an Arg.spec will add an argument
	  that will not appear in the Sawja Eclipse Plugin.*)

(** key is the option keyword, it must start with a '-' character*)
type key = string 

(** name is the short description for this option in Eclipse.*)
type name = string

(** doc is a one-line description of the corresponding option.*)
type doc = string 

type usage_msg = string 

(** Description of the analysis (name, short description)*)
type analysis = string * string

(** Output path of the data generated for the plugin by
    {!JPrintPlugin.NewCodePrinter.PluginPrinter.print_class} or
    {!JPrintPlugin.NewCodePrinter.PluginPrinter.print_program}*)
type plugin_output = 
    PluginOutput of (string * (string -> unit))
      (** [PluginOutput (key, f)] with key the argument name for this
	  argument and f the function to retrieve the string
	  description of the output path*)

(** [parse analysis args_list plugin_output usage_msg] is an adapted
    version of Arg.parse, it adds a an argument "--argumentsXMLDesc"
    that describe arguments in a XML format. 

    It parses the command line. [analysis] is the analysis
    description. [args_list] is a list of triples (key, spec,
    doc). [plugin_output] is the folder in which information data for
    the plugin must be generated with {!JPrintPlugin} module.
*)
val parse : analysis -> (key * name * spec * doc) list -> plugin_output -> usage_msg -> unit

(** [transform2arg spec] transforms an {!spec} to the Arg version.*)
val transform2arg : spec -> Arg.spec


