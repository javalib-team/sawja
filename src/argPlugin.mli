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

(** ArgPlugin is a wrapper for the module Arg that normalizes arguments for
    the Sawja Eclipse Plugin. The final executable must use
    ArgPlugin in order to be "installed" and executed by the Sawja
    Eclipse Plugin.*)

(** The concrete type describing the behavior associated with a keyword*)
type spec = 
  | ClassPath of (string -> unit)
      (** [ClassPath f] with [f] the function used to retrieve the classpath
        argument. In Eclipse, by default, this argument will be automatically
        set to the complete class path of the Java project.*)
  | Path of (string -> unit)
      (** [Path f] with [f] the function used to retrieve the path argument. The
        argument to [f] should represent a path to a folder or file.*)
  | ClassFiles of (string list -> unit)
      (** [ClassFiles f] with [f] the function to used retrieve the class name
        list argument. In Eclipse, by default, this argument will automatically
        contain all the class files of the Java project.*)
  | ClassFile of (string -> unit)
      (** [ClassFile f] with [f] the function used to retrieve the class name
        argument. In Eclipse this argument must be given by the user as a
        property of the analysis for each Java project.*)
  | Choice of ((string list) * (string -> unit) * string)
      (** [Choice (list, f, default)] provides an exclusive choice on a list of
        symbols: [list] is the list of symbols, [f] the function used to
        retrieve the chosen symbol, and [def] the default choice. The character
        ';' is forbidden in the symbols. Raises Invalid_argument on execution of
        {!parse} if [def] is not a symbol of [list].  *)
  | String of ((string -> unit) * string option)
      (** [String (f, default)] with [f] the function used to retrieve a string
        argument, and [default] the optional default string value (default
        values are only used in Eclipse: for executables this parameter is
        considered as a description)*)
  | Boolean of ((bool -> unit) * bool option)
      (** [Boolean (f, default)] with [f] the function used to retrieve a
        boolean argument and [default] the optional default boolean value
        (default values are only used in Eclipse: for executables this parameter
        is considered as a description)*)
  | NotPlugin of Arg.spec
      (** [NotPlugin spec] with [spec] an Arg.spec adds an argument unrelated to
        the Sawja Eclipse Plugin.*)

(** key is the option keyword, it must start with a '-' character*)
type key = string 

(** name is the short description for this option in Eclipse.*)
type name = string

(** doc is a one-line description of the corresponding option.*)
type doc = string 

type usage_msg = string 

(** Description of the analysis (name, short description)*)
type analysis = string * string

(** Output path of the data generated for the Java side of the plugin by
  {!JPrintPlugin.NewCodePrinter.PluginPrinter.print_class} or
  {!JPrintPlugin.NewCodePrinter.PluginPrinter.print_program}*)
type plugin_output = 
    PluginOutput of (string * (string -> unit))
      (** [PluginOutput (key, f)] with key the argument name for this
	  argument and f the function used to retrieve the string
	  description of the output path*)

(** 
  [parse analysis args_list plugin_output usage_msg] is an adapted version of
  Arg.parse; it automatically generates a an additional parameter
  "--argumentsXMLDesc" that describes the program's arguments in a XML format. 
  [analysis] is the analysis description. [args_list] is a list of triples (key,
  spec, doc). [plugin_output] is the folder in which information data for the
  plugin can be found (cf. the {!JPrintPlugin} module).
  *)
val parse : analysis -> (key * name * spec * doc) list -> plugin_output -> usage_msg -> unit

(** [transform2arg spec] transforms an {!spec} into the corresponding Arg datatype.*)
val transform2arg : spec -> Arg.spec


