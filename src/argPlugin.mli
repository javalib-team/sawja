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


(** ArgPlugin is a wrapper to module Arg that normalize arguments for Sawja's Eclipse Plugin*)

(** Adapted version of Arg.spec*)
type spec = 
  | ClassPath of (string -> unit)
  | Path of (string -> unit)
  | ClassFile of (string -> unit)
  | ClassFiles of (string list -> unit)
  | String of ((string -> unit) * string option)
      (** [String (f, default)] with f function to retrieve string
	  argument and default the optional default string value (only
	  used for description)*)
  | Boolean of ((bool -> unit) * bool option)
      (** [Boolean (f, default)] with f function to retrieve boolean
	  argument and default the optional default boolean value
	  (only used for description)*)
  | NotPlugin of Arg.spec
      (** [NotPlugin spec] with spec an Arg.spec will add an argument
	  that will not appear in the Sawja's Eclipse plugin.*)

type key = string 

type doc = string 

type usage_msg = string 

(** Description of analysis (name, short description)*)
type analysis = string * string

(** Output path of XML data generated for the
plugin by {!JPrintPlugin.PluginPrinter.print_class} or
{!JPrintPlugin.PluginPrinter.print_program}*)
type plugin_output = 
    PluginOutput of (string * (string -> unit))
      (** [PluginOutput (key, f)] with key the argument name for this
	  argument and f the function to retrieve string description of
	  the path*)

(** [parse analysis args_list plugin_output usage_msg] is an adapted version of
    Arg.parse, it adds a an argument "--argumentsXMLDesc" that
    describe arguments in a XML format*)
val parse : analysis -> (key * spec * doc) list -> plugin_output -> usage_msg -> unit

(** [transform2arg spec] transform an {!spec} to the Arg version of spec.*)
val transform2arg : spec -> Arg.spec


