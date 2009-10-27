(*
 * This file is part of JavaLib
 * Copyright (c)2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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

(** User interface for using Jaws. *)

open JBasics
open Javalib

(** {2 Definitions of node types.} *)

type 'a class_node = 'a JProgram.class_node = private {
  c_info : 'a Javalib.jclass;
  c_super : 'a class_node option;
  c_interfaces : 'a interface_node JBasics.ClassMap.t;
  mutable c_children : 'a class_node list;
}
and 'a interface_node = 'a JProgram.interface_node = private {
  i_info : 'a Javalib.jinterface;
  i_super : 'a class_node;
  i_interfaces : 'a interface_node JBasics.ClassMap.t;
  mutable i_children_interfaces : 'a interface_node list;
  mutable i_children_classes : 'a class_node list;
}

type 'a node = 'a JProgram.node

val node_equal : 'a node -> 'a node -> bool

(** {2 Accessing nodes content.} *)

(** Accessing classes nodes. *)

val c_info : 'a class_node -> 'a jclass
val c_super : 'a class_node -> 'a class_node option
val c_interfaces : 'a class_node -> 'a interface_node ClassMap.t
val c_children : 'a class_node -> 'a class_node list

(** Accessing interfaces nodes. *)

val i_info : 'a interface_node -> 'a jinterface
val i_super : 'a interface_node -> 'a class_node
val i_interfaces : 'a interface_node -> 'a interface_node ClassMap.t
val i_children_interfaces : 'a interface_node -> 'a interface_node list
val i_children_classes : 'a interface_node -> 'a class_node list

(** {2 Programs.} *)

type 'a program

val classes : 'a program -> 'a node ClassMap.t
val parsed_methods : 'a program -> ('a node * 'a concrete_method) ClassMethodMap.t

val iter : ('a node -> unit) -> 'a program ->  unit
val fold : ('a -> 'b node -> 'a) -> 'a -> 'b program -> 'a
