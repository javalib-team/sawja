(*
 * This file is part of SAWJA
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
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

(** Defines high level Ocaml representation of a java byte-code program. *)

open JBasics
open Javalib

(** {2 Maps and sets.} *)

module ClassMethMap : Map.S with type key = class_name * method_signature
module ClassMethSet : Set.S with type elt = class_name * method_signature

(** {2 Navigable hierarchy.} *)

(** To be able to navigate more easily in byte-code program, class
    names are replaced by pointers to class_file structures ( for
    super_class, implemented interfaces and sub-classes or
    sub-interfaces).
*)

type 'a class_node = private {
  c_info : 'a jclass;
  c_super : 'a class_node option;
  c_interfaces : 'a interface_node ClassMap.t;
  mutable c_children : 'a class_node list;
}
and 'a interface_node = private {
  i_info : 'a jinterface;
  i_super : 'a class_node;
  (** must be java.lang.Object. But note that interfaces are not
      considered as children of java.lang.Object.*)
  i_interfaces : 'a interface_node ClassMap.t;
  mutable i_children_interfaces : 'a interface_node list;
  mutable i_children_classes : 'a class_node list
}
and 'a node =
  | Interface of 'a interface_node
  | Class of 'a class_node

(** [make_class_node c super interfaces] builds a class node given a jclass
    [c], a super class node [super] and a map of implemented interfaces
    [interfaces]. This function adds the class [c] to the children of [super]
    and to the children classes of each interface in [interfaces]. *)
val make_class_node : 'a jclass -> 'a class_node option
  -> 'a interface_node ClassMap.t -> 'a class_node

(** [make_interface_node i super interfaces] builds an interface node given
    a jinterface [i], a super class node [c] (which is always [java.lang.Object])
    and a map of super interfaces [interfaces]. This function adds the interface
    [i] to the children interfaces of each interface in [interfaces]. *)
val make_interface_node : 'a jinterface -> 'a class_node
  -> 'a interface_node ClassMap.t -> 'a interface_node

(** {2 The [program] structure.} *)

type invoke =
    [ `Interface of JBasics.class_name
    | `Special of JBasics.class_name
    | `Static of JBasics.class_name
    | `Virtual of JBasics.object_type ] * JBasics.method_signature

val invoke_cnms : invoke -> class_name * method_signature

type 'a static_lookup_method = class_name -> method_signature -> invoke ->
  ClassMethodSet.t

(** A program is a record containing a map of class files identified by
    an id, and a dictionary containing functions to retrieve classes and
    methods ids from their names. *)
(* TODO: update the documentation *)
type 'a program = { classes : 'a node ClassMap.t;
		    parsed_methods : ('a node *
					'a concrete_method) ClassMethodMap.t;
		    static_lookup_method : 'a static_lookup_method
                      (** [static_lookup_method cni msi pc] returns the set of
			  methods that may be called from the program point
			  identified by [(cni,msi,pc)]. *) }

(** @raise Sys_error if the file could not be opened. *)
val load_program : string -> 'a program
val store_program : string -> 'a program -> unit

(** {2 Iterators.}*)

val iter : ('a node -> unit) -> 'a program -> unit
val fold : ('b -> 'a node -> 'b) -> 'b -> 'a program -> 'b

(** {2 Classes access functions.}*)

(** [get_node p cn] returns the class named [cn] in
    program [p], if any.
    @raise Not_found if [p] does not contain a class named [cn].
*)
val get_node : 'a program -> class_name -> 'a node

val get_name : 'a node -> class_name
val get_consts : 'a node -> constant array
val get_interfaces : 'a node -> 'a interface_node ClassMap.t

val get_all_children_classes : 'a class_node -> 'a class_node list
val i_equal : 'a interface_node -> 'a interface_node -> bool
val c_equal : 'a class_node -> 'a class_node -> bool
val node_equal : 'a node -> 'a node -> bool

val to_jclass : 'a node -> 'a interface_or_class

(** {2 Methods access functions.}*)

val main_signature : method_signature

(** [get_method c ms] returns the method with signature [ms] in class
    [c], if any.
    @raise Not_found if [c] does not contain a method with signature [ms].
*)
val get_method : 'a node -> method_signature -> 'a jmethod
val get_methods : 'a node -> 'a jmethod MethodMap.t
val get_concrete_methods : 'a node -> 'a concrete_method MethodMap.t
val defines_method : 'a node -> method_signature -> bool

(** {2 Fields access functions.}*)

(** [get_field c fs] returns the field with signature [fs] in class
    [c], if any.
    @raise Not_found if [c] does not contain a field with signature [fs].
*)
val get_field : 'a node -> field_signature -> any_field
val get_fields : 'a node -> any_field FieldMap.t
val defines_field : 'a node -> field_signature -> bool

(** {2 Access to the hierarchy} *)

(** [extends_class c1 c2] returns [true] if [c2] is a super-class
    of [c1]. A class extends itself. *)
val extends_class : 'a class_node -> 'a class_node -> bool

(** [extends_interface i1 i2] returns true if [i2] is a
    super-interface of [i1]. An interface extends itself. *)
val extends_interface : 'a interface_node -> 'a interface_node -> bool

(** [extends ioc1 ioc2] returns true if [ioc2] is a
    super-class or super-interface of [ioc1].
    This is a combination of [extends_class] and [extends_interface]
    which take into account that [java.lang.Object] is a super-class of
    any interface. *)
val extends : 'a node -> 'a node -> bool

(** [implements c1 i2] returns true if [i2] is a
    super-interface of [c1]. *)
val implements : 'a class_node -> 'a interface_node -> bool

(** [super_class cn] returns the super class of cn. *)
val super_class : 'a node -> 'a class_node option

(** [implemented_interfaces cn] returns the interfaces implemented
    by [cn], super-classes of [cn], or extended by those
    super-interfaces. *)
val implemented_interfaces : 'a class_node -> 'a interface_node list

(** [super_interfaces iname] returns the explicit and implicit
    super-interfaces of [iname].*)
val super_interfaces : 'a interface_node -> 'a interface_node list

val firstCommonSuperClass : 'a class_node -> 'a class_node -> 'a class_node

(** {2 Building a hierarchy from simple classes.} *)

(** @raise Not_found if a needed super class is not in the given ClassMap. *)
val build_hierarchy : 'a interface_or_class ClassMap.t -> 'a node ClassMap.t

(** {2 Transforming code representation in a program.} *)

(** [map_program f p] lazily applies [f] to all non-native implementations
    (cf. {!Javalib.implementation}) of the program [p]. [map_program] passes to
    [f] the class name and method signature corresponding to the method being
    converted.  The application is {b lazy}: [f] is not applied until
    [Lazy.force] is called on the implementation. *)
val map_program :
  (class_name -> method_signature -> 'a -> 'b) -> 'a program -> 'b program

(** [map_program2 f p] is similar to [map_program] but instead of the class name
    and method signature, [f] receives the actual class node and concrete method
    being converted.  The implementation of [map_program] is based on
    [map_program2]. *)
val map_program2 :
  ('a node -> 'a concrete_method -> 'a -> 'b) -> 'a program -> 'b program

(** {2 Callgraph.} *)

type callgraph = ((class_name * method_signature * int)
		  * (class_name * method_signature)) list

val get_callgraph : JCode.jcode program -> callgraph

val store_callgraph : callgraph -> string -> unit

(** {2 Exceptions.} *)

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception IncompatibleClassChangeError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoSuchMethodError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoSuchFieldError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoClassDefFoundError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception AbstractMethodError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception IllegalAccessError

exception Invoke_not_found of (class_name * method_signature
			       * class_name * method_signature)
