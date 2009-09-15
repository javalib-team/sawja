(*
 * This file is part of JavaLib
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
open JClass
open JClassIndexation

(** {2 Maps and Sets.} *)

module ClassSet : Set.S with type elt = class_signature
module MethodSet : Set.S with type elt = method_signature
  
module ClassMethSet : Set.S with type elt = class_signature * method_signature
module ClassMethMap : Map.S with type key = class_signature * method_signature

(** {2 Navigable hierarchy.} *)

(** To be able to navigate more easily in byte-code program, class
    names are replaced by pointers to class_file structures ( for
    super_class, implemented interfaces and sub-classes or
    sub-interfaces).
*)

type 'a class_file = {
  c_info : 'a jclass;
  c_super : 'a class_file option;
  c_interfaces : 'a interface_file ClassMap.t;
  get_c_children : unit -> 'a class_file list;
}
and 'a interface_file = {
  i_info : 'a jinterface;
  i_super : 'a class_file;
  (** must be java.lang.Object. But note that interfaces are not
      considered as children of java.lang.Object.*)
  i_interfaces : 'a interface_file ClassMap.t;
  get_i_children_interfaces : unit -> 'a interface_file list;
  get_i_children_classes : unit -> 'a class_file list
}
and 'a interface_or_class = [ `Interface of 'a interface_file | `Class of 'a class_file ]

(** {2 The [program] structure.} *)

type 'a static_lookup_method = class_signature -> method_signature -> int ->
  ('a class_file * 'a concrete_method) ClassMethodMap.t

(** A program is a record containing a map of class files identified by
    an id, and a dictionary containing functions to retrieve classes and
    methods ids from their names. *)
type 'a program = { classes : 'a interface_or_class ClassMap.t;
		    parsed_methods : ('a interface_or_class *
					'a concrete_method) ClassMethodMap.t;
		    static_lookup_method : 'a static_lookup_method
                      (** [static_lookup_method cni msi pc] returns the set of
			  methods that may be called from the program point
			  identified by [(cni,msi,pc)]. *) }

(** @raise Sys_error if the file could not be opened. *)
val load_program : string -> 'a program
val store_program : string -> 'a program -> unit

(** {2 Iterators.}*)

val iter : ('a interface_or_class -> unit) -> 'a program -> unit
val fold : ('b -> 'a interface_or_class -> 'b) -> 'b -> 'a program -> 'b

(** {2 Classes access functions.}*)

(** [get_interface_or_class p cn] returns the class named [cn] in
    program [p], if any.
    @raise Not_found if [p] does not contain a class named [cn].
*)
val get_interface_or_class : 'a program -> class_signature -> 'a interface_or_class

val get_signature : 'a interface_or_class -> class_signature
val get_name : 'a interface_or_class -> class_name
val get_interfaces : 'a interface_or_class -> 'a interface_file ClassMap.t

val get_all_children_classes : 'a class_file -> 'a class_file list
val equal : 'a interface_or_class -> 'a interface_or_class -> bool

val to_class : 'a interface_or_class -> 'a JClass.interface_or_class

(** {2 Methods access functions.}*)

val main_signature : method_signature

(** [get_method c ms] returns the method with signature [ms] in class
    [c], if any.
    @raise Not_found if [c] does not contain a method with signature [ms].
*)
val get_method : 'a interface_or_class -> method_signature -> 'a jmethod
val get_methods : 'a interface_or_class -> 'a jmethod MethodMap.t
val get_concrete_methods : 'a interface_or_class -> 'a concrete_method MethodMap.t
val defines_method : method_signature -> 'a interface_or_class -> bool

(** {2 Fields access functions.}*)

(** [get_field c fs] returns the field with signature [fs] in class
    [c], if any.
    @raise Not_found if [c] does not contain a field with signature [fs].
*)
val get_field : 'a interface_or_class -> field_signature -> any_field
val get_fields : 'a interface_or_class -> any_field FieldMap.t
val defines_field : field_signature -> 'a interface_or_class -> bool

(** {2 Access to the hierarchy} *)

(** [extends_class c1 c2] returns [true] if [c2] is a super-class
    of [c1]. A class extends itself. *)
val extends_class : 'a class_file -> 'a class_file -> bool

(** [extends_interface i1 i2] returns true if [i2] is a
    super-interface of [i1]. An interface extends itself. *)
val extends_interface : 'a interface_file -> 'a interface_file -> bool

(** [extends ioc1 ioc2] returns true if [ioc2] is a
    super-class or super-interface of [ioc1].
    This is a combination of [extends_class] and [extends_interface]
    which take into account that [java.lang.Object] is a super-class of
    any interface. *)
val extends : 'a interface_or_class -> 'a interface_or_class -> bool

(** [implements c1 i2] returns true if [i2] is a
    super-interface of [c1]. *)
val implements : 'a class_file -> 'a interface_file -> bool

(** [super_class cn] returns the super class of cn. *)
val super_class : 'a interface_or_class -> 'a class_file option

(** [implemented_interfaces cn] returns the interfaces implemented
    by [cn], super-classes of [cn], or extended by those
    super-interfaces. *)
val implemented_interfaces : 'a class_file -> 'a interface_file list

(** [super_interfaces iname] returns the explicit and implicit
    super-interfaces of [iname].*)
val super_interfaces : 'a interface_file -> 'a interface_file list

val firstCommonSuperClass : 'a class_file -> 'a class_file -> 'a class_file

(** {2 Callgraph.} *)

type callgraph = ((class_signature * method_signature * int)
		  * (class_signature * method_signature)) list

val get_callgraph : JOpcodes.lazy_code program -> callgraph

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

exception Invoke_not_found of (class_signature * method_signature
			       * class_signature * method_signature)
