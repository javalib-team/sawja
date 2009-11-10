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


(* TODO: static_lookup_* function have been removed. this was a bad
   idea.*)

(** Allows some navigation in the control flow graph of a program. *)

open JBasics
open Javalib
open JProgram

(** {2 Access to instructions}*)

(** Manipulation of program pointers *)
module PP : sig
  type 'a t
  exception NoCode of (class_name * method_signature)
  val get_class : 'a t -> 'a node
  val get_meth : 'a t -> 'a concrete_method
  val get_pc : 'a t -> int

  val get_pp : 'a node -> 'a concrete_method -> int -> 'a t

  (** [get_first_pp p cn ms] gets a pointer to the first instruction of
      the method [ms] of the class [cn].

      @raise Not_found if [cn] is not a class of [p], or [ms] is not a
      method of [cn].

      @raise NoCode if the method [ms] has no associated code.*)
  val get_first_pp : 'a program -> class_name -> method_signature -> 'a t
  val get_first_pp_wp : 'a node -> method_signature -> 'a t
  val goto_absolute : 'a t -> int -> 'a t
  val goto_relative : 'a t -> int -> 'a t

  val to_string : 'a t -> string
  val pprint : Format.formatter -> 'a t -> unit

  val equal : 'a t -> 'a t -> bool
  val compare : 'a t -> 'a t -> int
  val hash : 'a t -> int
end

(** The type of program point identifier. *)
type pp = JCode.jcode PP.t

val get_opcode : pp -> JCode.jopcode
val next_instruction : pp -> pp
val normal_successors : pp -> pp list
val handlers : JCode.jcode program -> pp -> JCode.exception_handler list
val exceptional_successors : JCode.jcode program -> pp -> pp list


(** {2 Lookup and resolve procedure} *)

(** {b Warning : lookup and resolve functions do not take in account
    visibility yet}! *)


(** [get_class p cn] returns the class named [cn] in program [p], if
    any.
    @raise NoClassDefFoundError if [p] does not contain a class named
    [cn].
*)
val resolve_class : 'a program -> class_name -> 'a node

(** [resolve_method ms c] returns the class or interface that defines
    the method [ms], if any.  The caller is responsible to check that
    the class and the method defined in the class are visible from the
    current class.
    @raise NoSuchMethodError if the method is not found
*)
val resolve_method : method_signature -> 'a class_node -> 'a node
val resolve_method' : method_signature -> 'a class_node -> 'a class_node
  (** only look for the method in the superclasses. *)

(** [mplements_method c ms] returns [true] iff the class has a method with the
    signature [ms] and which is not abstract. (Note: The method can be native.)
*)
val implements_method : 'a class_node -> method_signature -> bool

(** [resolve_interface_method ms c] return the interface that defines
    the method [ms], or [java.lang.Object] if no interface defines
    [ms] but [Object] does.  The caller is responsible to check that
    the interface and the method defined in the interface are visible
    from the current class.
    @raise NoSuchMethodError if the method is not found.
    @raise IncompatibleClassChangeError if [c] is not an interface.
*)
val resolve_interface_method : method_signature -> 'a interface_node -> 'a node

(** [resolve_interface_methods' ms i] looks for the methods [ms] in [i]
    and recursively in its interfaces, stopping at every first
    occurence in each hirarchy. It returns the list of interfaces that
    defines [ms]. *)
val resolve_interface_method' :
  ?acc:'a interface_node list -> method_signature -> 'a node -> 'a interface_node list

(** [resolve_all_interface_methods ms c] return the list of interfaces
    of [c] that defines the method [ms].  The list is ordered by
    increasing distant in the inheritance hierarchy.  The caller is
    responsible to check that the interface and the method defined in
    the interface are visible from the current class.
*)
val resolve_all_interface_methods : method_signature -> 'a interface_node -> 'a interface_node list


(** [resolve_field fsi c] returns the list of classes and interfaces
    that define the field [(c,fsi)], if any.  See the JVM Spec for the
    algorithm.  Several interfaces may define the same field and it is
    not specify which one to take (there is no order relation on
    direct interfaces), so this function returns a list.

    @see <http://java.sun.com/docs/books/jvms/second_edition/html/ConstantPool.doc.html#71685> Field Resolution
*)
val resolve_field : field_signature -> 'a node -> 'a node list

(** [lookup_virtual_method ms c] return the class that defines the
    method [ms], if any.  The caller is responsible to check that the
    class and the method defined in the class are visible from the
    current class.
    @raise AbstractMethodError if the method is not found or if the
    method is abstract.
*)
val lookup_virtual_method : method_signature -> 'a class_node -> 'a class_node

(** [lookup_interface_method ms c] return the class that defines the
    method [ms], if any. The caller is responsible to check that the
    class returned is visible from the current class. As the method is
    supposed to have been declared in a interface (and
    [resolve_interface_method] can ensure that), the method is
    necessarily [public].
    @raise AbstractMethodError if the method is not found or if the
    method is abstract.
*)
val lookup_interface_method : method_signature -> 'a class_node -> 'a class_node

(** [overrides_methods ms c] looks for the classes that define
    methods that are overridden by [(c,ms)] (in the parents of
    [c]). The result list is ordered such that [c1] is before [c2] iff
    [c1] extends [c2].

    @raise Not_found if [ms] cannot be found in [c]
*)
val overrides_methods : method_signature -> 'a class_node -> 'a class_node list

(** [overridden_by_methods ms c] looks for the classes that define
    methods that overrides (or implements) [(c,ms)] (in the children
    of [c]).

    @raise Invalid_argument("overridden_by_methods") if the method is
    a class or instance initialization method.*)
val overridden_by_methods : method_signature -> 'a node -> 'a class_node list

(** [implements_methods ms c] looks for the interfaces that defines
    methods [ms] in the direct interfaces of [c] and recursively in
    their super-interfaces. If [i1] and [i2] defines [ms] and [i1]
    extends [i2], then [i1] is before [i2] in the result list.

    @raise Not_found if [ms] cannot be found in [c]
*)
val implements_methods : method_signature -> 'a class_node -> 'a interface_node list

(** [static_lookup_special program c cs ms] returns the defining class
    of the method that would be reached by executing "invokespecial
    cs.ms" with c as the current class. *)
val static_lookup_special :
  'a program -> 'a node -> class_name -> method_signature -> 'a class_node

(** [static_lookup program pp] returns the highest methods in the hierarchy
    that may be called from program point [pp]. All methods that may be
    called at execution time are known to implement or extend one of
    the class that this function returns. *)
val static_lookup : JCode.jcode program -> pp
  -> (JCode.jcode node list * method_signature) option

(** [static_lookup' program pp] returns a list of methods that may be
    called from program point [pp].  The computation is base on RTA or
    CHA, depending on the function used to build the program. *)
val static_lookup' : JCode.jcode program -> pp -> pp list

(** {2 Interface Implementations.}*)

val implements_interface_or_subinterface : 'a node -> 'a interface_node
  -> bool

val implements_interface_or_subinterface_transitively : 'a class_node ->
  'a interface_node -> bool

(** {2 Invokes lookup algorithms.}*)

(** [invoke_virtual_lookup ?c ms instantiated_classes] returns the result of the
    InvokeVirtual instruction on a class [c] with the method signature [ms],
    given a set of possibly instantiated classes.

    Setting a value to [c] will only check (assert) that each class in
    [instantiated_classes] extends [c].  It raises an Assert_failure if the
    assertion fails and the program has been compiled without deactivating
    assertions. *)
val invoke_virtual_lookup : ?c:('a class_node option) -> method_signature ->
  'a class_node ClassMap.t -> ('a class_node * 'a concrete_method) ClassMethodMap.t

(** [invoke_interface_lookup ?i ms instantiated_classes] returns the result of
    the InvokeInterface instruction on an interface [i] with the method
    signature [ms], given a set of possibly instantiated classes.

    Setting a value to [i] will only check (assert) that each class in
    [instantiated_classes] implements [i] directly or indirectly.  It raises an
    Assert_failure if the assertion fails and the program has been compiled
    without deactivating assertions. *)
val invoke_interface_lookup : ?i:('a interface_node option) -> method_signature ->
  'a class_node ClassMap.t -> ('a class_node * 'a concrete_method) ClassMethodMap.t

(** [invoke_special_lookup current_class c ms] returns the result of
    the InvokeSpecial instruction on a class [c] with the method signature [ms],
    from the class [current_class]. *)
val invoke_special_lookup : 'a node -> 'a class_node ->
  method_signature -> 'a class_node * 'a concrete_method

(** [invoke_static_lookup c ms] returns the result of the InvokeStatic instruction
    on a class [c] with the method signature [ms]. *)
val invoke_static_lookup : 'a class_node -> method_signature ->
  'a class_node * 'a concrete_method


(** [get_successors program class method] returns the possible methods that may
    be invoked from the current program point. For the static initialization,
    only the topmost class initializer is return (and the successors of a clinit
    methods includes the clinit methods that are beneath). *)
val get_successors :
  JCode.jcode program ->
  JCode.jcode node -> JCode.jcode concrete_method -> ClassMethodSet.t
