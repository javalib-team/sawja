(*
 * This file is part of Jaws
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


(** Builds high level representations of java byte-code programs using
    a refinement of RTA. *)

open JBasics
open Javalib
open JProgram

(** This analysis starts from rta instantiated classes to refine the set
    of parsed methods by taking into account some restrictions on types
    that occur when resolving virtual and interface calls.

    Indeed, when issuing a virtual or interface call inside a calling method,
    the dynamic type of the called class is restricted by many factors :

    - the type of the parameters of the calling method
    - the classes instantiations that occur in the calling method
    - the classes coming from getfield and getstatic instructions
    - the classes coming from the return types of invoked methods

    It's important to notice that our algorithm is flow insensitive.
    Indeed, we first parse all the calling method instructions to build
    a local set of instantiated classes by applying the former
    restrictions to the set of instantiated classes calculated by RTA.
    It's important to say that this set will never propagate.

    Then we resolve the virtual and interface calls inside the calling method
    relying on this set. As the set of resolved methods will be smaller (or equal)
    than the one calculated by RTA, we will potentially parse less methods and
    get more precise on the callgraph.
*)

(** [parse_program ~other_entrypoints classpath (cs,ms)] first returns a
    [program] composed of all the code found in [classpath] and that
    may be accessible from at least one method of
    [(cs,ms)::entrypoints].  [classpath] is a list of directories and
    [.jar] files separated with ':'.  If [entrypoints] is not
    specified, the default methods are the methods invoked natively by
    the JVM during its initialization. (cf {!default_entrypoints}).
    The second return parameter is the set of instantiated classes
    calculated by RTA algorithm. *)
val parse_program :
  ?other_entrypoints:(class_name * method_signature) list ->
  string -> class_name * method_signature ->
  JCode.jcode program * JCode.jcode class_node ClassMap.t

(** Sun's JVM calls some methods natively during the JVM
    initialization.  We have included the list (that we suppose
    complete but without guarantee). Some of the method listed may not
    exists (as <clinit> method are optionals) but there should be
    executed in this order. *)
val default_entrypoints : (class_name * method_signature) list
