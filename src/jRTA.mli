(*
 * This file is part of SAWJA
 * Copyright (c)2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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

(** Builds high level representations of Java bytecode programs using Rapid Type
    Analysis algorithm. *)

open Javalib_pack
open JBasics
open Javalib
open JProgram

(** [parse_program ~instantiated ~other_entrypoints classpath cms]
    first returns a [program] composed of all the code found in
    [classpath] and that may be accessible from at least one method of
    [cms::other_entrypoints]. [classpath] is a list of directories
    and [.jar] or [.zip] files separated with ':' (or ';' under
    Windows).  [cms] is the
    {!Javalib_pack.JBasics.class_method_signature} that represents the
    main entrypoint of the program. If [other_entrypoints] is not
    specified, the default methods are the methods invoked natively by
    the JVM during its initialization. (cf {!default_entrypoints}).
    [instantiated] is the list of classes that may be instantiated
    natively by the JVM, if it is not specified it includes the class
    [java.lang.Class] (always instantiated by JVM) and
    {!default_native_throwable}. The second return parameter is the
    set of instantiated classes calculated by RTA algorithm. *)
val parse_program :
  ?instantiated:class_name list ->
  ?other_entrypoints:class_method_signature list ->
  ?native_stubs:string option ->
  string -> class_method_signature ->
  JCode.jcode program * JCode.jcode class_node ClassMap.t

(** Sun's JVM calls some methods natively during the JVM
    initialization.  We have included the list (that we suppose
    complete but without guarantee). Some of the method listed may not
    exists (as <clinit> method are optionals) but there should be
    executed in this order. *)
val default_entrypoints : class_method_signature list

(** Same thing for J2ME CLDC 1.1 *)
val cldc11_default_entrypoints : class_method_signature list

(** Subclasses of classes RuntimeException and Error that could be
    instantiated and thrown by the JVM (cf. JVM Spec 1.5 §2.16.4).*)
val default_native_throwable : class_name list
