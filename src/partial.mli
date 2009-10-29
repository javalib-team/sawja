(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
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

(** Partial class hierarchy for fast subclass test *)

type t

(** [make cp l] creates a new partial hierarchy using classpath [cp]
    and adding all class names found in the list [l]. *)
val make : Javalib.class_path -> JBasics.class_name list -> t

(** [add h cn] return an updated partial hierarchy where the class of
    class name [cn] has been added to [h]. An interface name is handled
    as [JBasics.java_lang_object]. *)
val add : t -> JBasics.class_name -> t

exception UnknownClass of JBasics.class_name

(** [is_sub_class h cn1 cn2] tests if [cn1] is a sub class of [cn2]
    in the given partial hierarchy [h]. Raise [UnknownClass cn1] if
    [cn1] has not been added to [h] before (using function [add]). *)
val is_sub_class : t -> JBasics.class_name -> JBasics.class_name -> bool

