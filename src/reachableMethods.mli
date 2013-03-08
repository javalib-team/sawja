(*
 * This file is part of SAWJA
 * Copyright (c)2010 Laurent Hubert (CNRS)
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

(** ReachabeMethods allows to compute the methods of a program that
    are reachable from a set of entry points (use {!Safe} solver).
    This can be used in order to update the field [parsed_method] of
    {!JProgram.program}.  The implementation is not very efficient,
    but it is simple and rely on the {!Safe} framework. *)

open Javalib_pack

(** [compute_reachable_methods p entry_points] computes the methods of
    a program that are reachable from the set of entry points
    [entry_points] in the program [p].  It assumes that
    [p.parsed_methods] is a correct over-approximation of reachable
    methods, and the result of this function can be used to refine
    [p.parsed_methods].  The implementation is not very efficient, but
    it is simple and rely on the {!Safe} framework. *)
val compute_reachable_methods :
    JCode.jcode JProgram.program -> 
    JBasics.class_method_signature list -> 
    (JCode.jcode JProgram.node * JCode.jcode Javalib.concrete_method) JBasics.ClassMethodMap.t
