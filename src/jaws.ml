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

open JBasics
open JCode
open Javalib

include JProgram

let c_info cnode = cnode.c_info
let c_super cnode = cnode.c_super
let c_interfaces cnode = cnode.c_interfaces
let c_children cnode = cnode.c_children

let i_info inode = inode.i_info
let i_super inode = inode.i_super
let i_interfaces inode = inode.i_interfaces
let i_children_interfaces inode = inode.i_children_interfaces
let i_children_classes inode = inode.i_children_classes

let classes p = p.classes
let parsed_methods p = p.parsed_methods

