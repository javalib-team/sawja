(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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

open Javalib_pack

(** [inline code] returns [Some code'] where [code'] is an equivalent
    version of [code] without subroutines. In case [code] does not
    contain subroutines at all, [code'] is physically equal to
    [code]. If [code] contains too complex subroutines (for example,
    nested subroutines), then [inline code] returns [None]. *)
val inline : JCode.jcode -> JCode.jcode option
