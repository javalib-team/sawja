(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
 * Copyright (c)2010 Vincent Monfort (INRIA)
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

(** Transform an original code containing subroutines in an equivalent version without subroutines if it does not contains too complex subroutines (like nested subroutines).*)

open Javalib_pack

(** [inline code] returns [Some (code',assoc)] where [code'] is an equivalent
    version of [code] without subroutines. In case [code] does not
    contain subroutines at all, [code'] is physically equal to
    [code]. If [code] contains too complex subroutines (for example,
    nested subroutines), then [inline code] returns [None].
    [assoc] is an association list between between pairs (new_pp, old_pp)
    of program point that handle invoke instructions  ([new_pp] belongs to [code']
    and [old_pp] belongs to [code]
 *)
val inline : JCode.jcode -> (JCode.jcode * ((int*int) list)) option
