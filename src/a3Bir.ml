(*
 * This file is part of SAWJA
 * Copyright (c)2009 Delphine Demange (INRIA)
 * Copyright (c)2009 David Pichardie (INRIA)
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


open Javalib_pack
open JBasics
open Javalib
open JCode

include BirA3

(** Concrete method transformation. *) 
let transform ?(bcv=false) ?(ch_link=false) ?(get_formula = false) ?(formula_handler = None) j_m j_code =
  let res = Bir.jcode2bir Bir.Addr3 bcv ch_link false j_m j_code in
  let res =
    if get_formula then 
      (match formula_handler with
         | None -> Bir.GetFormula.run Bir.GetFormula.default_formula res
         | Some fhandler -> Bir.GetFormula.run fhandler res)
    else res
  in bir2a3bir res

