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

include Bir

type t = bir

let vars m = m.bir_vars
let params m = m.bir_params
let code m = m.bir_code
let exc_tbl m = m.bir_exc_tbl
let line_number_table m = m.bir_line_number_table
(*let pc_bc2ir m = m.bir_pc_bc2ir*)
let pc_ir2bc m = m.bir_pc_ir2bc 

let get_source_line_number pc_ir m = 
  bir_get_source_line_number pc_ir m

let exception_edges = bir_exception_edges 

let print = bir_print 

let jump_target = bir_jump_target

let transform ?(bcv=false) ?(ch_link = false) ?(formula=false) ?(formula_cmd = default_formula_cmd) cm c = 
  let res = jcode2bir Normal bcv ch_link false cm c in
  let res = if formula then Bir.GetFormula.run formula_cmd res else res in
    res
    
let print_class = Printer.print_class

let print_program = Printer.print_program





