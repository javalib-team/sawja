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


open Sawja_pack
open Javalib_pack

open Javalib

let run filename =
  print_endline "JBir:";
  iter
    (fun i_or_c -> 
       ignore
	 (map_interface_or_class_context ~force:true
	    (fun cm ir_code ->
	       Printf.printf "class %s: method %s\n"
		 (JPrint.class_name (get_name i_or_c))
		 (JPrint.method_signature cm.cm_signature);
	       let live = Live_bir.run ir_code in
	       let rd = ReachDef.run ir_code in
         let rd2 = ReachDef.run2 ir_code in				
	       let ae = AvailableExpr.run ir_code in
		 Array.iteri 
		   (fun i op -> 
		      Printf.printf "     --> LIVE[%d]: %s\n" i
			(Live_bir.to_string (live i));
		      Printf.printf "     --> RD[%d]: %s\n" i
			(ReachDef.Lat.to_string ir_code (rd i));
					Printf.printf "     --> RD2[%d]: %s\n" i
			(ReachDef.Lat.to_string ir_code (rd2 i));
		      Printf.printf "     --> AE[%d]: %s\n" i
			(AvailableExpr.Lat.to_string (ae i));
			Printf.printf "%3d: %s\n"
			  i (JBir.print_instr op))
		   (JBir.code ir_code);
		 print_newline ())
	    (map_interface_or_class_context JBir.transform i_or_c)))
    filename;
  print_endline "JBirSSA:";
  iter
    (fun i_or_c -> 
       ignore
	 (map_interface_or_class_context ~force:true
	    (fun cm ir_code ->
	       Printf.printf "class %s: method %s\n"
		 (JPrint.class_name (get_name i_or_c))
		 (JPrint.method_signature cm.cm_signature);
	       let ae = AvailableExprSSA.run ir_code in
		 List.iter print_endline (JBirSSA.print ~phi_simpl:false ir_code);
		 print_newline ();
		 print_endline "Available expression for each ssa variable:";
		 Ptmap.iter 
		   (fun i v -> 
		      Printf.printf "--> AE[%s]: %s\n"
			(JBirSSA.var_name_g v)
			(AvailableExprSSA.to_string (ae i));)
		   (JBirSSA.vars ir_code);
		 print_newline ();
	    )
	    (map_interface_or_class_context JBirSSA.transform i_or_c)))
    filename

let _ =
  if not !Sys.interactive then
    run Sys.argv.(1)

      
      
