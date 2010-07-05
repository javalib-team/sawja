open Sawja_pack
open Javalib_pack

open JBasics
open JCode
open Javalib

let run filename =
  iter
    (fun i_or_c -> 
       ignore
	 (map_interface_or_class_context ~force:true
	    (fun cm ir_code ->
	       Printf.printf "class %s: method %s\n"
		 (JPrint.class_name (get_name i_or_c))
		 (JPrint.method_signature cm.cm_signature);
	       let live = Live.run ir_code in
	       let rd = ReachDef.run ir_code in
	       let ae = AvailableExpr.run ir_code in
		 Array.iteri 
		   (fun i op -> 
		      Printf.printf "     --> LIVE[%d]: %s\n" i
			(Live.to_string (live i));
		      Printf.printf "     --> RD[%d]: %s\n" i
			(ReachDef.to_string ir_code (rd i));
		      Printf.printf "     --> AE[%d]: %s\n" i
			(AvailableExpr.to_string (ae i));
			Printf.printf "%3d: %s\n"
			  i (JBir.print_instr op))
		   ir_code.JBir.code;
		 print_newline ())
	    (map_interface_or_class_context JBir.transform i_or_c)))
    filename

let _ =
  if not !Sys.interactive then
    run Sys.argv.(1)

      
      
