open Cmn
open Javalib
include Bir

let out_statistics = ref stdout
let out_nbvariables = ref stdout

let set_out_statistics filename = 
  if not (is_file filename)
  then begin
    out_statistics := open_out filename;
    Printf.fprintf !out_statistics "bc_size ";
    Printf.fprintf !out_statistics "nb_bcvars ";
    Printf.fprintf !out_statistics "nb_total_tempvars ";
    Printf.fprintf !out_statistics "nb_branchvar ";
    Printf.fprintf !out_statistics "nb_may_alias_var ";
    Printf.fprintf !out_statistics "nb_must_alias_var ";
    Printf.fprintf !out_statistics "nb_side_effect_var \n"
  end
  else out_statistics := open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0 filename

let close_out_statistics () = 
  close_out !out_statistics

let set_out_nbvariables filename = 
  if not (is_file filename)
  then begin
    out_nbvariables := open_out filename
  end
  else out_nbvariables := open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0 filename

let close_out_nbvariables () = 
  close_out !out_nbvariables

let add_nbvariables nb_jimple nb_bir =
  Printf.fprintf !out_nbvariables  "%d %d \n" nb_jimple nb_bir 
    
let add_stat implem stats =
  match implem with
    | Java code -> 
	let code = Lazy.force code in
	let size = Array.fold_left (fun n ins -> if ins<>JCode.OpInvalid then n+1 else n) 0 (code.JCode.c_code) in
	let nb_bcvars = code.JCode.c_max_locals in
	  Printf.fprintf !out_statistics "%d " size;
	  Printf.fprintf !out_statistics "%d " nb_bcvars;
	  Printf.fprintf !out_statistics "%d " stats.stat_nb_total;
	  Printf.fprintf !out_statistics "%d " stats.stat_nb_branchvar;
	  Printf.fprintf !out_statistics "%d " stats.stat_nb_tempvar_may_alias;
	  Printf.fprintf !out_statistics "%d " stats.stat_nb_tempvar_must_alias;
	  Printf.fprintf !out_statistics "%d \n" stats.stat_nb_tempvar_side_effect
    | _ -> assert false
      


let show flat verbose cstats m  = 
   match m with 
   | ConcreteMethod cm ->
       begin
	 if verbose then Printf.printf "\n%s\n" (JPrint.method_signature cm.cm_signature) ;
	 try
	   let (ir,stats) = transform_intra_stats flat ~stats:cstats cm in 
	     begin 
	       match ir.cm_implementation with 
		 | Java m -> begin
		     if verbose then List.iter (fun s -> Printf.printf "%s" s) (print_bir_intra m);
		     if cstats then
		       match stats with
			 | Some stats -> add_stat cm.cm_implementation stats 
			 | None -> assert false
		   end
		 | _ -> if verbose then Printf.printf "" 
	     end
	 with 
	   | Subroutine -> if verbose then Printf.printf " SUBROUTINE !"
       end
   | AbstractMethod am -> if verbose then Printf.printf "%s\nABSTRACT\n\n" (JPrint.method_signature am.am_signature)

 let show_iorc flat verbose cstats ci = 
   match ci with 
     | JInterface(i) ->
	 if verbose then 	Printf.printf "\ninterface %s\n\n" (JPrint.class_name i.i_name);
	 begin
	   match i.i_initializer with
	     | None -> ()
	     | Some cm -> show flat verbose cstats  (ConcreteMethod cm) 
	 end
     |  JClass (cl) -> 
	  begin
	    if verbose then Printf.printf "\nclass %s\n\n"(JPrint.class_name cl.c_name);
	    JBasics.MethodMap.iter (fun _ -> show flat verbose cstats) cl.c_methods 
	  end

let average l =
  let (total,size) = 
    List.fold_left (fun (total,size) x -> (total+.x,size+1)) (0.,0) l in
    if (size<>0) then total /. (float_of_int size)    else 0.

let run mode ?(verbose=false) filename =
  Javalib.iter (show_iorc mode verbose true) filename
