open Cmn
open Javalib
include Bir


let incr_stats stats a = 
  match stats with 
    | None -> ()
    | Some s ->
	match a with 
	  |  `Nb_jump_with_non_empty_stacks -> s.nb_jump_with_non_empty_stacks <- s.nb_jump_with_non_empty_stacks + 1
	  |  `Nb_back_jump_with_non_empty_stacks -> 
	       s.nb_back_jump_with_non_empty_stacks <- s.nb_back_jump_with_non_empty_stacks + 1
	  |  `Nb_store_is_var_in_stack -> s.nb_store_is_var_in_stack <- s.nb_store_is_var_in_stack + 1
	  |  `Nb_incr_is_var_in_stack -> s.nb_incr_is_var_in_stack <- s.nb_incr_is_var_in_stack + 1
	  |  `Nb_putfield_is_field_in_stack -> s.nb_putfield_is_field_in_stack <- s.nb_putfield_is_field_in_stack + 1
	  |  `Nb_arraystore_is_array_access_in_stack ->
	       s.nb_arraystore_is_array_access_in_stack <- s.nb_arraystore_is_array_access_in_stack + 1
	  |  `Nb_putstatic_is_static_in_stack -> s.nb_putstatic_is_static_in_stack <- s.nb_putstatic_is_static_in_stack + 1
	  |  `Nb_method_call_with_modifiable_in_stack -> 
	       s.nb_method_call_with_modifiable_in_stack <- s.nb_method_call_with_modifiable_in_stack + 1
	  |  `Nb_store -> s.nb_store <- s.nb_store + 1
	  |  `Nb_incr -> s.nb_incr <- s.nb_incr + 1
	  |  `Nb_putfield -> s.nb_putfield <- s.nb_putfield +1
	  |  `Nb_arraystore -> s.nb_arraystore <- s.nb_arraystore + 1
	  |  `Nb_putstatic ->  s.nb_putstatic <-  s.nb_putstatic + 1
	  |  `Nb_method_call -> s.nb_method_call <- s.nb_method_call + 1
	  |  `Nb_tempvar -> s.nb_tempvar <- s.nb_tempvar +1
	  |  `Nb_tempvar_branch -> s.nb_tempvar_branch <- s.nb_tempvar_branch +1 
	  |  `Nb_tempvar_removed -> s.nb_tempvar_removed <- s.nb_tempvar_removed + 1
	  |  `Nb_tempvar_method_effect -> s.nb_tempvar_method_effect <- s.nb_tempvar_method_effect
	  |  `Nb_tempvar_putfield -> s.nb_tempvar_putfield <- s.nb_tempvar_putfield +1
	  |  `Nb_tempvar_arraystore -> s.nb_tempvar_arraystore <- s.nb_tempvar_arraystore + 1
	  |  `Nb_tempvar_side_effect -> s.nb_tempvar_side_effect <- s.nb_tempvar_side_effect + 1
	  |  `Nb_classes -> s.nb_classes <-  s.nb_classes + 1
	  |  `Nb_methods -> s.nb_methods <-  s.nb_methods + 1
	  |  `Nb_subroutines ->  s.nb_subroutines <- s.nb_subroutines + 1

let show flat verbose cstats m stat  = 
   match m with 
   | ConcreteMethod cm ->
       begin
	 if verbose then Printf.printf "\n%s\n" (JPrint.method_signature cm.cm_signature) ;
	 incr_stats stat `Nb_methods ;
	 try
	   let (ir,stats) = transform_intra_stats flat ~stats:cstats ~stats_opt:stat cm in 
	     begin 
	       match ir.cm_implementation with 
		 | Java m -> if verbose then List.iter (fun s -> Printf.printf "%s" s) (print_bir_intra m) ; stats
		     (*(print_bir (Lazy.force m)) ; stats*)
		 | _ -> if verbose then Printf.printf "" ; stats
	     end
	 with 
	   | Subroutine -> if verbose then Printf.printf " SUBROUTINE !"; 
	       incr_stats stat `Nb_subroutines ;
	       None
       end
   | AbstractMethod am -> if verbose then Printf.printf "%s\nABSTRACT\n\n" (JPrint.method_signature am.am_signature) ; stat

 let show_iorc flat verbose cstats ~stats ci = 
   match ci with 
     | JInterface(i) ->
	 incr_stats stats `Nb_classes ;
	 if verbose then 	Printf.printf "\ninterface %s\n\n" (JPrint.class_name i.i_name);
	 begin
	   match i.i_initializer with
	     | None -> stats
	     | Some cm -> show flat verbose cstats  (ConcreteMethod cm) stats
	 end
     |  JClass (cl) -> 
	  begin
	    incr_stats stats `Nb_classes ;
	    if verbose then Printf.printf "\nclass %s\n\n"(JPrint.class_name cl.c_name);
	    JBasics.MethodMap.fold (fun _ -> show flat verbose cstats) cl.c_methods stats
	  end

let average l =
  let (total,size) = 
    List.fold_left (fun (total,size) x -> (total+.x,size+1)) (0.,0) l in
    if (size<>0) then total /. (float_of_int size)    else 0.

let show_average_stat stats =
    let average_tempvar = average stats.average_tempvar in
    let average_tempvar_branch = average stats.average_tempvar_branch in
    let average_tempvar_putfield = average stats.average_tempvar_putfield in
    let average_tempvar_arraystore = average stats.average_tempvar_arraystore in
    let average_tempvar_method_effect = average stats.average_tempvar_method_effect in
    let average_tempvar_side_effect = average stats.average_tempvar_side_effect in
    let average_tempvar_after_simplification = average stats.average_tempvar_after_simplification in
    let total_average = average_tempvar_branch +. average_tempvar_putfield +. average_tempvar_arraystore +. 
      average_tempvar_method_effect +. average_tempvar_side_effect +. average_tempvar_after_simplification in
      Printf.printf "\n" ;
      Printf.printf "%2d\t classes (or interfaces) have been parsed\n" stats.nb_classes;
      Printf.printf "%2d\t methods have been transformed\n" stats.nb_methods;
      Printf.printf "%2d\t transformations have failed because of subroutines\n\n" stats.nb_subroutines;
      Printf.printf "%03.2f%%\t average of local var increase\n" average_tempvar;
      Printf.printf "%03.2f%%\t average of local var increase because of branch variables\n" average_tempvar_branch;
      Printf.printf "%03.2f%%\t average of local var increase because of putfield variables\n" average_tempvar_putfield;
      Printf.printf "%03.2f%%\t average of local var increase because of arraystore variables\n" average_tempvar_arraystore;
      Printf.printf "%03.2f%%\t average of local var increase because of method_effect variables\n" average_tempvar_method_effect;
      Printf.printf "%03.2f%%\t average of local var increase because of side effect free expressions\n" average_tempvar_side_effect;
      Printf.printf "%03.2f%%\t average of local var that can be decrease with simplification\n" average_tempvar_after_simplification;
      Printf.printf "%03.2f%%\t of total average\n" total_average 


let show_file flat verbose cstats cp name =
  reset_stats0 ; show_iorc flat verbose cstats ~stats:(Some stats0) (get_class cp name) 


let run_on_class flat classfile =
  if is_file classfile && Filename.check_suffix classfile ".class" then 
    begin
      let cp = class_path (Filename.dirname classfile) in
      let file = Filename.chop_suffix (Filename.basename classfile) ".class" in
      let stats = show_file flat true true cp (JBasics.make_cn file) in
	close_class_path cp;
	match stats with 
	  | Some s -> show_average_stat s
	  | None -> Printf.printf "No statistics was could be computed"
    end 
  else begin
    Printf.printf "%s is not a valid class file\n" classfile;
    exit 0
  end


let run_on_jar flat jarfile =
  if is_file jarfile && Filename.check_suffix jarfile ".jar" then
    begin
      let cp = Filename.dirname jarfile in
      let jarfile = Filename.basename jarfile in
	reset_stats0 ; 
	let stats = read (Javalib.make_directories cp) (fun stats -> show_iorc flat false true ~stats:stats) (Some stats0) [jarfile]
	in 
	  match stats with 
	    | Some s -> show_average_stat s
	    | _ -> Printf.printf "No statistics could be computed"
    end
  else begin
    Printf.printf "%s is not a valid jar file\n" jarfile;
    exit 0
  end

let make_dir_absolute dir =
  if Filename.is_relative dir
  then Filename.concat (Unix.getcwd ()) dir
  else dir



let run_on_dir flat dir =
  if is_dir dir then
    let cp = dir in
    let jar_files = ref [] in
    let dir = Unix.opendir (make_dir_absolute dir) in
      reset_stats0 ; 
      try 
	while true do
	  let next = Unix.readdir dir in
	    if Filename.check_suffix next ".jar" then jar_files := next :: !jar_files
	done 
      with End_of_file -> 
	( Unix.closedir dir;
	  let stats = read (Javalib.make_directories cp) (fun stats -> show_iorc flat false true ~stats:stats) (Some stats0) !jar_files	    in
	    match stats with 
	      | Some s -> show_average_stat s
	      | None -> Printf.printf "No statistics could be computed")
  else begin
    Printf.printf "%s is not a valid directory\n" dir;
    exit 0
  end
