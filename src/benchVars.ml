open Javalib
open Bir
open JBasics

let jmethod_msig_stats m =
  match m with
    | ConcreteMethod cm ->
	begin try
	  let (ir,stats) = transform_intra_stats Addr3 ~stats:true cm in
	    begin
	      match ir.cm_implementation with
		| Java _ ->  (get_method_signature m), stats
		| _ ->  (get_method_signature m), None
	    end
	with
	  | Subroutine ->  (get_method_signature m), None
	end
    | AbstractMethod _ -> (get_method_signature m), None
	

let _ =  
  let classfile = Sys.argv.(1) in
  let jimplefile = Sys.argv.(2) in
  let dumpfile = Sys.argv.(3) in
    if is_file classfile && Filename.check_suffix classfile ".class" then 
      begin
	let jimpleMap = JimpleVars.compute jimplefile in
	let cp = class_path (Filename.dirname classfile) in
	let file = Filename.chop_suffix (Filename.basename classfile) ".class" in
	let cl = (get_class cp (make_cn file)) in
	let methods = get_methods cl in
	  Bc2bir.set_out_nbvariables dumpfile ;
	  MethodMap.iter 
	    (fun _ m -> 
	       let bir, stats = jmethod_msig_stats (get_method cl (get_method_signature m)) in 
	       let nb_jimple = MethodMap.find (get_method_signature m) jimpleMap in 
		 match stats with 
		   | None -> ()
		   | Some s -> Bc2bir.add_nbvariables nb_jimple s.stat_nb_total)
	    methods ; 
	  Bc2bir.close_out_nbvariables() ;
	  close_class_path cp;
      end 
    else raise (JBasics.No_class_found classfile)
   
