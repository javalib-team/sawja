open Cmn
open Javalib

(* include Bir *)

let benchs = ref ["/Users/demange/sootall-2.3.0.jar"]
  (* ["jar/javacc.jar"; *)
  (*      "jar/sootclasses-2.2.3.jar"; *)
  (*      "jar/jscience.jar"; *)
  (*      "jar/rt.jar"] *) 
let out_statistics = ref stdout
let out_nbvariables = ref stdout

let is_file f =
  try
    (Unix.stat f).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let is_dir d =
  try
    (Unix.stat d).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let set_out_statistics filename = 
  if not (is_file filename)
  then begin
    out_statistics := open_out filename;
    Printf.fprintf !out_statistics "bc_size ";
    Printf.fprintf !out_statistics "nb_bcvars ";
    Printf.fprintf !out_statistics "nb_total_tempvars ";
    Printf.fprintf !out_statistics "nb_branchvar \n";
  end
  else out_statistics := open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0 filename

let set_out_nbvariables filename = 
  if not (is_file filename)
  then out_nbvariables := open_out filename
  else out_nbvariables := open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0 filename


let add_nbvariables nb_jimple nb_bir =
  Printf.fprintf !out_nbvariables  "%d %d \n" nb_jimple nb_bir 
    
let add_stat stats implem =
  match implem with
    | Java code -> 
	let code = Lazy.force code in
	let size = Array.fold_left (fun n ins -> if ins<>JCode.OpInvalid then n+1 else n) 0 (code.JCode.c_code) in
	let nb_bcvars = code.JCode.c_max_locals in
	  Printf.fprintf !out_statistics "%d " size;
	  Printf.fprintf !out_statistics "%d " nb_bcvars;
	  Printf.fprintf !out_statistics "%d " stats.Bir.stat_nb_total;
	  Printf.fprintf !out_statistics "%d\n" (stats.Bir.stat_nb_branchvar+stats.Bir.stat_nb_branchvar2)
    | _ -> assert false

let sort_benchs_by_size () =
  let ranges = [25;50;100;200;400;800;1600] in
  let find_range x =
    let rec aux = function
	[] -> 0
      | y::q -> if x<y then 0 else 1+ (aux q)
    in aux ranges in
  let bc_size = Array.make (1+ List.length ranges) 0 in
  let total_bcvar = Array.make (1+ List.length ranges) 0 in
  let total_temp = Array.make (1+ List.length ranges) 0 in
  let temp_branch = Array.make (1+ List.length ranges) 0 in
  let percentage x y = 
    match y with 
      | 0 -> assert false
      | _ -> (100*x)/y in
  let incr_tab t i = t.(i) <- t.(i) +1 in
  let n = ref 0 in
  let add t i v =
    if (not (t.(i) < max_int /2)) then 
      (Printf.printf "error %d at line %d\n" i !n; exit 0);
    assert (v < max_int /2);
    t.(i) <- t.(i) + v in
  let benchs =  !benchs in
    List.iter
      (fun f -> 
	 if not (is_file f) 
	 then begin
	   Printf.printf "bench %s is missing\n" f; 
	   exit 0
	 end) benchs;
    (* if is_file data then Unix.unlink data;  *)
    let f = open_in "compact1.data" in
    let _ = input_line f in
      begin
	try
	  while true do 
	    n := !n + 1;
	    let r = input_line f in
	    let s = Array.of_list (List.map int_of_string (Str.split (Str.regexp " ") r)) in
	      if (Array.length s) <> 4 then raise End_of_file ;
	      let i = find_range s.(0) in
		incr_tab bc_size i;
		add total_bcvar i s.(1);
		add total_temp i s.(2);
		add temp_branch i s.(3);
	  done
	with End_of_file -> 
	  close_in f
      end;
      for i=0 to List.length ranges do
	Printf.printf "[%d,%s] : size=%d nb_tempvar=%d nb_branchvar=%d\n"
	  (if i=0 then 0 else List.nth ranges (i-1))
	  (if i=List.length ranges then "+oo" else (string_of_int (List.nth ranges i)))
	  bc_size.(i)
	  (percentage total_temp.(i) total_bcvar.(i))
	  (percentage temp_branch.(i) total_bcvar.(i))
      done

      
(* type mode = JBir | BBir *)
(* let mode = ref JBir *)

let target = ref ""
let compress_ir_flag = ref false
let simplify_assign_flag = ref false

(* todiscuss (Delphine) : should not the option set the flag to true ? *)
let args = [ 
  ("-compress", Arg.Bool (fun b -> compress_ir_flag:=b) , " Compress empty lines");
  ("-simplify", Arg.Bool (fun b -> simplify_assign_flag:=b) , " Simplify consecutive assignements ");
  (*   ("-block", Arg.Unit (fun _ -> mode := BBir) ,  "") *)
]

let show transform print_fun j_iorc =
  JPrint.print_class
    (Javalib.map_interface_or_class transform j_iorc)
    print_fun 

let trans_print mode_trans j_iorc = 
  match mode_trans with 
    | Normal | Addr3 -> 
	show (JBir.transform ~compress:(!compress_ir_flag)) (fun ?(jvm=false) -> JBir.print) 
	  j_iorc stdout
    | Flat -> 
	show (FBir.transform ~compress:(!compress_ir_flag)) (fun ?(jvm=false) -> FBir.print)
	  j_iorc stdout
	  
type stats_mode = VarDistrib | CompSoot

let trans_stats transform stats j_iorc = 
  let ir = Javalib.map_interface_or_class transform j_iorc  in
    match stats with 
      | VarDistrib -> 	  
	  let ir_cmethods = Javalib.get_concrete_methods ir in
	  let j_cmethods = Javalib.get_concrete_methods j_iorc in
	    JBasics.MethodMap.iter 
	      (fun ms ir_cm ->
		 let j_cm = JBasics.MethodMap.find ms j_cmethods in
		   match ir_cm.cm_implementation with 
			 | Native -> ()
			 | Java typet ->
			     let stats = Bir.make_tempvar_stats typet.Bir.code in
			       add_stat stats j_cm.cm_implementation
	      )
	      ir_cmethods
      | CompSoot -> failwith "compute_stats"

    
type mode_run = Print | Stats

(* transform the classfile, jar or jar_dir according to the flags arguments and :
   - for a single class file : prints out its IR
   - for a jar or a jar directory : computes the associated statistics and do not print anything
*)
let _ =
  if not !Sys.interactive then
     begin
       Arg.parse args (fun s -> target := s) ("usage: "^Sys.argv.(0)^" [classname | jar | jar_dir]") ;
       try
	 let run_mode = begin 
	   if is_file !target && Filename.check_suffix !target ".class" then Print
	   else if is_file !target && Filename.check_suffix !target ".jar" then Stats
	   else if is_dir !target then Stats
	   else failwith "unknown type of argument" end
	 in
	   match run_mode with 
	     | Print -> Javalib.iter (trans_print Normal) !target 
	     | Stats -> 
		 begin
		   let data = "compact1.data" in
		   if is_file data then Unix.unlink data  ;
		   set_out_statistics data ;	
		   Javalib.iter 
		     (trans_stats (Bir.transform ~compress:(!compress_ir_flag)) VarDistrib) 
		     !target ;
		   sort_benchs_by_size () ;
		   close_out !out_statistics 		   
		 end
       with
	 | x -> Printf.printf "uncaught exception %s\n" (Printexc.to_string x) 
     end    

(* match !mode with *)
(* 	  | JBir -> *)
(* Javalib.iter compute_stats  *)
(* 	  Bc2bir.Normal ~verbose:(Filename.check_suffix !target ".class") !target *)
(*   | BBir -> *)
(* 	      Javalib.iter BBir.show !target *)
      
    
