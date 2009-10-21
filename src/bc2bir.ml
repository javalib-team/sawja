open Cmn
open Javalib
include Bir

let out_statistics = ref stdout
let out_nbvariables = ref stdout

let is_file f =
  try
    (Unix.stat f).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let set_out_statistics filename = 
  if not (is_file filename)
  then begin
    out_statistics := open_out filename;
    Printf.fprintf !out_statistics "bc_size ";
    Printf.fprintf !out_statistics "nb_bcvars ";
    Printf.fprintf !out_statistics "nb_total_tempvars ";
    Printf.fprintf !out_statistics "nb_branchvar ";
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
  let percentage x y = (100*x)/y in
  let incr t i = t.(i) <- t.(i) +1 in
  let n = ref 0 in
  let add t i v =
    if (not (t.(i) < max_int /2)) then 
      (Printf.printf "error %d at line %d\n" i !n; exit 0);
    assert (v < max_int /2);
    t.(i) <- t.(i) + v in
  let data = "compact1.csv" in
  let benchs =  
    ["jar/javacc.jar";
     "jar/sootclasses-2.2.3.jar";
     "jar/jscience.jar";
     "jar/rt.jar"] in
    List.iter
      (fun f -> 
	 if not (is_file f) 
	 then begin
	   Printf.printf "bench %s is missing\n" f; 
	   exit 0
	 end) benchs;
    if is_file data then Unix.unlink data;
    let f = open_in "compact1.csv" in
    let _ = input_line f in
      begin
	try
	  while true do 
	    n := !n + 1;
	    let r = input_line f in
	    let s = Array.of_list (List.map int_of_string (Str.split (Str.regexp " ") r)) in
	    let i = find_range s.(0) in
	      incr bc_size i;
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

      
type mode = JBir | BBir

let mode = ref JBir
let target = ref ""

let args = [ 
  ("-compress", Arg.Bool (fun b -> Bc2bir.compress_ir_flag:=b) , " Compress empty lines");
  ("-simplify", Arg.Bool (fun b -> Bc2bir.simplify_assign_flag:=b) , " Simplify consecutive assignements ");
  ("-block", Arg.Unit (fun _ -> mode := BBir) ,  "")
]

let show transform print j_iorc =
  JPrint.print_class (Javalib.map_interface_or_class (transform ~compress:true) j_iorc)
    print


let _ =
   if not !Sys.interactive then
    begin
     Arg.parse args (fun s -> target := s) ("usage: "^Sys.argv.(0)^" <classname>") ;
      try 
	match !mode with
	  | JBir ->
	      Bc2bir.run Bc2bir.Normal ~verbose:(Filename.check_suffix !target ".class") !target
	  | BBir ->
	      Javalib.iter BBir.show !target
      with 
	| x -> Printf.printf "uncaught exception %s\n" (Printexc.to_string x) 
    end    

      
    
