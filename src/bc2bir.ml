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

      
    
