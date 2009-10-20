

type mode = JBir | BBir

let mode = ref JBir
let target = ref ""

let args = [ 
  ("-compress", Arg.Bool (fun b -> Bc2bir.compress_ir_flag:=b) , " Compress empty lines");
  ("-simplify", Arg.Bool (fun b -> Bc2bir.simplify_assign_flag:=b) , " Simplify consecutive assignements ");
  ("-block", Arg.Unit (fun _ -> mode := BBir) ,  "")
]

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

      
    
