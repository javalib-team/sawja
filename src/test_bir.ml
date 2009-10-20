

type mode = Class | Jar | Dir

let mode = ref Class
let target = ref ""

let args = [ 
  ("-class", Arg.String (fun s ->
			     mode := Class;
			     target := s) , " Transform all methods of a given .class file and give statistics");
  ("-compress", Arg.Bool (fun b -> Bc2bir.compress_ir_flag:=b) , " Compress empty lines");
  ("-simplify", Arg.Bool (fun b -> Bc2bir.simplify_assign_flag:=b) , " Simplify consecutive assignements ");
  ("-jar", Arg.String (fun s -> mode := Jar;
			   target := s) ,  " Transform all methods of a given .jar file and print statistics");
  ("-dir", Arg.String (fun s -> mode := Dir;
			   target := s) , " Transform all methods of all .jar files in a given directory and print statistics");
]

let _ =
   if not !Sys.interactive then
    begin
     Arg.parse args (fun s -> target := s) ("usage: "^Sys.argv.(0)^" <classname>") ;
      try
	match !mode with
	  | Class -> Bc2bir.run_on_class Bc2bir.Normal (!target) 
	  | Jar -> Bc2bir.run_on_jar Bc2bir.Normal !target 
	  | Dir -> Bc2bir.run_on_dir Bc2bir.Normal !target
      with 
	| x -> Printf.printf "uncaught exception %s\n" (Printexc.to_string x) 
    end    

      
    
