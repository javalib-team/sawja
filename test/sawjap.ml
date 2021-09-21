open! Javalib_pack
open Sawja_pack

module IR = JBir

let transform folding cm c =
  IR.transform cm c ~folding

let print_class folding i_or_c = 
  Javalib.JPrint.print_class
    (Javalib.map_interface_or_class_context
       (transform folding)
       i_or_c)		  
    (IR.print ~show_type:false)
    stdout





  
let () =
  let input_files = ref [] in
  let folding = ref JBir.FoldOrFail in
  Arg.parse
    [("--fold",
      Arg.Symbol
        (["yes"; "no"; "try"],
         function 
         | "yes" -> folding := JBir.FoldOrFail
         | "no" -> folding := JBir.DoNotFold
         | "try" -> folding := JBir.FoldIfPossible
         | _ -> assert false),
      "Disable/enable constructor folding")]
    (fun filename -> input_files := filename :: !input_files)
    "sawjap [--do-not-fold] <file>";
  List.iter (Javalib.iter (print_class !folding)) !input_files

