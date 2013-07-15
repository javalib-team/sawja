open Javalib_pack
open JBasics
open HeapParser
open ParserType

let parse_program instances_dump classpath cms = 
  let heap = get_init_heap instances_dump in
  let (hp_class,hp_array) = (heap.hp_class,heap.hp_array)
  in
  let instanciated = 
    ClassMap.fold 
      (fun cn cl lst -> 
         match Ptmap.is_empty cl.cl_instances with
           | true -> lst
           | false -> cn::lst) 
      hp_class []
  in
  JRTA.parse_program ~instantiated:instanciated ~other_entrypoints:[] classpath cms



