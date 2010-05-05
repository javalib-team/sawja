open Javalib_pack

val compute_reachable_methods :
    JCode.jcode JProgram.program -> 
    JBasics.class_method_signature list -> 
    (JCode.jcode JProgram.node * JCode.jcode Javalib.concrete_method) JBasics.ClassMethodMap.t
