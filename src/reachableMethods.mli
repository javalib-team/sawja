(** ReachabeMethods allows to compute the methods of a program that
    are reachable from a set of entry points.  This can be used in
    order to update the field [parsed_method] of {!JProgram.program}.
    The implementation is not very efficient, but it is simple and
    rely on the {!Safe} framework. *)

open Javalib_pack


(** [compute_reachable_methods p entry_points] computes the methods of
    a program that are reachable from the set of entry points
    [entry_points] in the program [p].  It assumes that
    [p.parsed_methods] is a correct over-approximation of reachable
    methods, and the result of this function can be used to refine
    [p.parsed_methods].  The implementation is not very efficient, but
    it is simple and rely on the {!Safe} framework.  The look-up
    function used to compute the call graph is
    {!JControlFlow.get_successors} which uses [p.static_lookup_method]
    to resolve method calls and also take in account static
    initializers.  *)
val compute_reachable_methods :
    JCode.jcode JProgram.program -> 
    JBasics.class_method_signature list -> 
    (JCode.jcode JProgram.node * JCode.jcode Javalib.concrete_method) JBasics.ClassMethodMap.t
