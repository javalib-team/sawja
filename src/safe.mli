(*
 * This file is part of SAWJA
 * Copyright (c)2009, 2010 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

(** Defines a fixpoint solver managing domains for differents levels of the code
  representation (global, class, field, method, program point). [Safe] defines a
  structure of variables and the constraints between those variables.  It also
  defines domains for different levels of the program: global, classes, fields,
  methods and program points. {!ReachableMethods} is a very simple use case of
  this solver, and {!XTA} is a richer example.*)

(** In order to use this solver: 

    - Instantiate a variable module using the functor {!Safe.Var.Make}
    with the necessary {!Safe.Var.CONTEXT}. You could use
    {!Var.EmptyContext} if no context is needed.
    
    - Define a domain module (regarding {!Safe.Domain.S} interface)
    for each level of the program (global, class, field, method,
    program point). You could use {!Domain.Empty} module for
    unnecessary levels in your analysis. Some classic domain
    representations are supplied in {!Safe.Domain} module.

    - Instantiate a state module using the functor {!Safe.State.Make} with the
    previously defined modules.

    - Instantiate a constraints module using the functor
    {!Safe.Constraints.Make} with the state module.

    - Instantiate a solver module using the functor
    {!Safe.Solver.Make} with the constraints module.

    Once the aforementioned modules have been created:
    
    - Create an initial state {!Safe.State.S.t} using {!Safe.State.S.bot}
    and modifying it.

    - Compute the constraint list {!Safe.Constraints.S.cst} [list].
    
    - Create the variable list {!Safe.Var.S.t} [list] of entry points
    variables.

    - And then use the {!Safe.Solver.Make.solve_constraints} function to obtain
    the fixpoint.

*)

open Javalib_pack
module Domain : sig

  (*This exception can be used for debugging purpose. If your domain raise such
   * an exception, it will stop and raise an State.DebugSt exception containing
   * the last state reached before the fail.*)
  exception DebugDom

  (** This may be used to combine analyzes. *)
  module type TRADUCTOR_ANALYSIS =
  sig
    type localID
    type localDomain
    type globalID
    type globalDomain
    val loc2gloID : localID -> globalID
    val loc2gloDomain : localDomain -> globalDomain
    val glo2locID : globalID -> localID
    val glo2locDomain : globalDomain -> localDomain
  end

  (** Used when there is only one analysis. *)
  module Trad_Identity :
    functor (TYPE : sig type id type dom end) ->
  sig
    type localID = TYPE.id
    type localDomain = TYPE.dom
    type globalID = TYPE.id
    type globalDomain = TYPE.dom
    val loc2gloID : localID -> globalID
    val loc2gloDomain : localDomain -> globalDomain
    val glo2locID : globalID -> localID
    val glo2locDomain : globalDomain -> localDomain
  end

  module type S = sig

    (** Type of combined sub-analyzes domains (eg. D1.t * D2.t). *)
    type t

    (** Type of combined sub-analyzes IDs (Left of D1.analysisID | Right of
        D2.analysisID)*)
    type analysisID

    (** Type of sub-analyzes domains (eg. Left of D1.analysisDomain | Right of
        D2.analysisDomain) *)
    type analysisDomain

    (** Standard domain operations. *)

    val bot : t
    val isBot : analysisDomain -> bool

    (** [join modifies v1 v2] returns the union of [v1] and [v2] and sets
        [modifies] to true if the result is different from [v1]. *)
    val join : ?modifies:bool ref -> t -> t -> t
      (** [join_ad modifies v1 v2] returns the union of [v1] and [v2]
          and sets [modifies] to true if the result is different from
          [v1]. The option [do_join] allows avoiding to compute the
          join of values when it is known that the target value (the
          second one) is smaller or equal.*)
    val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
    val equal : t -> t -> bool
    val get_analysis : analysisID -> t -> analysisDomain
    val pprint : Format.formatter -> t -> unit
  end

  module Empty : S
    
  (** Builds a domain for local variables given the domain of the variables. *)
  module Local : functor (Var:S) -> sig
    type t
    type analysisID = Var.analysisID
    type analysisDomain = t
    val bot : t (*No map (Unreachable code)*)
    val init : t
      (** [init] is an initial value for local variables: it is not bottom but
          contains no local variable (it correspond to a reachable point in the
          code). *)
    val isBot : analysisDomain -> bool
    val join : ?modifies:bool ref -> t -> t -> t
    val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
    val equal : t -> t -> bool
    val get_analysis : analysisID -> t -> analysisDomain
    val pprint : Format.formatter -> t -> unit
    val get_var : int -> analysisDomain -> Var.t
    val set_var : int -> Var.t -> analysisDomain -> analysisDomain
      (** [set_var x v d] sets the value [v] to the variable [x] in the local
        function [d].  If a previous binding was already in place, then it is
        simply discarded *)
  end

  module Stack : functor (Var:S) -> sig

    type t = 
      Bot                   (*No Stack (unreacheble code)*)
    | Top                   (*Unknown stack (has potentially infinite element) *)
    | Stack of Var.t list   (*Known stack composed of abstract elements*)
    type analysisID = Var.analysisID
    type analysisDomain = t
    val bot : t
    val top : t
    val isBot : analysisDomain -> bool
    val isTop : analysisDomain -> bool  
    val join : ?modifies:bool ref -> t -> t -> t
    val join_ad : ?do_join:bool -> ?modifies:bool ref -> t -> analysisDomain -> t
    val equal : t -> t -> bool
    val get_analysis : analysisID -> t -> analysisDomain
    val pprint : Format.formatter -> t -> unit
    val init : t
      (** initial (empty) stack *)
    val push : Var.t -> t -> t
    val pop_n : int -> t -> t
    val pop : t -> t
    val first : t -> Var.t
      (** raise [Invalid_argument] if the stack is empty. Raise Failure if the
          stack is Top. *)
    val dup : t -> t
    val dupX1 : t -> t
    val dupX2 : t -> t
    val dup2 : t -> t
    val dup2X1 : t -> t
    val dup2X2 : t -> t
    val swap : t -> t
  end

  module Combine : functor (Left : S) -> functor (Right : S) -> sig
    include S
    module Trad_Left : functor (Trad : TRADUCTOR_ANALYSIS
                                with type globalID = Left.analysisID
                                and type globalDomain = Left.analysisDomain) ->
      (TRADUCTOR_ANALYSIS
       with type localID = Trad.localID
       and type localDomain = Trad.localDomain
       and type globalID = analysisID
       and type globalDomain = analysisDomain)
    module Trad_Right :
      functor (Trad : TRADUCTOR_ANALYSIS
               with type globalID = Right.analysisID
               and type globalDomain = Right.analysisDomain) ->
        (TRADUCTOR_ANALYSIS
         with type localID = Trad.localID
         and type localDomain = Trad.localDomain
         and type globalID = analysisID
         and type globalDomain = analysisDomain)
  end

end



module Var : sig

  module type CONTEXT =
  sig

  (** The Context can be
      - Context sensitivity (duplicate program points)
      - Analysis identification (several program points because there are several analyses)
      - Information flow (intermediate state, return, parameters, returned exceptions, etc. ) *)
    type context

    val compare : context -> context -> int
    val equal : context -> context -> bool
    val hash : context -> int
    val to_string : context -> string
    val pprint : Format.formatter -> context -> unit
  end
  module EmptyContext : (CONTEXT with type context = unit)

  module type S = sig
    module Context : CONTEXT

    (** just a shortcut *)
    type ioc = JBasics.class_name
    type var_global = [ `Global of Context.context ]
    type var_ioc = [ `IOC of Context.context * ioc ]
    type var_field =
        [ `Field of Context.context * ioc * JBasics.field_signature ]
    type var_method =
        [ `Method of Context.context * ioc * JBasics.method_signature ]
    type var_pp =
        [ `PP of Context.context * ioc * JBasics.method_signature * int ]
    type t =
        [ `Field of Context.context * ioc * JBasics.field_signature
        | `Global of Context.context
        | `IOC of Context.context * ioc
        | `Method of Context.context * ioc * JBasics.method_signature
        | `PP of Context.context * ioc * JBasics.method_signature * int ]
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val pprint : Format.formatter -> t -> unit
    val compare_global : var_global -> var_global -> int
    val compare_ioc : var_ioc -> var_ioc -> int
    val compare_field : var_field -> var_field -> int
    val compare_method : var_method -> var_method -> int
    val compare_pp : var_pp -> var_pp -> int
    val equal_global : var_global -> var_global -> bool
    val equal_ioc : var_ioc -> var_ioc -> bool
    val equal_field : var_field -> var_field -> bool
    val equal_method : var_method -> var_method -> bool
    val equal_pp : var_pp -> var_pp -> bool
    val hash_global : var_global -> int
    val hash_ioc : var_ioc -> int
    val hash_field : var_field -> int
    val hash_method : var_method -> int
    val hash_pp : var_pp -> int
    val pprint_global : Format.formatter -> var_global -> unit
    val pprint_ioc : Format.formatter -> var_ioc -> unit
    val pprint_field : Format.formatter -> var_field -> unit
    val pprint_method : Format.formatter -> var_method -> unit
    val pprint_pp : Format.formatter -> var_pp -> unit
  end

  module Make :
    functor (Context : CONTEXT) -> (S with module Context = Context)
      
end

module State : sig

  module type S = sig

    (** One domain for each kind of variable. *)
    module Var : Var.S
    module Global : Domain.S
    module IOC : Domain.S
    module Field : Domain.S
    module Method : Domain.S
    module PP : Domain.S

    type analysisID =
        [ `FieldAnalysis of Field.analysisID
        | `GlobalAnalysis of Global.analysisID
        | `IOCAnalysis of IOC.analysisID
        | `MethodAnalysis of Method.analysisID
        | `PPAnalysis of PP.analysisID ]

    (** Data (value) for one particular analysis. *)
    type analysisDomain =
        [ `FieldDomain of Field.analysisDomain
        | `GlobalDomain of Global.analysisDomain
        | `IOCDomain of IOC.analysisDomain
        | `MethodDomain of Method.analysisDomain
        | `PPDomain of PP.analysisDomain ]

    (** Data for all analyses for one particular variable (slot). *)
    type abData =
        [ `Field of Field.t
        | `Global of Global.t
        | `IOC of IOC.t
        | `Method of Method.t
        | `PP of PP.t ]

    type t

    exception DebugSt of t
 
    (** [bot (g,c,f,m,p)] generates an bottom element where [g], [c], [f], [m]
        and [p] are approximations of the number of global, class, field, method
        and program point variables, respectively. Note that any positive value
        is correct, but poorly chosen ones may affect performance. *)
    val bot : (int*int*int*int*int) -> t
    val pprint : Format.formatter -> t -> unit
    val get_pinfo :
      'a JProgram.program -> t -> JPrintHtml.info -> JPrintHtml.info

      
    val join_ad :
      ?do_join:bool -> ?modifies:bool ref -> abData -> analysisDomain -> abData
      
    (** [join] must only be used for initialization of State and not during
      constraint resolution.*)
    val join : ?do_join:bool -> ?modifies:bool ref -> t -> Var.t -> analysisDomain -> unit
      
    (** {2 Accessing data content}*)



    val get : t -> Var.t -> abData
    val get_global : t -> Var.var_global -> Global.t
    val get_IOC : t -> Var.var_ioc -> IOC.t
    val get_field : t -> Var.var_field -> Field.t
    val get_method : t -> Var.var_method -> Method.t
    val get_PP : t -> Var.var_pp -> PP.t

    val get_ab_global : abData -> Global.t
    val get_ab_field : abData -> Field.t
    val get_ab_method : abData -> Method.t
    val get_ab_IOC : abData -> IOC.t
    val get_ab_pp : abData -> PP.t
  
    (** {2 Modifying final results}*)
      
    (** {b Warning: State MUST not be modified manually during constraint
      resolution. The following functions MUST only be used on the final result
      of State}! *)


    val iter_global : t -> (t -> Var.var_global -> abData -> unit) 
      -> unit
    val iter_IOC : t -> (t -> Var.var_ioc -> abData-> unit) -> unit
    val iter_field : t -> (t -> Var.var_field -> abData -> unit) 
      -> unit
    val iter_method : t -> (t -> Var.var_method -> abData -> unit) 
      -> unit
    val iter_PP : t -> (t -> Var.var_pp -> abData -> unit) -> unit  
    val replace : t -> Var.t -> abData -> unit
    val remove : t -> Var.t -> unit
  end

  module Make :
    functor (Var : Var.S) ->
      functor (GlobalDomain : Domain.S) ->
        functor (IOCDomain : Domain.S) ->
          functor (FieldDomain : Domain.S) ->
            functor (MethodDomain : Domain.S) ->
              functor (PPDomain : Domain.S) ->
                (S with module Var = Var
                   and module Global = GlobalDomain
                   and module IOC = IOCDomain
                   and module Field = FieldDomain
                   and module Method = MethodDomain
                   and module PP = PPDomain)
end


module Constraints : sig

  module type S = sig
    module State : State.S
    type variable = State.Var.t
    type cst = {
      dependencies : variable list;
      target : variable;
      transferFun : State.t -> State.analysisDomain;
    }
    val get_dependencies : cst -> variable list
    val get_target : cst -> variable
    val pprint : Format.formatter -> cst -> unit

    (** [apply_cst ?modifies abst cst] applies the constraint [cst] on the current
      [abst].  The result of the constraint (given by [cst.transferFun]) is
      joined to the current value stored in [abst]. [modifies] is set to true
      if the application of a constraint modified the state [abst].

      If a DebugDom exception is raised by the used domain, this function catch
      it and raise a DebugSt exception containing the last state reached before
      the fail. This is intended for debug.
      *)
    val apply_cst : ?do_join:bool -> ?modifies:bool ref -> State.t -> cst -> unit
  end

  module Make : functor (State : State.S) ->
    (S with module State = State)
end

module Solver : sig
  module Make : functor (Constraints : Constraints.S) -> sig

    (** [debug_level] defines the debugging level (verbosity) of the solver *)
    val debug_level : int ref

    (** [solve_constraints ~optimize_join prog csts state init] computes the
      fixpoint of the constraints [csts], starting from the initial state
      [state] by applying the constraints that depends on nothing or on initial
      variables [init].  If [optimize_join] is true, then it tries to avoid
      joining useless values, at the cost of some additional computations. *)
    val solve_constraints :
      ?optimize_join:bool ->
      'a ->
      Constraints.cst list ->
      Constraints.State.t ->
      Constraints.State.Var.t list -> Constraints.State.t
  end
end
