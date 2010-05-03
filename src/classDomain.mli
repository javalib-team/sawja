open Javalib_pack

module Make :
  functor (S : sig val nb_bits:int end) ->
    sig
      type t
      type analysisID = unit
      type analysisDomain = t
      val bot : t
      val empty : t
      val isBot : t -> bool
      val singleton : JBasics.class_name -> t
      val is_empty : t -> bool
      val mem : JBasics.class_name -> t -> bool
      val set_size : t -> int
      val meet : t -> t -> t
      val of_set : JBasics.ClassSet.t -> t
      val to_set : t -> JBasics.ClassSet.t
      val join : ?modifies:bool ref -> t -> t -> t
      val join_ad : ?modifies:bool ref -> t -> t -> t
      val equal : t -> t -> bool
      val get_analysis : unit -> 'a -> 'a
      val pprint : Format.formatter -> t -> unit
    end
