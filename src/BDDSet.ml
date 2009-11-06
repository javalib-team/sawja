module BddBuddy = struct

  type t = Buddy.t

  let init nb_bits =
    if not (Buddy.isrunning ())
    then
      let check int f =
        if int < 0
        then failwith ("buddy."^f^" failed")
      in
      let nbnodes = 1000000 in
        check (Buddy.init nbnodes nbnodes) "init";
        check (Buddy.setcacheratio 2) "setcacheratio";
        check (Buddy.setmaxincrease nbnodes) "setmaxincrease";
        check (Buddy.setvarnum nb_bits) "setvarnum";
        check (Buddy.setmaxnodenum 0) "setmaxnodenum";
        check (Buddy.setminfreenodes 50) "setminfreenodes";
        (* ignore (Buddy.autoreorder Buddy.WIN2ITE); *)
        ()

  let ff _ = Buddy.ff ()
  let tt _ = Buddy.tt ()

  let bdd_and v1 v2 = Buddy.apply v1 v2 Buddy.AND
  let bdd_or v1 v2 = Buddy.apply v1 v2 Buddy.OR
  let bdd_implies v1 v2 = Buddy.apply v1 v2 Buddy.IMP

  let equal = (=)
  let compare = Pervasives.compare

  let ithvar _man i = Buddy.ithvar i
  let nithvar _man i = Buddy.nithvar i

  let is_true = (=) (tt ())
  let is_false = (=) (ff ())

  let high = Buddy.high
  let low = Buddy.low

  let var = Buddy.var

end

(* module BddCudd = struct *)
(*   type t = Bdd.t *)

(*   let nb_bits = 32 *)

(*   let init nb_bits = Manager.make nb_bits 0 0 0 0 *)

(*   let ff man = Bdd.dfalse man *)
(*   let tt man = Bdd.dtrue man *)

(*   let bdd_and = Bdd.dand *)
(*   let bdd_or = Bdd.dor *)
(*   let bdd_implies = Bdd.is_leq *)

(*   let equal = (=) *)
(*   let compare = Pervasives.compare *)


(*   let ithvar man v = Bdd.ithvar man v *)
(*   let nithvar man v = Bdd.dnot (Bdd.ithvar man v) *)

(*   let is_true = Bdd.is_true *)
(*   let is_false = Bdd.is_false *)

(*   let high = Bdd.dthen *)
(*   let low = Bdd.delse *)

(*   let var t = *)
(*     let var = Bdd.topvar t in *)
(*       if var = 65536 *)
(*       then invalid_arg "BddCudd.var" *)
(*       else var *)

(* end *)

module Make (S:sig val nb_bits : int end) = struct

  module Bdd = BddBuddy

  type t = Bdd.t

  type bdd_ite =
    | Ite of int * Bdd.t * Bdd.t
    | Bool of bool


  open S

  let singletons = ref Ptmap.empty

  let man = Bdd.init nb_bits
  let ff = Bdd.ff man
  let tt = Bdd.tt man

  let empty = ff

  let is_empty s = s = empty

  let singleton elem : Bdd.t =
    try Ptmap.find elem !singletons
    with Not_found ->
      let rec bdd_of_elem' current_bit current_val =
        if current_bit >= nb_bits
        then
          if current_val = 0
          then tt
          else invalid_arg "singleton: not enough bits"
        else
          let half = current_val lsr 1
          and var =
            if (current_val land 1) == 1 (* is_odd *)
            then Bdd.ithvar man current_bit
            else Bdd.nithvar man current_bit
          in
            Bdd.bdd_and var (bdd_of_elem' (succ current_bit) half)
      in
      let bdd = bdd_of_elem' 0 elem
      in
        singletons := Ptmap.add elem bdd !singletons;
        bdd


  let union set1 set2 = Bdd.bdd_or set1 set2

  let inter set1 set2 = Bdd.bdd_and set1 set2

  let add elem set = union set (singleton elem) 

  let mem v s =
    let s1 = Bdd.is_true (Bdd.bdd_implies (singleton v) s)
    and s2 = s = (add v s)
    in
      if s1 <> s2 then assert false;
      s1

  (* let compare = Pervasives.compare in *)
  let equal = (=)

  let elements : t -> int list =
    let cache_elements = ref PMap.empty
    in function set ->
      let rec inspect set =
        if Bdd.is_true set
        then Bool true
        else if Bdd.is_false set
        then Bool false
        else Ite (Bdd.var set,
                  (Bdd.low set),
                  (Bdd.high set))
      in
      let pow2 x = 1 lsl x in
        (* [pow2 x] returns 2 power x. *)
      let rec to_list cur_bit set =
        try PMap.find (cur_bit,set) !cache_elements
        with Not_found ->
          let res =
            match inspect set with
              | Bool true -> [0]
              | Bool false -> []
              | Ite (var,ff,tt) ->
                  let (ff,tt) =
                    if cur_bit < var
                    then set,set
                    else (assert (cur_bit = var); ff,tt)
                  in
                  let (ffl,ttl) =
                    to_list (succ cur_bit) ff,
                    to_list (succ cur_bit) tt
                  in
                    ffl @ (List.map ((+) (pow2 cur_bit)) ttl)
          in
            cache_elements := PMap.add (cur_bit,set) res !cache_elements;
            res
      in
        to_list 0 set

end

(*   in { *)
(*       ClassDomain.empty = empty; *)
(*       ClassDomain.is_empty = is_empty; *)
(*       ClassDomain.singleton = singleton; *)
(*       ClassDomain.mem = mem; *)
(*       ClassDomain.elements = elements; *)
(*       ClassDomain.inter = inter; *)
(*       ClassDomain.union = union; *)
(*       ClassDomain.add = add; *)
(*       ClassDomain.equal = equal; *)
(*     } *)

(* (\* register the module into classDomain *\) *)
(* let () = *)
(*   ClassDomain.bdd_init_function := Some init *)
