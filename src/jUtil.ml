(* 
 * Fold a function f on an accumulator x0 and an array t 
 * f a [| b0 ; b1 ; ... ; bn |] --->  f 0 b0 (f 1 b1 (f ... (f n bn x0) ...) )
 *)
let foldi f x0 t =
  let n = Array.length t in
  let rec aux i =
    if i>=n then x0
    else f i t.(i) (aux (i+1)) in
    aux 0

let for_all f t =
  let n = Array.length t in
  let rec aux i =
    if i>=n then true
    else f i t.(i) && (aux (i+1)) in
    aux 0

(* [find_index x l] return the position of element [x] in list [l]. 
   @raise Not_found if [x] does not appear in [l]. *)
let find_index x l = 
  let rec aux i = function
      [] -> raise Not_found
    | y::q ->
	if x=y then i 
	else aux (i+1) q in
    aux 0 l

let print_list_sep_map sep f l =
  match l with
    | [] -> ""
    | [x] -> Printf.sprintf "%s" (f x)
    | x::q -> Printf.sprintf "%s%s" (f x) (List.fold_right (fun x s -> sep^(f x)^s) q "")	 

let print_list_sep sep = print_list_sep_map sep (fun x -> x)


(* Containers. *)

module GenericSet ( S : sig type t end ) =
struct
  type elt = int * S.t
  type t = elt Ptmap.t

  let empty = Ptmap.empty
  let is_empty = Ptmap.is_empty
  let mem e m = Ptmap.mem (fst e) m
  let add e m = Ptmap.add (fst e) e m
  let singleton e = Ptmap.add (fst e) e empty
  let remove e m = Ptmap.remove (fst e) m
  let union m1 m2 = Ptmap.merge_first m1 m2
  let diff m1 m2 = Ptmap.diff (fun _ _ -> true) m1 m2
  let equal m1 m2 = 0 == (compare m1 m2)
  let elements m = Ptmap.fold (fun _ e l ->  e :: l) m []
  let cardinal m = Ptmap.cardinal m
  let iter f m = Ptmap.iter (fun _ e -> f e) m
  let fold f m b = Ptmap.fold (fun _ e b -> f e b) m b
  let exists f m = Ptmap.exists (fun _ e -> f e) m
  let filter f m = Ptmap.filter f m
  let inter m1 m2 = Ptmap.inter m1 m2
  (* val partition : ('a -> bool) -> 'a t -> 'a t * 'a t *)
  (* val choose_and_remove : *)
end

module GenericMap ( S : sig type t end ) =
struct
  type key = int * S.t
  type 'a t = (key * 'a) Ptmap.t

  let empty = Ptmap.empty
  let is_empty = Ptmap.is_empty
  let add key o m = Ptmap.add (fst key) (key, o) m
  let modify key f m = Ptmap.modify (fst key)
    (fun x -> match x with
       | None -> (key, f None)
       | Some (_,a) -> (key, f (Some a))
    ) m
  let find key m = snd (Ptmap.find (fst key) m)
  let remove key m = Ptmap.remove (fst key) m
  let mem key m = Ptmap.mem (fst key) m
  let iter f m = Ptmap.iter (fun _ (k,d) -> f k d) m
  let map f m = Ptmap.map (fun (k,d) -> (k, f d)) m
  let mapi f m = Ptmap.mapi (fun _ (k,d) -> (k, f k d)) m
  let fold f m e = Ptmap.fold (fun _ (k,d) -> f k d) m e
  let compare f m1 m2 = Ptmap.compare (fun a b -> f (snd a) (snd b)) m1 m2
  let equal f m1 m2 = Ptmap.equal (fun a b -> f (snd a) (snd b)) m1 m2
  let merge f m1 m2 = Ptmap.merge (fun a b -> (fst a), f (snd a) (snd b)) m1 m2
  let choose_and_remove m =
    let (_,(k,d),m) = Ptmap.choose_and_remove m in
      (k, d, m)
  let filter f m =
    Ptmap.filter (fun (_,d) -> f d) m
  let filteri f m =
    Ptmap.filter (fun (k,d) -> f k d) m
  let key_elements m =
    Ptmap.fold (fun _ (k,_) l -> k :: l) m []
  let value_elements m =
    Ptmap.fold (fun _ (_,b) l -> b :: l) m []
  let elements m =
    Ptmap.fold (fun _ e l -> e :: l) m []
end

open Javalib_pack.JBasics

module MaptoSet ( S : sig type t end )
  ( GMap : GenericMapSig with type key = S.t )
  ( GSet : GenericSetSig with type elt = S.t ) =
struct
  let to_set m =
    GMap.fold (fun k _ s -> GSet.add k s) m GSet.empty
end
