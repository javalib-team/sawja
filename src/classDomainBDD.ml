
module Make (S:sig val nb_bits:int end) = struct

  module IntSet = BDDSet.Make(S)

  let class_names : JBasics.class_name Ptmap.t ref = ref Ptmap.empty

  type t = Set of IntSet.t | Bot | Top
  type analysisID = unit
  type analysisDomain = t

  let bot = Bot
  let empty = Set IntSet.empty
  let isBot = (=) Bot

  let singleton cn =
    let ci = JBasics.cn_hash cn in
      class_names := Ptmap.add ci cn !class_names;
      Set (IntSet.singleton ci)

  let is_empty = function
    | Bot -> true
    | Set s -> IntSet.is_empty s
    | Top -> false

  let mem cn = function
    | Bot -> false
    | Top -> true
    | Set s -> IntSet.mem (JBasics.cn_hash cn) s

  let set_size = function
    | Bot -> 0
    | Set s -> List.length (IntSet.elements s)
    | Top -> raise (Invalid_argument "set_size")

  let meet v1 v2 = match v1,v2 with
    | Set s1, Set s2 -> Set (IntSet.inter s1 s2)
    | Bot, _
    | _ , Bot -> Bot
    | Set _ as v, Top
    | Top, (Set _ as v) -> v
    | Top, Top -> Top

  let of_set s =
    let set =
      JBasics.ClassSet.fold
        (fun cn iset ->
           let ci = JBasics.cn_hash cn in
             class_names := Ptmap.add ci cn !class_names;
             IntSet.add ci iset)
        s
        IntSet.empty
    in Set set

  let to_set = function
    | Set s ->
        List.fold_left
          (fun cs ci ->
             let cn = Ptmap.find ci !class_names in
               JBasics.ClassSet.add cn cs)
          JBasics.ClassSet.empty
          (IntSet.elements s)
    | Bot | Top -> invalid_arg "to_set"

  let join ?(modifies=ref false) v1 v2 =
    match v1,v2 with
      | Top, _
      | _, Bot ->
          v1
      | _, Top
      | Bot,_ ->
          modifies:=true;
          v2
      | Set s1, Set s2 ->
          let s = IntSet.union s1 s2
          in
            if IntSet.equal s s1
            then v1
            else
              (modifies := true;
               if IntSet.equal s s2
               then v2
               else Set s)

  let join_ad = join

  let equal v1 v2 = match v1,v2 with
    | Bot, Bot
    | Top, Top -> true
    | Set s1, Set s2 -> IntSet.equal s1 s2
    | _, _ -> false


  let get_analysis () v = v

  let pprint fmt = function
    | Bot -> Format.pp_print_string fmt "Bot"
    | Top -> Format.pp_print_string fmt "Top"
    | Set v ->
        Format.pp_print_string fmt "{";
        List.iter
          (fun cni ->
             let cn = Ptmap.find cni !class_names in
               Format.pp_print_string fmt (Javalib.JPrint.class_name cn ^";"))
          (IntSet.elements v);
        Format.pp_print_string fmt  "}"

end
