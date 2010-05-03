open Javalib_pack

module Make (S:sig end) = struct
  module ClassSet = JBasics.ClassSet

  type t = Set of ClassSet.t | Bot | Top

  type analysisID = unit
  type analysisDomain = t

  let bot = Bot
  let empty = Set ClassSet.empty
  let isBot = (=) Bot

  let singleton c = Set (ClassSet.singleton c)

  let is_empty = function
    | Bot -> true
    | Set s -> ClassSet.is_empty s
    | Top -> false

  let mem c = function
    | Bot -> false
    | Top -> true
    | Set s -> ClassSet.mem c s

  let set_size = function
    | Bot -> 0
    | Set s -> ClassSet.cardinal s
    | Top -> raise (Invalid_argument "set_size")

  let filter f = function
    | Set s -> Set (ClassSet.filter f s)
    | Bot -> Bot
    | Top -> Top

  let meet v1 v2 = match v1,v2 with
    | Set s1, Set s2 -> Set (ClassSet.inter s1 s2)
    | Bot, _
    | _, Bot -> Bot
    | Set _ , Top -> v1
    | _ , Set _ -> v2
    | Top, Top -> Top

  let of_set s = Set s

  let to_set = function
    | Set s -> s
    | Bot | Top -> invalid_arg "to_set"

  let join ?(modifies=ref false) v1 v2 =
    match v1,v2 with
      | _, Bot -> v1
      | Bot,_ ->
          modifies:=true;
          v2
      | Top, _ -> v1
      | _, Top ->
          modifies := true;
          v2
      | Set s1, Set s2 ->
          let s = ClassSet.union s1 s2
          in
            if ClassSet.equal s s1
            then v1
            else
              (modifies := true;
               if ClassSet.equal s s2
               then v2
               else Set s)

  let join_ad = join

  let equal v1 v2 = match v1,v2 with
    | Bot, Bot -> true
    | Top, Top -> true
    | Set s1, Set s2 ->
        s1==s2 || ClassSet.equal s1 s2
    | _, _ -> false


  let get_analysis () v = v

  let pprint fmt = function
    | Bot -> Format.pp_print_string fmt "Bot"
    | Top -> Format.pp_print_string fmt "Top"
    | Set v ->
        Format.pp_print_string fmt "{";
        ClassSet.iter
          (fun cn ->
             Format.pp_print_string fmt (Javalib.JPrint.class_name cn ^";"))
          v;
        Format.pp_print_string fmt  "}"

end
