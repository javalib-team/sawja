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
