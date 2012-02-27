(*
 * This file is part of SAWJA
 * Copyright (c)2010 David Pichardie (INRIA)
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


type workset_strategy =
  | Decr (* program points are chosen in decreasing order *)
  | Incr (* program points are chosen in increasing order *)

type ('dom,'tf) manager = {
  bot : 'dom;
  join : 'dom -> 'dom -> 'dom; (* join at current program point *)
  leq :  'dom -> 'dom -> bool;
  normalize :  'dom -> 'dom; (* normalize after each join *)
  eval : 'tf -> 'dom -> 'dom;
  size : int; (* intra analysis will be computed for points in [0 .. size-1] *)
  workset_strategy : workset_strategy;
  cstrs : (int * 'tf * int) list;
  init_points : int list; (* entry points in the equation system *)
  init_value : int -> 'dom; (* constant contraints on entry points *)
  verbose : bool;
  dom_to_string : 'dom -> string;
  transfer_to_string : 'tf -> string
}

let rec f_list f acc = function
    [] -> acc
  | x::q -> f_list f (f acc x) q

let run man =
  let join_list _ = f_list man.join man.bot in
  let succs = Array.make man.size Ptset.empty in
  let transfs = Array.make man.size [] in
  let _ = List.iter
	    (fun (i,tf,j) -> 
	       succs.(i) <- Ptset.add j succs.(i);
	       transfs.(j) <- (tf,i)::transfs.(j))
	    man.cstrs in
  let rec workset_init n = 
    if n<0 then Ptset.empty 
    else Ptset.add n (workset_init (n-1)) in
  let res = Array.make man.size man.bot in
  let _ = List.iter (fun i -> res.(i) <- man.init_value i) man.init_points in
  let choose ws = 
    match man.workset_strategy with
      | Incr -> Ptset.min_elt ws 
      | Decr -> Ptset.max_elt ws in
  let rec loop ws =
    if Ptset.is_empty ws then ()
    else
      let i = choose ws in
      let ws = Ptset.remove i ws in
      let v = join_list i (List.map (fun (tf,j) -> man.eval tf res.(j)) transfs.(i)) in
      let v = if (List.mem i man.init_points) then man.join (man.init_value i) v else v in
	if man.verbose then begin
	  Printf.printf "choosing %d...\n" i;
	  Printf.printf "%d transfert functions:\n" (List.length transfs.(i));
	  List.iter (fun (tf,j) -> Printf.printf "  %d: %s\n" j (man.transfer_to_string tf)) transfs.(i);
	  Printf.printf "old(i): ";
	  let s = (man.dom_to_string res.(i)) in
	    Printf.printf "%s\n" s;
	  Printf.printf "new(i) (before normalization): ";
	  let s = (man.dom_to_string v) in
	    Printf.printf "%s\n" s;
	end;
      let v = man.normalize v in
	if man.verbose then begin
	  Printf.printf "new(i): ";
	  let s = (man.dom_to_string v) in
	    Printf.printf "%s\n" s;
	end;
	if man.leq v res.(i) then loop ws
	else begin
	  if man.verbose then begin
	    Printf.printf "adding";
	    Ptset.iter (Printf.printf " %d") succs.(i);
	    print_newline ()
	  end;
	  res.(i) <- v;
	  loop (Ptset.union ws succs.(i)) 
	end in
    loop (workset_init (man.size-1));
    (fun i -> res.(i))
