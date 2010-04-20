open Javalib
open JBasics
open JCode

(* TODO : 
   - remove the Ptset and use only intervals ?
*)

exception InlineSubroutineFailed

let lift_cm f cm =
  match cm.cm_implementation with
    | Native -> ()
    | Java code -> f (Lazy.force code)
	
let iter_code f = function
  | JInterface i ->
      begin
	match i.i_initializer with
	  | None -> ()
	  | Some cm -> lift_cm f cm
      end
  | JClass c ->
      MethodMap.iter
	(fun _ jm -> 
	   match jm with
	     | ConcreteMethod cm -> lift_cm f cm
	     | _ -> ()) c.c_methods

let array_exists f t =
  let n = Array.length t in
  let rec aux i = i<n && (f t.(i) || aux (i+1)) in
    aux 0

let array_fold f t b =
  let n = Array.length t in
  let rec aux i =
    if i<n 
    then f i t.(i) (aux (i+1)) 
    else b in
    aux 0

(* all beginning of subroutines (astore ...) *)
(** [get_jsr code] returns a couple composed of a set containing the
    target pp of Jsr instructions and a associative list that associate
    the pp of the Jsr instruction and its target.*)
let get_jsr code =
  array_fold 
    (fun i op (jsr,l) ->
       match op with 
	 | OpJsr j -> Ptset.add (i+j) jsr, (i,i+j)::l
	 | _ -> jsr, l)
    code.c_code 
    (Ptset.empty, [])

(* for each pc, all predecessor (except pc-1) *)
(** [compute_jump_target code] returns an array of Pset containing all
    jump sources for a jump target (Handlers, Goto, If, ... except Jsr and Ret
    jump...). Index of array is the pp target of jump and Pset
    contained represents all possibles pp sources for the jump to pp
    target.*)
let compute_jump_target code =
  let jump_target = Array.make (Array.length code.c_code) Ptset.empty in
  let add i j = jump_target.(j) <- Ptset.add i jump_target.(j) in
    (* add numbers from i to j-1 to set if i<j*)
  let rec add_interval i j set =
    if i>=j then set
    else 
      if code.c_code.(i)<>OpInvalid
      then add_interval (i+1) j (Ptset.add i set) 
      else add_interval (i+1) j set 
  in    
    (* add all pp covered by the handler e in jump_target for start pp of the handler e*)
  let add_handler e =
    jump_target.(e.e_handler) <- add_interval e.e_start e.e_end jump_target.(e.e_handler) 
  in
    (* execute add handler for all code handlers (contained in exception table)*)
    List.iter add_handler code.c_exc_tbl;
    (* add all sources of jump for the targets of jump in jump_target*)
    Array.iteri
      (fun i instr ->
	 match instr with
	   | OpIf (_, n) 
	   | OpIfCmp (_, n) 
	   | OpGoto n -> add i (i+n)
	   | OpTableSwitch (default, _, _, table) ->
	       List.iter (fun n -> add i (i+n)) (default :: Array.to_list table)
	   | OpLookupSwitch (default, npairs) ->
	       List.iter (fun n -> add i (i+n)) (default :: List.map snd npairs)
	   | _ -> ())
      code.c_code;
    jump_target

(* direct predecessor *)
let rec pred code i =
  assert (i>0);
  if code.c_code.(i-1) = OpInvalid 
  then pred code (i-1)
  else code.c_code.(i-1)

(* test if pc+1 is a successor *)
let no_direct_succ = function
  | OpGoto _
  | OpReturn _
  | OpThrow -> true
  | _ -> false

(* next valid opcode *)
let next_opcode c i =
  try
    let k = ref (i+1) in
      while c.(!k)=OpInvalid do incr k done;
      [!k]
  with _ -> []

let next_after_jsr c i =
  try
    let k = ref (i+1) in
      while c.c_code.(!k)=OpInvalid do incr k done;
      !k
  with _ -> assert false

(* successors, except handlers *)    
let normal_next code i = 
  match code.c_code.(i) with
  | OpIf (_, n) 
  | OpIfCmp (_, n) -> (next_opcode code.c_code i)@[i+n]
  | OpGoto n -> [i+n]
  | OpJsr _ 
  | OpRet _  -> raise InlineSubroutineFailed
  | OpBreakpoint 

  | OpInvalid -> assert false
  | OpTableSwitch (default, _, _, table) ->
      List.map (( + ) i) (default :: Array.to_list table)
  | OpLookupSwitch (default, npairs) ->
      List.map (( + ) i) (default :: List.map snd npairs)
  | OpReturn _ -> []
  | OpThrow -> []
  | _ -> next_opcode code.c_code i

(* all handlers that may catch exception thrown in [i] (filtered by filter) *)
let add_handlers filter code i =
  let handlers = code.c_exc_tbl in
  let handlers = List.filter (fun e -> e.e_start <= i && i < e.e_end) handlers in
  let handlers = List.map (fun e -> e.e_handler) handlers in
    List.filter filter handlers
      
(* all successors (handlers filtered by filter) *)
let next filter_handler code i =
  (add_handlers filter_handler code i)@(normal_next code i)

(* test if a OpRet may be reachable from [i] *)
let may_reach_a_ret code i =
  let rec aux (set,may) i =
    if Ptset.mem i set then (set,may)
    else
      match code.c_code.(i) with
	| OpRet _ -> (set,true)
	| _ -> 
	    List.fold_left aux 
	      (Ptset.add i set,may)
	      (next (fun _ -> true) code i)
  in
    snd (aux (Ptset.empty,false) i)

(* put direct targets in jumps *)
let change_target i = function
  | OpIf (c, n) -> OpIf (c,i+n)
  | OpIfCmp (c, n) -> OpIfCmp (c,i+n)
  | OpJsr n -> OpJsr (i+n)
  | OpGoto n -> OpGoto (i+n)
  | OpTableSwitch (default, l, h, table) ->
      OpTableSwitch (default+i,l,h,Array.map ((+)i) table)
  | OpLookupSwitch (default, npairs) ->
      OpLookupSwitch (default+i,List.map (fun (x,y) -> (x,y+i)) npairs)
  | op -> op

(* print code cith direct jumps *)
let print_code code out = 
  Array.iteri
    (fun i op -> 
       if op<>OpInvalid
       then Printf.fprintf out "%d: %s\n"
	 i (JPrint.jopcode (change_target i op)))
    code.c_code;
  List.iter 
    (fun e -> Printf.fprintf out " %s\n" (JPrint.exception_handler e))
    code.c_exc_tbl

let next_pc c i =
  try
    let k = ref (i+1) in
      while c.(!k)=OpInvalid do incr k done;
      !k
  with _ -> i

(** [explore_from code i] returns the couple composed by the set of OpRet pp at exit points and the set of prog points reachable from [i] in code [code].*)
let explore_from code i =
  match code.c_code.(i) with
    | OpStore (`Object,x) ->
	let rec aux (ex,set) i =
	  if Ptset.mem i set then (ex,set) 
	  else
	    match code.c_code.(i) with
	    | OpJsr _ -> raise InlineSubroutineFailed (* no nested subrountines please *) 
	    | OpRet y -> 
		begin 
		  if not (x=y) then raise InlineSubroutineFailed;
		  (Ptset.add i ex,set)
		end
	    | OpStore (_,y)
	    | OpLoad (_,y) when x=y -> raise InlineSubroutineFailed (* don't touch the ret adress please *)
	    | _ -> 
		List.fold_left aux  
		  (ex,Ptset.add i set)
		  (next (may_reach_a_ret code) code i)
		  (* we remove handlers that don't reach OpRet instructions *)
	in
	  aux (Ptset.empty,Ptset.add i Ptset.empty) (next_pc code.c_code i)
    | _ -> assert false

(* same as above but only one exit is allowed *)
let explore_from code i =
  let (exxit,set) = explore_from code i in
    if not (Ptset.cardinal exxit = 1) then raise InlineSubroutineFailed;
    (Ptset.choose exxit,set)

(** [make_intervals_union code set] try to build an interval from the
    subroutine pp contained in [set].  @Raise InlineSubroutineFailed if
    more than one interval is needed to include all pp of [set]*)
let make_intervals_union code set =
  (* if i+1 is a pp of code, then return first not OpInvalid from i+1. if not return i+1*)
  let rec next i = 
    if (i+1<Array.length code.c_code) then
      if code.c_code.(i+1) = OpInvalid 
      then next (i+1)
      else i+1 
    else i+1 in
    (* returns the last contigous pp included in
       [set] from [i] *)
  let rec fill set i = 
    let next = next i in
      if Ptset.mem next set
      then fill (Ptset.remove next set) next
      else next-1, set in 
  let rec next_int set l =
    if Ptset.is_empty set then l
    else
      let i = Ptset.min_elt set in
      let (j,set) = fill (Ptset.remove i set) i in
	next_int set ((i,j)::l) 
  in
  let intervals = next_int set [] in
    match intervals with
      | [x] -> x
      | _ -> raise InlineSubroutineFailed
	  

let rec print_list_sep_rec sep pp = function
  | [] -> ""
  | x::q -> sep^(pp x)^(print_list_sep_rec sep pp q)

let print_list_sep sep pp = function
  | [] -> ""
  | x::q -> (pp x)^(print_list_sep_rec sep pp q)
      
let print_set s =
  "{"^(print_list_sep "," string_of_int (Ptset.elements s))^"}"

let inter (a,b) (c,d) =
  assert (a<=b);
  assert (c<=d);
  let low = max a c in
  let high = min b d in
    if low <= high then Some (low,high) else None


let inline code instrs subroutines =
  (* Should not be size+pps_sub.size(+1 for ret ?) instead ?*)
  let new_size =
    List.fold_left
      (fun size (_,start) -> 
	 let (_,_,_,size_subroutine,_,_) = subroutines start in
	   size + size_subroutine
      ) (Array.length code.c_code) instrs in
  let new_code = Array.make new_size OpInvalid in
    Array.blit code.c_code 0 new_code 0 (Array.length code.c_code);
    let current = ref (Array.length code.c_code) in
    let handlers = ref [] in
      (* Create copy of subroutine code for each Jsr instr src_pp and
	 necessary handlers linked to subroutines ...*)
      List.iter
	(fun (i,start) ->
	   let (code_start,codes,code_exit,
		size,handlers_mine,handlers_global) = subroutines start in
	     Ptset.iter (fun i -> new_code.(i) <- OpInvalid) codes;
	     new_code.(code_exit) <- OpInvalid;
	     (* copy all suroutine code at the end of new code*)
	     Ptset.iter
	       (fun i -> 
		  if i <> code_start then
		    new_code.(!current+i-code_start) <- code.c_code.(i)
	       ) codes;
	     (* Replace Jsr instr by Goto copy of jsr code for [i] src_pp*)
	     new_code.(i) <- 
	       OpGoto 
	       ((next_after_jsr code code_start)+ !current-code_start-i);
	     (* Replace Ret instr by Goto [i] src_pp*)
	     new_code.(!current+code_exit-code_start) <- 
	       OpGoto 
	       ((next_after_jsr code i)-(!current+code_exit-code_start));
	     handlers := 
	       (List.map (fun e -> 
			    {
			      e_start = e.e_start - code_start + !current;
			      e_end = e.e_end - code_start + !current;
			      e_handler = e.e_handler - code_start + !current;
			      e_catch_type = e.e_catch_type
			    }) handlers_mine)
	     @ (List.map (fun e -> 
			    let (e_start,e_end) = 
			      match inter (code_start,code_start+size) (e.e_start,e.e_end) with
				| None -> assert false
				| Some x -> x in
			      {
				e_start = e_start - code_start + !current;
				e_end = e_end - code_start + !current;
				e_handler = e.e_handler;
				e_catch_type = e.e_catch_type
			      }
			 ) handlers_global)
	     @ !handlers;
	     current  := !current + size
	) 
	instrs;
      
      let next = next_pc new_code in
      let first_valid pp = 
	try
	  if new_code.(pp)=OpInvalid
	  then next pp
	  else pp
	with _ -> next pp
      in
	{
	  c_max_stack = code.c_max_stack;
	  c_max_locals = code.c_max_locals;
	  c_code = new_code;
	  (* Remove not valid anymore handlers (those included in
	     subroutine code and those who catch old subroutine code. Add
	     the new handlers created.*)
	  c_exc_tbl = 
	    (List.filter 
	       (fun e -> 
		  (new_code.(e.e_handler)<>OpInvalid)&&
		    (new_code.(e.e_start)<>OpInvalid)  
	       ) 
	       code.c_exc_tbl)
	    @(List.map
		(fun e -> 
		   (* TODO: ask david if he is ok with this fix ! (it
		      was not ... see first_valid function bug fix)*)
		   {
		     e_start = first_valid e.e_start;
		     e_end = first_valid e.e_end;
		     e_handler = e.e_handler;
		     e_catch_type = e.e_catch_type
		   }) !handlers);
	  c_line_number_table = None;
	  c_local_variable_table = None;
	  c_stack_map_midp = None;
	  c_stack_map_java6 =  None;
	  c_attributes = code.c_attributes
	}

(** [scan_subroutine code jump_target start] returns (start pp, pp included, pp of Ret instr, length of subroutine, handlers (that catch sub) which code is included in subroutines, ohter handlers (that catch sub)).
    @Raise InlineSubroutineFailed if all pp of sub are not contiguous.*)
let scan_subroutine code jump_target start =
  (* start must be only reachable from the OpJsr *)
  if not (no_direct_succ (pred code start)) then raise InlineSubroutineFailed;
  if not (Ptset.is_empty jump_target.(start)) then raise InlineSubroutineFailed;
  (* (pp of ret instruction, pp of instructions reachable
     that could lead to a ret instruction)*)
  let (ret,succs) = explore_from code start in
    (* returns an interval if all instructions in subroutines
       are contiguous instructions, raise exception if not. *)
  let (start,endd) = make_intervals_union code succs in
    Ptset.iter
      (fun j ->
	 (* no jumps in the subroutine from outside *)
	 if not (Ptset.subset jump_target.(j) succs) then raise InlineSubroutineFailed)
      succs;

    let handlers = (* all handlers that catch the subroutine instructions*)
      List.filter
	(fun e ->  
	   Ptset.exists
	     (fun i -> e.e_start <= i && i < e.e_end)
	     succs) 
	code.c_exc_tbl in
      (* handlers which code is included in subrountine, others *)
    let (handlers_mine,handlers_global) = 
      List.partition
	(fun e -> Ptset.mem e.e_handler succs || e.e_handler = ret)
	handlers 
    in
      (start,succs,ret,(max endd (ret+1))-start,handlers_mine,handlers_global)


let nb_meth = ref 0

let write code num old_new =
  let out = open_out (Printf.sprintf "jsr/m%d_%s.txt" num old_new) in
    print_code code out;
    close_out out

let time = ref 0.
let start_inlining () =
  Printf.printf "starting inlining...";
  time := Unix.time ()
let end_inlining () =
  Printf.printf " done in %fs\n" (Unix.time () -. !time);
  flush stdout
		
let test target = Javalib.iter
  (iter_code
     (fun code ->
	let (jsr,instrs) = get_jsr code in
	  if not (Ptset.is_empty jsr) then 
	    begin
	      start_inlining ();
	      let jump_target = compute_jump_target code in
	      let subroutines = Ptset.fold
				  (fun i map ->
				     Ptmap.add i (scan_subroutine code jump_target i) map)
				  jsr Ptmap.empty in
		end_inlining ();
		write code !nb_meth "old";
		let new_code = inline code instrs (fun i -> Ptmap.find i subroutines) in
		  write new_code !nb_meth "new";
		  incr nb_meth
	    end
     ))
  target;
  Printf.printf "%d methods with subroutines\n" !nb_meth

let inline code = 
  (* couple (Pset (Jsr targets), List (jsr_src_pp,jsr_trg_pp))*)
  let (jsr,instrs) = get_jsr code in
    if Ptset.is_empty jsr then Some code
    else
      try
	(* for all jumps (handlers, goto, ... except jsr) Array(trg_pp, Pset(src_pp))*)
	let jump_target = compute_jump_target code in
	let subroutines = 
	  Ptset.fold
	    (fun i map ->
	      Ptmap.add 
		 i (scan_subroutine code jump_target i) map)
	    jsr Ptmap.empty 
	in
	Some (inline code instrs (fun i -> Ptmap.find i subroutines))
      with
	  InlineSubroutineFailed -> None


