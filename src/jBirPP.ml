open Javalib_pack
open Javalib
open JProgram
open JControlFlow


type t = JBir.t PP.t
let get_class = PP.get_class
let get_meth = PP.get_meth
let get_pc = PP.get_pc
let get_pp = PP.get_pp
let get_first_pp =PP.get_first_pp
let get_first_pp_wp =PP.get_first_pp_wp
let goto_absolute =PP.goto_absolute
let goto_relative =PP.goto_relative
let equal =PP.equal
let compare =PP.compare
let hash =PP.hash
let next_instruction =PP.next_instruction
let static_pp_lookup =PP.static_pp_lookup
let to_string = PP.to_string
let pprint = PP.pprint
let get_ir = PP.get_ir

let get_code pp = 
  let meth = (get_meth pp) in
  let cn = get_name (get_class pp) in
  let ms = meth.cm_signature in
  let impl = meth.cm_implementation in
    match impl with 
      | Native -> raise (PP.NoCode (cn, ms)) (* If we are in a pp, we should have code available. *)
      | Java laz -> JBir.code (Lazy.force laz)

let get_opcode pp =
  (get_code pp).(get_pc pp)

let static_lookup program pp =
  match get_opcode pp with
    | JBir.InvokeVirtual (_,_,kind, ms,_) ->
        (match kind with
             JBir.VirtualCall obj -> 
      	 Some (static_lookup_virtual program obj ms)
           | JBir.InterfaceCall cs -> 
      	 Some (static_lookup_interface program cs ms)
        )
    | JBir.InvokeStatic (_, cs, ms,_) ->
        Some ([static_lookup_static program cs ms])
    | JBir.InvokeNonVirtual (_, _, cs, ms, _) ->
        Some ([Class (static_lookup_special program (get_class pp) cs ms)])
    | _ -> None

let handlers pp =
  let open JBir in
  let pc = (get_pc pp) in
  let cn = get_name (get_class pp) in
  let meth = (get_meth pp) in
  let ms = meth.cm_signature in
  let impl = meth.cm_implementation in
  let excs = 
    match impl with 
      | Native -> raise (PP.NoCode (cn,ms))
      | Java laz-> exc_tbl (Lazy.force laz)
  in
    List.filter (fun exc -> 
    pc >= exc.e_start && pc < exc.e_end
    ) excs

let normal_successors pp =
  match get_opcode pp with
    | JBir.Goto l -> 
        [goto_absolute pp l]
    | JBir.Ifd (_,l) -> 
        [next_instruction pp; goto_absolute pp l]
    | JBir.Throw _ 
    | JBir.Return _ -> []
    | _ -> [next_instruction pp]


  let exceptional_successors pp =
    List.map 
      (fun e -> goto_absolute pp e.JBir.e_handler) (handlers pp)
      

