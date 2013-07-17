open JCFADom
open Javalib_pack
open JBasics
open Javalib
open JPrintHtml

let pp_var_from_PP pp = 
  let (cn,ms) = 
    cms_split ((JBirPP.get_meth pp).cm_class_method_signature)
  in
  let pc = JBirPP.get_pc pp in
    `PP ((),cn,ms,pc)



let pp_annot prog state = 
  fun cn ms pc -> 
    let nd = JProgram.get_node prog cn in
    let cm = JProgram.get_concrete_method nd ms in
    let pp = JBirPP.get_pp nd cm pc in 
    let pp_var = pp_var_from_PP pp in
    let localvar = CFAState.get_PP state pp_var in
      [(AbLocals.to_string localvar)]

let field_annot state =
  fun cn fs ->
    let f_var = `Field ((), cn, fs) in
    let fs_ab = CFAState.get_field state f_var in
    let buf = Buffer.create 200 in
    let fmt_buf = Format.formatter_of_buffer buf in
      AbField.pprint fmt_buf fs_ab;
      Format.pp_print_flush fmt_buf ();
      [Buffer.contents buf]


let method_annot state = 
  fun cn ms ->
    let m_var = `Method ((), cn, ms) in
    let ms_ab = CFAState.get_method state m_var in
    let buf = Buffer.create 200 in
    let fmt_buf = Format.formatter_of_buffer buf in
      AbMethod.pprint fmt_buf ms_ab;
      Format.pp_print_flush fmt_buf ();
      [Buffer.contents buf]

let annot prog state = {
  p_class = (fun _ -> []);
  p_field = field_annot state;
  p_method = method_annot state;
  p_pp = pp_annot prog state;
}



let print prog state dir = 
  JBir.print_program ~info:(annot prog state) prog dir


