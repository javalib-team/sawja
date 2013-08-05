open Javalib_pack
open JBasics
open JProgram



let bt_compare bt1 bt2 = 
  match bt1, bt2 with
    | `Int, `Int -> 0 
    | `Int,_ -> 1
    | _, `Int  -> -1
    | `Short, `Short -> 0
    | `Short, _ -> 1
    | _, `Short -> -1
    | `Char, `Char -> 0
    | `Char, _ -> 1
    | _, `Char -> -1
    | `Byte, `Byte -> 0
    | `Byte, _ -> 1
    | _, `Byte -> -1
    | `Bool , `Bool -> 0
    | `Bool , _ -> 1
    | _, `Bool  -> -1
    | `Long , `Long -> 0
    | `Long , _ -> 1
    | _ , `Long  -> -1
    | `Float, `Float -> 0
    | `Float, _ -> 1
    | _, `Float -> -1
    | `Double, `Double -> 0
    | `Double, _ -> 1

let rec vt_compare vt1 vt2 =
  match vt1, vt2 with
    | TBasic _, TObject _ -> 1
    | TObject _, TBasic _ -> -1
    | TBasic bt1, TBasic bt2 -> bt_compare bt1 bt2
    | TObject o1, TObject o2 -> obj_compare o1 o2

and obj_compare obj1 obj2 =
  match obj1, obj2 with
    | TClass _, TArray _ -> 1 
    | TArray _, TClass _ -> -1 
    | TClass cn1, TClass cn2 -> cn_compare cn1 cn2
    | TArray vt1, TArray vt2 -> vt_compare vt1 vt2

module DicoObj = Map.Make(struct type t=object_type let compare = obj_compare end)
let cur_hash = ref 0
let dicoObj = ref DicoObj.empty

let new_hash _ =
  cur_hash := !cur_hash+1;
  !cur_hash

let get_hash obj =
  try DicoObj.find obj !dicoObj
  with Not_found -> 
    let new_hash = new_hash () in
      dicoObj := DicoObj.add obj new_hash !dicoObj;
      new_hash



module ObjectMap = GenericMap.Make (struct 
                                      type t = object_type 
                                      let get_hash = get_hash
                                    end)

module ObjectSet = GenericSet.Make (struct 
                                      type t = object_type 
                                      let get_hash = get_hash
                                    end)



  (*Return true of t1 is a primitive direct supertype of t2. 
  * from JLS 4.10.1*)
let primitive_direct_supertype t1 t2 = 
  match t1, t2 with
    | `Double, `Float
    | `Float, `Long 
    | `Long, `Int 
    | `Int, `Char
    | `Int, `Short
    | `Short, `Byte -> true
    | _ -> false
     
  (*Return true of t1 is a supertype of t2*)
let class_supertype prog t1 t2 =
  let supertype_from_class cn1 cn2 = 
    let nd1, nd2=  (get_node prog cn1), (get_node prog cn2) in
      match (nd1, nd2) with
        | Class c1, Class c2 ->
            extends_class c2 c1
        | Interface i1, Interface i2 ->
            extends_interface i2 i1
        | Class c1, Interface i2 ->
            implements c1 i2
        | _ -> false
  in
  let rec class_or_array_supertype t1 t2 =
    match t1, t2 with
      | TClass cn1, TClass cn2 ->
          supertype_from_class cn1 cn2
      | TArray (TObject vt1) , TArray (TObject vt2) ->
          (*if S and T are both reference types, then S[] >1 T[] iff S >1 T .*)
          class_or_array_supertype vt1 vt2
      | TClass cn1, TArray _ ->
          (cn_equal java_lang_object cn1 ||
           cn_equal (make_cn "java.lang.Cloneable") cn1 ||
           cn_equal (make_cn "java.io.Serializable") cn1) 
      | _ -> false
  in
    class_or_array_supertype t1 t2

  (* Return true if cn1 is the direct superclass of cn2 *)
let direct_superclass prog cn1 cn2 = 
  match super_class (get_node prog cn2) with
    | None -> false
    | Some c2 -> node_equal (Class c2) (get_node prog cn1)

(*
  (* Return true if cn1 is a direct superinterface of cn2 *)
let direct_superinterface prog cn1 cn2 = 
  List.exists
    (fun i_node -> node_equal i_node (get_node prog cn1))
    (directly_implements (get_node prog cn2))

  (*Return true of t1 is a direct supertype of t2*)
let class_direct_supertype prog t1 t2 =
  match t1, t2 with 
    | TClass cn1, TClass cn2 -> 
        let nd1, nd2=  (get_node prog cn1), (get_node prog cn2) in
          (match nd2 with
             | Interface inode2 ->
                 let super_inter2 = super_interfaces inode2 in
                   if List.is_empty super_inter2
                   then         
                     (* 3)
                      * The type object if cn2 is an interface with no direct
                      * superinterfaces.
                      **)
                     (cn_equal cn1 java_lang_object)
                   else 
                       (*
                        *The superinterfaces.
                        * *)
                       List.exists
                         (fun super_nd2 -> node_equal nd1 super_nd2)
                         super_inter2
             | Class cnode ->

                 (*
                  * 1) 
                  * t1 is a class superclass of type C1
                  * t2 is a class superclass of type C2
                  * if c1 is a direct superclass of c2 then t1 is a supertype of t2
                  *)
                 direct_superclass cn1 cn2 ||
                 (* 2)
                  * t1 is a superinterface of t2
                  * *)
                 direct_superinterface cn1 cn2
          )

    
 *)

(*Return true if t1 is a supertype of t2*)
let supertype prog t1 t2  =
  match obj_compare t1 t2 with
    | 0 -> false (*if t1 = t2 then false*)
    | _ -> class_supertype prog t1 t2

(*
 * Return true if t1 is a subtype of t2.
 * The subtypes of a type T are all types U such that T is a supertype of U, and 
 * the null type
*)
let subtype prog t1 t2 =
  match obj_compare t1 t2 with
    | 0 -> false (*if t1 = t2 then false*)
    | _ -> supertype prog t2 t1

