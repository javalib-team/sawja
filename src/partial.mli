(** Partial class hierarchy for fast subclass test *)

type t

(** [make cp l] creates a new partial hierarchy using classpath [cp] 
    and adding all class names found in the list [l]. *)
val make : Javalib.class_path -> JBasics.class_name list -> t

(** [add h cn] return an updated partial hierarchy where the class of
    class name [cn] has been added to [h]. An interface name is handled 
    as [JBasics.java_lang_object]. *)
val add : t -> JBasics.class_name -> t

exception UnknownClass of JBasics.class_name

(** [is_sub_class h cn1 cn2] tests if [cn1] is a sub class of [cn2]
    in the given partial hierarchy [h]. Raise [UnknownClass cn1] if
    [cn1] has not been added to [h] before (using function [add]). *)
val is_sub_class : t -> JBasics.class_name -> JBasics.class_name -> bool

