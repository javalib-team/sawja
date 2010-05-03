open Javalib_pack

(** [inline code] returns [Some code'] where [code'] is an equivalent
    version of [code] without subroutines. In case [code] does not
    contain subroutines at all, [code'] is physically equal to
    [code]. If [code] contains too complex subroutines (for example,
    nested subroutines), then [inline code] returns [None]. *)
val inline : JCode.jcode -> JCode.jcode option
