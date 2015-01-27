(** String lifting to the toplevel of [Lambda] expressions.
    This transformation simplifies later optimization passes, since they may be
    freed from the concern of accidentally duplicating a string and thus potentially
    changing semantics.
*)

val lift_strings_to_toplevel : Lambda.lambda -> Lambda.lambda
