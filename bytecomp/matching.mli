(* Compilation of pattern-matching *)

open Typedtree
open Lambda

val for_function:
        Location.t -> Ident.t -> (pattern * lambda) list -> lambda
val for_trywith:
        Ident.t -> (pattern * lambda) list -> lambda
val for_let:
        Location.t -> Ident.t -> pattern -> lambda -> lambda
