(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda

val transl_implementation: string -> structure -> module_coercion -> lambda
val transl_toplevel_definition: structure -> lambda
