(* Generation of bytecode from lambda terms *)

open Lambda
open Instruct

val compile_implementation: lambda -> instruction list
val compile_phrase: lambda -> instruction list * instruction list

