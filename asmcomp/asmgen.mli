(* From lambda to assembly code *)

val compile_implementation: string -> Lambda.lambda -> unit
val compile_phrase: Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: error -> unit


