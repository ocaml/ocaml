open Types
open Typedtree
open Path

type validity = Valid | Invalid | Unknown | HighFailure | Timeout

type bool_info = Ptrue | Pfalse | Pothers

val getop : Path.t -> string
val is_pattern_true : pattern -> bool_info
val is_expression_true : expression -> bool_info
val is_expression_bool : expression -> bool
val is_expression_prop : expression -> bool
val is_expression_argable : expression -> bool
