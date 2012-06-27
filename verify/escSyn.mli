open Types
open Typedtree
open Path

type validity = Valid | Invalid | Unknown | HighFailure | Timeout

type bool_info = Ptrue | Pfalse | Pothers

val trace : string -> ('a -> unit) -> 'a -> unit
val getop : Path.t -> string
val is_pattern_true : pattern -> bool_info
val is_expression_true : expression -> bool_info
val is_expression_bool : expression -> bool
val is_expression_prop : expression -> bool
val is_expression_argable : expression -> bool
val pre_processing : expression -> expression
val subst : Ident.t list -> (Ident.t, Ident.t) Tbl.t -> expression -> expression
val rename_boundvars : (Ident.t, Ident.t) Tbl.t -> expression -> expression
