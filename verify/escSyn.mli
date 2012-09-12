open Types
open Typedtree
open Path

type validity = Valid | Invalid | Unknown | HighFailure | Timeout

type bool_info = Ptrue | Pfalse | Pothers

val trace : string -> ('a -> unit) -> 'a -> unit
val mtrace : string -> unit
val print_list : ('a -> unit) -> 'a list -> unit
val getop : Path.t -> string
val get_primitive_op : expression -> string
val memPath : Path.t -> Path.t list -> bool
val add_pat_expr_list_to_tbl : (pattern * expression) list -> (Path.t, expression) Tbl.t
                     ->  (Path.t, expression) Tbl.t
val intersection : Path.t list -> Path.t list -> Path.t list
val is_pattern_true : pattern -> bool_info
val is_expression_true : expression -> bool_info
val is_expression_bool : expression -> bool
val is_expression_prop : expression -> bool
val is_expression_primitiveop : expression -> bool
val is_expression_tvalue : expression -> bool
val is_expression_argable : expression -> bool
val pre_processing : expression -> expression
val subst : Ident.t list -> (Ident.t * Ident.t) list -> expression -> expression
val rename_boundvars : (Ident.t * Ident.t) list -> expression -> expression
val pattern_to_expression : pattern -> expression
