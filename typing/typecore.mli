(* Type inference for the core language *)

open Asttypes
open Typedtree

val type_binding:
        Env.t -> rec_flag ->
          (Parsetree.pattern * Parsetree.expression) list -> 
            (Typedtree.pattern * Typedtree.expression) list * Env.t
val type_expression:
        Env.t -> Parsetree.expression -> Typedtree.expression

type error =
    Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * type_expr * type_expr
  | Pattern_type_clash of type_expr * type_expr
  | Multiply_bound_variable
  | Orpat_not_closed
  | Expr_type_clash of type_expr * type_expr
  | Apply_non_function of type_expr
  | Label_multiply_defined of Longident.t
  | Label_missing
  | Label_not_mutable of Longident.t
  | Non_generalizable of type_expr
  | Bad_format_letter of char

exception Error of Location.t * error

val report_error: error -> unit
