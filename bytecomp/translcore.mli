(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Typedtree
open Lambda

val transl_exp: compilenv -> expression -> lambda
val transl_let:
        compilenv -> rec_flag -> (pattern * expression) list ->
          compilenv * (lambda -> lambda)
val transl_primitive: primitive_description -> lambda
val transl_exception: Ident.t -> exception_declaration -> lambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr

exception Error of Location.t * error

val report_error: error -> unit


