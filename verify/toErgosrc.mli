open Why_ptree
open Types
open Typedtree

type error = 
  | Nonsimple_expression
  | Partial_function 
  | Not_lambda
  | Pattern_not_ident
  | Pattern_not_shallow
  | Argument_is_none
  | Expression_not_ident
  | Structure_not_convertable
  | Constant_not_convertable
  | Pattern_not_convertable
  | Expression_not_convertable
  | Type_not_convertable
  | Not_toplevel_function
  | Pattern_to_lexpr

exception Error of Location.t * error

val def_to_axioms : (pattern * expression) list -> decl list
val toGoal : expression -> decl 
val toGoal_neg :  expression -> decl 
val toAxiom : expression -> pattern -> decl 
val toAxiom_neg : expression -> pattern -> decl 
val type_to_typelogic : loc -> Ident.t * type_declaration -> decl list
val out_ergotasks : out_channel -> decl list -> unit
val report_error: Format.formatter -> error -> unit
val is_expression_ergoble: expression -> bool
val is_scrutinee_ergoble: expression -> bool
val bound_vars_to_logic : pattern -> decl list
val toGoal_eq : expression -> decl
val toGoal_neq : expression -> decl
