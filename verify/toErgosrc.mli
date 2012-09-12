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
  | Pattern_not_convertable
  | Expression_not_convertable
  | Type_not_convertable
  | Not_toplevel_function
  | Pattern_to_lexpr

exception Error of Location.t * error

type fvalue = VBottom | VValue of lexpr

val def_to_axioms : (string * type_declaration) list -> 
                         (pattern * expression) list -> decl list
val toGoal : (string * type_declaration) list -> expression -> decl 
val toGoal_neg : (string * type_declaration) list -> expression -> decl 
val toAxiom : (string * type_declaration) list -> expression -> decl 
val toAxiom_neg : (string * type_declaration) list -> expression -> decl 
val toAxiom_peq : (string * type_declaration) list -> expression -> pattern -> decl 
val toAxiom_pneq : (string * type_declaration) list -> expression -> pattern -> decl 
val toAxiom_beq : (string * type_declaration) list -> expression -> decl 
val toAxiom_bneq : (string * type_declaration) list -> expression -> decl 
val type_to_typelogic : loc -> Ident.t * type_declaration -> decl list
val out_ergotasks : out_channel -> decl list -> unit
val report_error: Format.formatter -> error -> unit
val is_expression_ergoble: expression -> bool
val bound_vars_to_logic : pattern -> decl list
val toGoal_beq : (string * type_declaration) list -> expression -> decl
val toGoal_bneq : (string * type_declaration) list -> expression -> decl
val toGoal_peq : (string * type_declaration) list -> expression -> pattern -> decl 
val toGoal_pneq : (string * type_declaration) list -> expression -> pattern ->  decl 
val mlaxiom_to_smtaxiom : (string * type_declaration) list -> axiom_declaration -> decl
val simpl_formula : (string, fvalue) Tbl.t -> lexpr -> lexpr
