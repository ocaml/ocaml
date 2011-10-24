open Why_ptree
open Typedtree
open EscSyn
open Format


val static_contract_checking: ThmEnv.t -> (Ident.t * expression) -> expression * validity

type error = 
  | Simpl_expr_Texp_function
  | Simpl_expr_Texp_let
  | Simpl_expr_Path_not_handled
  | Simpl_args_not_function_type
  | Simpl_args_no_optional_arg
  | Simpl_args_continuation_not_matched
  | KnownCon_impossible_pattern
  | Rebuild_not_function_type
  | Rebuild_pattern_not_matched
  | ErrorExp_Unknownblame
  | ErrorExp_Texp_unr
  | ErrorExp_function_has_no_argument
  | ErrorExp_no_pattern_matching
  | ErrorExp_tuple_no_argument
  | ErrorExp_constructor_no_argument
  | ErrorExp_expression_not_handled
  | Check_expression_not_handled
  | Has_function_call_expression_not_handled

exception Error of error

val report_error: formatter -> error -> unit
