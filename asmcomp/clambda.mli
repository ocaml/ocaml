(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda

type function_label = string

type ulambda =
    Uvar of Ident.t
  | Uconst of structured_constant
  | Udirect_apply of function_label * ulambda list
  | Ugeneric_apply of ulambda * ulambda list
  | Uclosure of (function_label * int * Ident.t list * ulambda) list
              * ulambda list
  | Uoffset of ulambda * int
  | Ulet of Ident.t * ulambda * ulambda
  | Uletrec of (Ident.t * ulambda) list * ulambda
  | Uprim of primitive * ulambda list
  | Uswitch of ulambda * int array * ulambda array * int array * ulambda array
  | Ustaticfail
  | Ucatch of ulambda * ulambda
  | Utrywith of ulambda * Ident.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Ident.t * ulambda * ulambda * direction_flag * ulambda

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool }          (* True if environment not used *)

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
