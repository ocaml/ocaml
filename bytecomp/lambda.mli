(* The "lambda" intermediate code *)

open Asttypes

type primitive =
    Pidentity
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  | Pmakeblock of int
  | Ptagof
  | Pfield of int
  | Psetfield of int
  | Pccall of string * int
  | Pupdate
  | Praise
  | Psequand | Psequor | Pnot
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  | Pgetstringchar | Psetstringchar
  | Pvectlength | Pgetvectitem | Psetvectitem

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

type structured_constant =
    Const_base of constant
  | Const_block of int * structured_constant list

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list
  | Lfunction of Ident.t * lambda
  | Llet of Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda * int) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of lambda * int * int * (int * lambda) list
  | Lstaticfail
  | Lcatch of lambda * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lshared of lambda * int option ref

val const_unit: structured_constant
val lambda_unit: lambda
val share_lambda: lambda -> lambda
val name_lambda: lambda -> (Ident.t -> lambda) -> lambda
val free_variables: lambda -> Ident.t list
val is_guarded: lambda -> bool

type compilenv

val empty_env: compilenv
val add_env: Ident.t -> lambda -> compilenv -> compilenv
val transl_access: compilenv -> Ident.t -> lambda

val transl_path: Path.t -> lambda
