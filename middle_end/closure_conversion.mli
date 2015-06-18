(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Generation of [Flambda] intermediate language code from [Lambda] code
   by performing a form of closure conversion.

   Function declarations (which may bind one or more variables identifying
   functions, possibly with mutual recursion) are transformed to
   [Fset_of_closures] expressions.  [Fselect_closure] expressions are then used to
   select a closure for a particular function from a [Fset_of_closures]
   expression.  The [Fset_of_closures] expressions say nothing about the
   actual runtime layout of the closures; this is handled when [Flambda] code
   is translated to [Clambda] code.

   The following transformations are also performed during closure
   conversion:
   - Constant blocks are converted to applications of the [Pmakeblock]
     primitive.
   - [Levent] debugging event nodes are removed and the information within
     them attached to function, method and [raise] calls.
   - Access to global fields of the current compilation unit (of the form
     [Lprim (Pfield _ | Psetfield _, [Lprim (Pgetglobal _, []); ...])])
     are converted to [Pgetglobalfield] and [Psetglobalfield] primitives.
   - Tuplified functions are converted to curried functions and a stub
     function emitted to call the curried version.  For example:
       let rec f (x, y) = f (x + 1, y + 1)
     is transformed to:
       let rec internal_f x y = f (x + 1,y + 1)
       and f (x, y) = internal_f x y  (* [f] is marked as a stub function *)
   - The [Pdirapply] and [Prevapply] application primitives are removed and
     converted to normal [Flambda] application nodes.
   - String constants are lifted to the toplevel to avoid special cases later
     (duplicating them may change the semantics of the program).
*)

open Abstract_identifiers

val lambda_to_flambda
   : symbol_for_global':(Ident.t -> Symbol.t)
  -> exported_fields:int
  -> Lambda.lambda
  -> Expr_id.t Flambda.t
