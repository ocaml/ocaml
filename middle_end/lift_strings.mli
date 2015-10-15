(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Lift strings to the toplevel of [Lambda] expressions.

    This transformation simplifies later optimization passes: it frees them
    from the concern of accidentally duplicating a string and thus potentially
    changing the semantics.

    There is no runtime cost to this transformation: strings are constants
    and will not appear in closures.

    The [Eliminate_const_block] pass must be run before this one.
*)
val run : Lambda.lambda -> Lambda.lambda
