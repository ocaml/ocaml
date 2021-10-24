(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Frédéric Bour                                              *)
(*             Gabriel Scherer, projet Partout, INRIA Saclay              *)
(*             Basile Clément, projet Cambium, INRIA Paris                *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Tail-modulo-cons optimization.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.
*)

open Lambda

val rewrite : lambda -> lambda
