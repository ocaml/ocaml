(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Check AST invariants

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val structure : Parsetree.structure -> unit
val signature : Parsetree.signature -> unit

(** Checks the invariant of Location.t's loc_ghost field, that are stated in
    location.mli. This can be run with -dparsetree-loc-ghost-invariants, which
    is used slightly in the testsuite, but should be used more to find more
    of the places where the invariant is broken. *)
val check_loc_ghost :
  (Ast_iterator.iterator -> Ast_iterator.iterator -> 'a -> unit)
  -> 'a
  -> source_contents:string
  -> unit
