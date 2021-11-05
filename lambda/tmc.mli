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

(** TMC (Tail Modulo Cons) is a code transformation that
    rewrites transformed functions in destination-passing-style, in
    such a way that certain calls that were not in tail position in the
    original program become tail-calls in the transformed program.

    As a classic example, the following program
    {|
     let[@tail_mod_cons] rec map f = function
     | [] -> []
     | x :: xs ->
       let y = f x in
       y :: map f xs
    |}
    becomes (expressed in almost-source-form; the translation is in
    fact at the Lambda-level)
    {|
     let rec map f = function
     | [] -> []
     | x :: xs ->
       let y = f x in
       let dst = y :: Placeholder in
       map_dps dst 1 f xs; dst
     and map_dps dst offset f = function
     | [] ->
       dst.offset <- []
     | x :: xs ->
       let y = f x in
       let dst' = y :: Placeholder in
       dst.offset <- dst';
       map_dps dst 1 f fx
    |}

    In this example, the expression (y :: map f xs) had a call in
    non-tail-position, and it gets rewritten into tail-calls. TMC
    handles all such cases where the continuation of the call
    (what needs to be done after the return) is a "construction", the
    creation of a (possibly nested) data block.

    The code transformation generates two versions of the
    input function, the "direct" version with the same type and
    behavior as the original one (here just [map]), and
    the "destination-passing-style" version (here [map_dps]).

    Any call to the original function from outside the let..rec
    declaration gets transformed into a call into the direct version,
    which will itself call the destination-passing-style versions on
    recursive calls that may benefit from it (they are in tail-position
    modulo constructors).

    Because of this inherent code duplication, the transformation may
    not always improve performance. In this implementation, TMC is
    opt-in, we only transform functions that the user has annotated
    with an attribute to request the transformation.
*)

open Lambda

val rewrite : lambda -> lambda
