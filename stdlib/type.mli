(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Type introspection.

    @since 5.1 *)

(** {1:witness Type equality witness} *)

type (_, _) eq = Equal: ('a, 'a) eq
(** The purpose of [eq] is to represent type equalities that may not otherwise
    be known by the type checker (e.g. because they may depend on dynamic data).

    A value of type [(a, b) eq] represents the fact that types [a] and [b] are
    equal.

    If one has a value [eq : (a, b) eq] that proves types [a] and [b] are equal,
    one can use it to convert a value of type [a] to a value of type [b] by
    pattern matching on [Equal]:
    {[
      let cast (type a) (type b) (Equal : (a, b) Type.eq) (a : a) : b = a
    ]}

    At runtime, this function simply returns its second argument unchanged.
*)
