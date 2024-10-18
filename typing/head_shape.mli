(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Partout, INRIA Saclay                        *)
(*   Nicolas Chataing, ENS Paris                                          *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Head_shape_types.shape

(** Returns the head shape information of type path *)
val of_type_path : Env.t -> Path.t -> t

(** Returns the head shape information of the constructor
    of a regular (non-extensible) variant type. *)
val of_regular_cstr_description :
  Env.t -> Data_types.constructor_description -> t

(** Returns the head shape information of an unboxed constructor *)
val of_unboxed_cstr_description :
  Env.t -> Head_shape_types.unboxed_cstr_description -> t

(** Check a new type declaration, that may be a variant type
    containing unboxed constructors, to verify that the unboxing
    requests respect the "disjointness" requirement of constructor
    unboxing -- the values of two constructors must not conflict.

   This function fails with an error if the declaration is
   unsafe. *)
val check_typedecl : Env.t -> Path.t * Types.type_declaration -> unit
