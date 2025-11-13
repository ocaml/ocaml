(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Address classes for pointers.

    These specify how pointer values should be interpreted on
    architectures with different address spaces. *)

type t =
  | Code     (** Pointer to code *)
  | Data     (** Pointer to data *)
  | Generic  (** Generic pointer (default) *)

(** Convert address class to human-readable string *)
val to_string : t -> string

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
