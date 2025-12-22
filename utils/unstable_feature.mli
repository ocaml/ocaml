(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Alistair O'Brien, University of Cambridge                *)
(*                                                                        *)
(*   Copyright 2025 Alistair O'Brien                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Name : sig
  type t = private string

  val pp : Format_doc.formatter -> t -> unit

  val of_string : string -> t option
end

(** Unstable features for the OCaml compiler and standard library. *)
type t =
  { name : Name.t
    (** [name] is the unique name associated with the feature *)
  ; issue : int
    (** [issue] is the tracking issue associated with the feature *)
  }

(** {1 Scopes}

    A feature scope defines the set of enabled language features.

    The root scope is defined by features passed to the
    [-Z <allowed-features>] CLI flag. *)

module Scope : sig
  val enable : Name.t -> unit

  val disable : Name.t -> unit

  val is_enabled : Name.t -> bool

  (** Temporarily enable and disable features. Calls to [enable], [disable]
      inside the body of the function argument will be rolled back when the
      function returns. *)
  val with_local : (unit -> 'a) -> 'a
end

