(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A label in the assembly stream. They may be either numeric or textual.
    (Numeric ones are converted to textual ones by this module.) The argument
    to [String] should not include any platform-specific prefix (such as "L",
    ".L", etc).

    Note: Labels are not symbols---they are not accessible in the object file.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Create a fresh integer-valued label (using the [new_label] function passed
    to [initialize], below). *)
val create : unit -> t

(** Create an integer-valued label. *)
val create_int : int -> t

(** Create a textual label.  The supplied name must not require escaping. *)
val create_string : string -> t

(** Convert a label to the corresponding textual form, suitable for direct
    emission into an assembly file.  This may be useful e.g. when emitting
    an instruction referencing a label. *)
val encode : t -> string

(** To be called by the emitter at the very start of code generation.
    [new_label] should always be [Cmm.new_label]. *)
val initialize
   : new_label:(unit -> int)
  -> unit
