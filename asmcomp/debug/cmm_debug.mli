(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functionality for augmenting Cmm code that does not correspond to
    OCaml source code, such as that generated for the startup file, with
    debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create
   : startup_cmm_file:string
  -> startup_cmm_chan:Stdlib.out_channel
  -> t

val startup_cmm_file : t -> string

val write_cmm_to_channel_and_fix_up_debuginfo : t -> Cmm.phrase -> Cmm.phrase
