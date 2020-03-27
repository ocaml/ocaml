(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of test-result related types and functions *)

type status = Pass | Skip | Fail

type t = {
  status : status;
  reason : string option
}

val pass : t

val skip : t

val fail : t

val pass_with_reason : string -> t

val skip_with_reason : string -> t

val fail_with_reason : string -> t

val string_of_result : t -> string

val is_pass : t -> bool

val is_skip : t -> bool

val is_fail : t -> bool
