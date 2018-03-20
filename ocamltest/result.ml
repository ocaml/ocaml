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

let result_of_status s = { status = s; reason = None }

let pass = result_of_status Pass

let skip = result_of_status Skip

let fail = result_of_status Fail

let result_with_reason s r = { status = s; reason = Some r }

let pass_with_reason r = result_with_reason Pass r

let skip_with_reason r = result_with_reason Skip r

let fail_with_reason r = result_with_reason Fail r

let style_of_status s msg =
  let pre = match s with
    | Pass -> "2" (* green *)
    | Fail -> "1" (* red *)
    | Skip -> "4" (* blue *)
  in
  "\x1b[1;3" ^ pre ^ "m" ^ msg ^ "\x1b[0m"

let string_of_status ?(color=true) s =
  let r = match s with
    | Pass -> "passed"
    | Skip -> "skipped"
    | Fail -> "failed"
  in
  if color then style_of_status s r else r

let string_of_reason = function
  | None -> ""
  | Some reason -> (" (" ^ reason ^ ")")

let string_of_result ?color r =
  (string_of_status ?color r.status) ^ (string_of_reason r.reason)

let is_pass r = r.status = Pass

let is_skip r = r.status = Skip

let is_fail r = r.status = Fail
