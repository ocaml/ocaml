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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  start_address : Code_address.t;
  end_address : Code_address.t;
  location : bytes;
}

let create ~start_address ~end_address ~location =
  { start_address; end_address; location }

let start_address t = t.start_address

let end_address t = t.end_address

let location t = t.location

let to_string t =
  Printf.sprintf "[%s, %s): %d bytes"
    (Code_address.to_string t.start_address)
    (Code_address.to_string t.end_address)
    (Bytes.length t.location)

let print ppf t =
  Format.fprintf ppf "%s" (to_string t)
