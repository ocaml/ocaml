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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Not_inlined
  | Inlined
  | Declared_not_inlined
  | Declared_inlined

(* DWARF-5 spec. page 233, table 7.20. *)
let code t =
  let code =
    match t with
    | Not_inlined -> 0x00
    | Inlined -> 0x01
    | Declared_not_inlined -> 0x02
    | Declared_inlined -> 0x03
  in
  Numbers.Uint8.of_int_exn code

let as_dwarf_value t = 
  let comment =
    match t with
    | Not_inlined -> "not inlined"
    | Inlined -> "inlined"
    | Declared_not_inlined -> "declared `not inlined'"
    | Declared_inlined -> "declared `inlined'"
  in
  Dwarf_value.uint8 ~comment (code t)
