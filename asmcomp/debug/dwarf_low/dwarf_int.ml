(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives

type t =
  | Thirty_two of Int32.t
  | Sixty_four of Int64.t

let zero () =
  match Dwarf_format.get () with
  | Thirty_two -> Thirty_two Int32.zero
  | Sixty_four -> Sixty_four Int64.zero

let to_int64 t =
  match t with
  | Thirty_two t -> Int64.of_int32 t
  | Sixty_four t -> t

let size t =
  match t with
  | Thirty_two _ -> 4L
  | Sixty_four _ -> 8L

let emit t =
  match t with
  | Thirty_two i -> A.int32 i
  | Sixty_four i -> A.int64 i
