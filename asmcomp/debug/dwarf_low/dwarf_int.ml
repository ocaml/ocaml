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

module Uint64 = Numbers.Uint64

type t =
  | Thirty_two of Int32.t
  | Sixty_four of Int64.t

let print ppf t =
  match t with
  | Thirty_two i -> Format.fprintf ppf "%ld" i
  | Sixty_four i -> Format.fprintf ppf "%Ld" i

let num i32 =
  match Dwarf_format.get () with
  | Thirty_two -> Thirty_two i32
  | Sixty_four -> Sixty_four (Int64.of_int32 i32)

let zero () = num 0l
let one () = num 1l
let two () = num 2l
let four () = num 4l
let eight () = num 8l

(* CR mshinwell: Note: for cross compilation, [Sys.int_size] here should be
   replaced by the size of [int] on the *host*. *)
let of_host_int_exn i =
  match Sys.int_size, Dwarf_format.get () with
  | 31, Thirty_two -> Thirty_two (Int32.of_int i)
  | 63, Sixty_four -> Sixty_four (Int64.of_int i)
  | 31, Sixty_four -> Sixty_four (Int64.of_int i)
  | 63, Thirty_two ->
    if i >= -0x8000_0000 && i <= 0x7fff_ffff then
      Thirty_two (Int32.of_int i)
    else
      Misc.fatal_errorf "Cannot encode host integer %d in the requested \
          DWARF format"
        i
  | size, _ -> Misc.fatal_errorf "Unknown [int] size %d" size

let of_targetint_exn i =
  match Targetint.repr i, Dwarf_format.get () with
  | Int32 i32, Thirty_two -> Thirty_two i32
  | Int64 i64, Sixty_four -> Sixty_four i64
  | Int32 i32, Sixty_four -> Sixty_four (Int64.of_int32 i32)
  | Int64 i64, Thirty_two ->
    if i64 >= -0x8000_0000L && i64 <= 0x7fff_ffffL then
      Thirty_two (Int64.to_int32 i64)
    else
      Misc.fatal_errorf "Cannot encode target integer %a in the requested \
          DWARF format"
        Targetint.print i

let to_int64 t =
  match t with
  | Thirty_two t -> Int64.of_int32 t
  | Sixty_four t -> t

let to_uint64_exn t =
  match t with
  | Thirty_two t -> Uint64.of_int32_exn t
  | Sixty_four t -> Uint64.of_int64_exn t

(* CR-someday mshinwell: This should probably check for overflow. *)
let add t1 t2 =
  match t1, t2 with
  | Thirty_two i1, Thirty_two i2 ->
    Thirty_two (Int32.add i1 i2)
  | Sixty_four i1, Sixty_four i2 ->
    Sixty_four (Int64.add i1 i2)
  | Thirty_two _, Sixty_four _
  | Sixty_four _, Thirty_two _ ->
    Misc.fatal_error "Cannot intermix sizes of [Dwarf_int]s"

let succ t = add t (one ())

let size t =
  match t with
  | Thirty_two _ -> Thirty_two 4l
  | Sixty_four _ -> Sixty_four 8L

let emit ?comment t =
  match t with
  | Thirty_two i -> A.int32 ?comment i
  | Sixty_four i -> A.int64 ?comment i
