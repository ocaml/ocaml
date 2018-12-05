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
      raise Dwarf_format.Too_large_for_thirty_two_bit_dwarf
  | size, _ -> Misc.fatal_errorf "Unknown [int] size %d" size

let of_int64_exn i64 =
  match Dwarf_format.get () with
  | Sixty_four -> Sixty_four i64
  | Thirty_two ->
    if i64 >= -0x8000_0000L && i64 <= 0x7fff_ffffL then
      Thirty_two (Int64.to_int32 i64)
    else
      raise Dwarf_format.Too_large_for_thirty_two_bit_dwarf

let of_targetint_exn i =
  match Targetint.repr i, Dwarf_format.get () with
  | Int32 i32, Thirty_two -> Thirty_two i32
  | Int64 i64, Sixty_four -> Sixty_four i64
  | Int32 i32, Sixty_four -> Sixty_four (Int64.of_int32 i32)
  | Int64 i64, Thirty_two ->
    if i64 >= -0x8000_0000L && i64 <= 0x7fff_ffffL then
      Thirty_two (Int64.to_int32 i64)
    else
      raise Dwarf_format.Too_large_for_thirty_two_bit_dwarf

let to_int64 t =
  match t with
  | Thirty_two t -> Int64.of_int32 t
  | Sixty_four t -> t

let to_uint64_exn t =
  match t with
  | Thirty_two t -> Uint64.of_int32_exn t
  | Sixty_four t -> Uint64.of_int64_exn t

let add t1 t2 =
  begin match t1, t2 with
  | Thirty_two _, Thirty_two _
  | Sixty_four _, Sixty_four _ -> ()
  | Thirty_two _, Sixty_four _
  | Sixty_four _, Thirty_two _ ->
    Misc.fatal_error "Cannot intermix sizes of [Dwarf_int]s"
  end;
  let t1 = to_int64 t1 in
  let t2 = to_int64 t2 in
  of_int64_exn (Int64.add t1 t2)

let succ t = add t (one ())

let width_as_int64 () =
  match zero () with
  | Thirty_two _ -> 4L
  | Sixty_four _ -> 8L

let size t =
  match t with
  | Thirty_two _ -> Thirty_two 4l
  | Sixty_four _ -> Sixty_four 8L

let emit ?comment t =
  match t with
  | Thirty_two i -> A.int32 ?comment i
  | Sixty_four i -> A.int64 ?comment i
