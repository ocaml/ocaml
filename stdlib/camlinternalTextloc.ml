(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Daniel C. Buenzli                           *)
(*               Olivier Nicole, projet Cambium, Inria Paris              *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
end

let file_none = "-"

type byte_pos = int

type line_num = int

type line_pos = line_num * byte_pos

type repr =
  { file : string;
    first_byte : byte_pos; last_byte : byte_pos;
    first_line : line_pos; last_line : line_pos }

type t = textloc

external of_repr : repr -> t = "%identity"
external to_repr : t -> repr = "%identity"

let byte_pos_none = -1
let line_num_none = -1

let line_pos_first = 1, 0
let line_pos_none = line_num_none, byte_pos_none

let make ~file ~first_byte ~last_byte ~first_line ~last_line =
  of_repr { file; first_byte; last_byte; first_line; last_line }

let none =
  make
    ~file:file_none ~first_byte:byte_pos_none ~last_byte:byte_pos_none
    ~first_line:line_pos_none ~last_line:line_pos_none

let is_none l = (to_repr l).first_byte < 0

let is_empty l = (to_repr l).first_byte > (to_repr l).last_byte

let equal l1 l2 =
  let l1 = to_repr l1
  and l2 = to_repr l2 in
  String.equal l1.file l2.file
  && Int.equal l1.first_byte l2.first_byte
  && Int.equal l1.last_byte l2.last_byte

module Compare : sig
  type t = int

  val (>>>) : t -> (unit -> t) -> t
  (** Comparison chaining operator. [x >>> f] returns the [x] if it is greater
      than or lesser than zero, that is, if it represents a "not equal"
      comparison result. If [x] is zero, that is, if the comparison result is
      "equal", then, [f ()] is returned. *)
end = struct
  type t = int

  let (>>>) x next =
    match x with
    | 0 -> next ()
    | x -> x
end

let compare l1 l2 =
  let l1 = to_repr l1
  and l2 = to_repr l2 in
  let open Compare in
  String.compare l1.file l2.file >>> fun () ->
  Int.compare l1.first_byte l2.first_byte >>> fun () ->
  Int.compare l1.last_byte l2.last_byte

let file l = (to_repr l).file

let set_file l file = of_repr { to_repr l with file }

let first_byte l = (to_repr l).first_byte
let last_byte l = (to_repr l).last_byte
let first_line l = (to_repr l).first_line
let last_line l = (to_repr l).last_line

type line_range =
  | Single_line of line_pos
  | Range of line_pos * line_pos

let line_range l =
  let first_line = first_line l in
  let last_line = last_line l in
  if fst first_line = fst last_line then
    Single_line first_line
  else
    Range (first_line, last_line)

let set_first l ~first_byte ~first_line =
  of_repr { to_repr l with first_byte; first_line }

let set_last l ~last_byte ~last_line =
  of_repr { to_repr l with last_byte; last_line }

let to_first l =
  make ~file:(file l) ~first_byte:(first_byte l) ~last_byte:(first_byte l)
       ~first_line:(first_line l) ~last_line:(first_line l)

let to_last l =
  make ~file:(file l) ~first_byte:(last_byte l) ~last_byte:(last_byte l)
       ~first_line:(last_line l) ~last_line:(last_line l)

let before l =
  make ~file:(file l) ~first_byte:(first_byte l) ~last_byte:byte_pos_none
       ~first_line:(first_line l) ~last_line:line_pos_none

let after l =
  make ~file:(file l) ~first_byte:(last_byte l + 1) ~last_byte:(last_byte l)
       ~first_line:(last_line l) ~last_line:(last_line l)


let span l0 l1 =
  let first_byte, first_line =
    if first_byte l0 < first_byte l1
    then first_byte l0, first_line l0
    else first_byte l1, first_line l1
  in
  let last_byte, last_line, file =
    if last_byte l0 < last_byte l1
    then last_byte l1, last_line l1, file l1
    else last_byte l0, last_line l0, file l0
  in
  make ~file ~first_byte ~first_line ~last_byte ~last_line

let reloc ~first ~last =
  make ~file:(file last)
      ~first_byte:(first_byte first) ~last_byte:(last_byte last)
      ~first_line:(first_line first) ~last_line:(last_line last)

let of_lexing_range (start : Lexing.position) (end_ : Lexing.position) : t =
  let open Lexing in
  make
    ~file:start.pos_fname
    ~first_byte:start.pos_cnum
    ~last_byte:(end_.pos_cnum - 1) (* "end" means past the end *)
    ~first_line:(start.pos_lnum, start.pos_bol)
    ~last_line:(end_.pos_lnum, end_.pos_bol)

module type Low_level = sig
  type repr
  val int : int -> repr
  val string : string -> repr
  val block : repr list -> repr
end

module Lift_repr (Low_level : Low_level) = struct
  let to_repr l =
    let { file; first_byte; last_byte;
          first_line = first_line_num, first_line_pos;
          last_line = last_line_num, last_line_pos } : repr = to_repr l in
    let open Low_level in
    block
      [ string file;
        int first_byte; int last_byte;
        block [ int first_line_num; int first_line_pos ];
        block [ int last_line_num; int last_line_pos ];
      ]

  let make ~file ~first_byte ~last_byte ~first_line ~last_line =
    to_repr (make ~file ~first_byte ~last_byte ~first_line ~last_line)
end
