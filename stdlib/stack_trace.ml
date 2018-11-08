(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf

type raw_backtrace_slot
type raw_backtrace

type t = raw_backtrace

type backtrace_slot =
  | Known_location of {
      is_raise    : bool;
      filename    : string;
      line_number : int;
      start_char  : int;
      end_char    : int;
      is_inline   : bool;
    }
  | Unknown_location of {
      is_raise : bool
    }

(* to avoid warning *)
let _ = [Known_location { is_raise = false; filename = "";
                          line_number = 0; start_char = 0; end_char = 0;
                          is_inline = false };
         Unknown_location { is_raise = false }]

external convert_raw_backtrace_slot:
  raw_backtrace_slot -> backtrace_slot = "caml_convert_raw_backtrace_slot"

external convert_raw_backtrace:
  raw_backtrace -> backtrace_slot array = "caml_convert_raw_backtrace"

let convert_raw_backtrace bt =
  try Some (convert_raw_backtrace bt)
  with Failure _ -> None

let format_backtrace_slot pos slot =
  let info is_raise =
    if is_raise then
      if pos = 0 then "Raised at" else "Re-raised at"
    else
      if pos = 0 then "Raised by primitive operation at" else "Called from"
  in
  match slot with
  | Unknown_location l ->
      if l.is_raise then
        (* compiler-inserted re-raise, skipped *) None
      else
        Some (sprintf "%s unknown location" (info false))
  | Known_location l ->
      Some (sprintf "%s file \"%s\"%s, line %d, characters %d-%d"
              (info l.is_raise) l.filename
              (if l.is_inline then " (inlined)" else "")
              l.line_number l.start_char l.end_char)

let backtrace_to_string backtrace =
  match backtrace with
  | None ->
     "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
      let b = Buffer.create 1024 in
      for i = 0 to Array.length a - 1 do
        match format_backtrace_slot i a.(i) with
          | None -> ()
          | Some str -> bprintf b "%s\n" str
      done;
      Buffer.contents b

let to_string raw_backtrace =
  backtrace_to_string (convert_raw_backtrace raw_backtrace)

let backtrace_slot_is_raise = function
  | Known_location l -> l.is_raise
  | Unknown_location l -> l.is_raise

let backtrace_slot_is_inline = function
  | Known_location l -> l.is_inline
  | Unknown_location _ -> false

let backtrace_slots raw_backtrace =
  (* The documentation of this function guarantees that Some is
     returned only if a part of the trace is usable. This gives us
     a bit more work than just convert_raw_backtrace, but it makes the
     API more user-friendly -- otherwise most users would have to
     reimplement the "Program not linked with -g, sorry" logic
     themselves. *)
  match convert_raw_backtrace raw_backtrace with
    | None -> None
    | Some backtrace ->
      let usable_slot = function
        | Unknown_location _ -> false
        | Known_location _ -> true in
      let rec exists_usable = function
        | (-1) -> false
        | i -> usable_slot backtrace.(i) || exists_usable (i - 1) in
      if exists_usable (Array.length backtrace - 1)
      then Some backtrace
      else None

external raw_backtrace_length :
  raw_backtrace -> int = "caml_raw_backtrace_length" [@@noalloc]

external get_raw_backtrace_slot :
  raw_backtrace -> int -> raw_backtrace_slot = "caml_raw_backtrace_slot"

external get_raw_backtrace_next_slot :
  raw_backtrace_slot -> raw_backtrace_slot option
  = "caml_raw_backtrace_next_slot"

external current: int -> raw_backtrace = "caml_get_current_callstack"

module Slot = struct
  type t = backtrace_slot
  let format = format_backtrace_slot
  let is_raise = backtrace_slot_is_raise
  let is_inline = backtrace_slot_is_inline

  type location = {
    filename : string;
    line_number : int;
    start_char : int;
    end_char : int;
  }

  let location = function
    | Unknown_location _ -> None
    | Known_location l ->
      Some {
        filename    = l.filename;
        line_number = l.line_number;
        start_char  = l.start_char;
        end_char    = l.end_char;
      }

  let of_raw = convert_raw_backtrace_slot

  module Raw = struct
    type t = raw_backtrace_slot
    let get = get_raw_backtrace_slot
    let next = get_raw_backtrace_next_slot
  end
end

let length = raw_backtrace_length
let slots = backtrace_slots
