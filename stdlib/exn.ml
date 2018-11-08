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

type t = exn = ..

let printers = ref []

let locfmt = format_of_string "File \"%s\", line %d, characters %d-%d: %s"

let field x i =
  let f = Obj.field x i in
  if not (Obj.is_block f) then
    sprintf "%d" (Obj.magic f : int)           (* can also be a char *)
  else if Obj.tag f = Obj.string_tag then
    sprintf "%S" (Obj.magic f : string)
  else if Obj.tag f = Obj.double_tag then
    string_of_float (Obj.magic f : float)
  else
    "_"

let rec other_fields x i =
  if i >= Obj.size x then ""
  else sprintf ", %s%s" (field x i) (other_fields x (i+1))

let fields x =
  match Obj.size x with
  | 0 -> ""
  | 1 -> ""
  | 2 -> sprintf "(%s)" (field x 1)
  | _ -> sprintf "(%s%s)" (field x 1) (other_fields x 2)

let to_string x =
  let rec conv = function
    | hd :: tl ->
        (match try hd x with _ -> None with
        | Some s -> s
        | None -> conv tl)
    | [] ->
        match x with
        | Out_of_memory -> "Out of memory"
        | Stack_overflow -> "Stack overflow"
        | Match_failure(file, line, char) ->
            sprintf locfmt file line char (char+5) "Pattern matching failed"
        | Assert_failure(file, line, char) ->
            sprintf locfmt file line char (char+6) "Assertion failed"
        | Undefined_recursive_module(file, line, char) ->
            sprintf locfmt file line char (char+6) "Undefined recursive module"
        | _ ->
            let x = Obj.repr x in
            if Obj.tag x <> 0 then
              (Obj.magic (Obj.field x 0) : string)
            else
              let constructor =
                (Obj.magic (Obj.field (Obj.field x 0) 0) : string) in
              constructor ^ (fields x) in
  conv !printers

external last_trace: unit -> Stack_trace.t = "caml_get_exception_raw_backtrace"

external raise: exn -> 'a = "%raise"

external reraise: exn -> 'a = "%reraise"

external raise_notrace: exn -> 'a = "%raise_notrace"

external raise_with_trace: exn -> Stack_trace.t -> 'a
  = "%raise_with_backtrace"

external set_tracing: bool -> unit = "caml_record_backtrace"
external tracing: unit -> bool = "caml_backtrace_status"

let register_printer fn =
  printers := fn :: !printers

let exn_slot x =
  let x = Obj.repr x in
  if Obj.tag x = 0 then Obj.field x 0 else x

let id x =
  let slot = exn_slot x in
  (Obj.obj (Obj.field slot 1) : int)

let name x =
  let slot = exn_slot x in
  (Obj.obj (Obj.field slot 0) : string)


let uncaught_exception_handler = ref None

let set_uncaught_exception_handler fn = uncaught_exception_handler := Some fn

let empty_backtrace : Stack_trace.t = Obj.obj (Obj.new_block Obj.abstract_tag 0)

let try_get_raw_backtrace () =
  try
    last_trace ()
  with _ (* Out_of_memory? *) ->
    empty_backtrace

let handle_uncaught_exception' exn debugger_in_use =
  try
    (* Get the backtrace now, in case one of the [at_exit] function
       destroys it. *)
    let raw_backtrace =
      if debugger_in_use (* Same test as in [runtime/printexc.c] *) then
        empty_backtrace
      else
        try_get_raw_backtrace ()
    in
    (try Stdlib.do_at_exit () with _ -> ());
    match !uncaught_exception_handler with
    | None ->
        eprintf "Fatal error: exception %s\n" (to_string exn);
        prerr_string (Stack_trace.to_string raw_backtrace);
        flush stderr
    | Some handler ->
        try
          handler exn raw_backtrace
        with exn' ->
          let raw_backtrace' = try_get_raw_backtrace () in
          eprintf "Fatal error: exception %s\n" (to_string exn);
          prerr_string (Stack_trace.to_string raw_backtrace);
          eprintf "Fatal error in uncaught exception handler: exception %s\n"
            (to_string exn');
          prerr_string (Stack_trace.to_string raw_backtrace');
          flush stderr
  with
    | Out_of_memory ->
        prerr_endline
          "Fatal error: out of memory in uncaught exception handler"

(* This function is called by [caml_fatal_uncaught_exception] in
   [runtime/printexc.c] which expects no exception is raised. *)
let handle_uncaught_exception exn debugger_in_use =
  try
    handle_uncaught_exception' exn debugger_in_use
  with _ ->
    (* There is not much we can do at this point *)
    ()

external register_named_value : string -> 'a -> unit
  = "caml_register_named_value"

let () =
  register_named_value "Printexc.handle_uncaught_exception"
    handle_uncaught_exception
