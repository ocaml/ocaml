(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [ThreadIO]: thread-compatible input-output operations *)

let input_char ic = Thread.wait_inchan ic; input_char ic
let input_line ic = Thread.wait_inchan ic; input_line ic
let input ic buf ofs len = Thread.wait_inchan ic; input ic buf ofs len

let rec really_input ic s ofs len =
  if ofs < 0 or ofs + len > String.length s then invalid_arg "really_input"
  else if len <= 0 then ()
  else begin
    let r = input ic s ofs len in
    if r = 0
    then raise End_of_file
    else really_input ic s (ofs+r) (len-r)
  end

let input_byte ic = Thread.wait_inchan ic; input_byte ic
let input_binary_int ic = Thread.wait_inchan ic; input_binary_int ic
let input_value ic = Thread.wait_inchan ic; input_value ic

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

let lexing_from_channel ic =
  Lexing.from_function (fun buf n -> input ic buf 0 n)
