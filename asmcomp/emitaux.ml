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

(* Common functions for emitting assembly code *)

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (string_of_int n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_symbol s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        output_char !output_channel c
    | _ ->
        Printf.fprintf !output_channel "$%02x" (Char.code c)
  done

let emit_string_literal s =
  emit_string "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c = '\\' then
      emit_string "\\\\"
    else if c >= ' ' & c <= '~' & c <> '"' then
      output_char !output_channel c
    else
      Printf.fprintf !output_channel "\\%03o" (Char.code c)
  done;
  emit_string "\""

