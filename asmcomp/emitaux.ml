(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Common functions for emitting assembly code *)

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (string_of_int n)

let emit_char c = output_char !output_channel c

let emit_nativeint n = output_string !output_channel (Nativeint.to_string n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_symbol esc s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        output_char !output_channel c
    | _ ->
        Printf.fprintf !output_channel "%c%02x" esc (Char.code c)
  done

let emit_string_literal s =
  let last_was_escape = ref false in
  emit_string "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf !output_channel "\\%o" (Char.code c)
      else output_char !output_channel c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      output_char !output_channel c;
      last_was_escape := false
    end else begin
      Printf.fprintf !output_channel "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  emit_string "\""

let emit_string_directive directive s =
  let l = String.length s in
  if l = 0 then ()
  else if l < 80 then begin
    emit_string directive;
    emit_string_literal s;
    emit_char '\n'
  end else begin
    let i = ref 0 in
    while !i < l do
      let n = min (l - !i) 80 in
      emit_string directive;
      emit_string_literal (String.sub s !i n);
      emit_char '\n';
      i := !i + n
    done
  end

let emit_bytes_directive directive s =
   let pos = ref 0 in
   for i = 0 to String.length s - 1 do
     if !pos = 0
     then emit_string directive
     else emit_char ',';
     emit_int(Char.code s.[i]);
     incr pos;
     if !pos >= 16 then begin emit_char '\n'; pos := 0 end
   done;
   if !pos > 0 then emit_char '\n'

