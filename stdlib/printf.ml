(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

external format_int: string -> int -> string = "caml_format_int"
external format_int32: string -> int32 -> string = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
                         = "caml_nativeint_format"
external format_int64: string -> int64 -> string = "caml_int64_format"
external format_float: string -> float -> string = "caml_format_float"

let bad_format fmt pos =
  invalid_arg
    ("printf: bad format " ^ String.sub fmt pos (String.length fmt - pos))

(* Parses a format to return the specified length and the padding direction. *)
let parse_format format =
  let rec parse neg i =
    if i >= String.length format then (0, neg) else
    match String.unsafe_get format i with
    | '1'..'9' ->
        (int_of_string (String.sub format i (String.length format - i - 1)),
         neg)
    | '-' ->
        parse true (succ i)
    | _ ->
        parse neg (succ i) in
  try parse false 1 with Failure _ -> bad_format format 0

(* Pad a (sub) string into a blank string of length [p],
   on the right if [neg] is true, on the left otherwise. *)
let pad_string pad_char p neg s i len =
  if p = len && i = 0 then s else
  if p <= len then String.sub s i len else
  let res = String.make p pad_char in
  if neg
  then String.blit s i res 0 len
  else String.blit s i res (p - len) len;
  res

(* Format a string given a %s format, e.g. %40s or %-20s.
   To do: ignore other flags (#, +, etc)? *)
let format_string format s =
  let (p, neg) = parse_format format in
  pad_string ' ' p neg s 0 (String.length s)

(* Extract a %format from [fmt] between [start] and [stop] inclusive.
   '*' in the format are replaced by integers taken from the [widths] list.
   The function is somewhat optimized for the "no *" case. *)

let extract_format fmt start stop widths =
  match widths with
  | [] -> String.sub fmt start (stop - start + 1)
  | _  ->
      let b = Buffer.create (stop - start + 10) in
      let rec fill_format i w =
        if i > stop then Buffer.contents b else
          match (String.unsafe_get fmt i, w) with
          | ('*', h :: t) ->
              Buffer.add_string b (string_of_int h); fill_format (succ i) t
          | ('*', []) ->
              bad_format fmt start (* should not happen *)
          | (c, _) ->
              Buffer.add_char b c; fill_format (succ i) w
      in fill_format start (List.rev widths)

let format_int_with_conv conv fmt i =
   match conv with
   | 'n' | 'N' -> fmt.[String.length fmt - 1] <- 'u'; format_int fmt i
   | _ -> format_int fmt i

(* Decode a %format and act on it.
   [fmt] is the printf format style, and [pos] points to a [%] character.
   After consuming the appropriate number of arguments and formatting
   them, one of the three continuations is called:
   [cont_s] for outputting a string (args: string, next pos)
   [cont_a] for performing a %a action (args: fn, arg, next pos)
   [cont_t] for performing a %t action (args: fn, next pos)
   "next pos" is the position in [fmt] of the first character following
   the %format in [fmt]. *)

(* Note: here, rather than test explicitly against [String.length fmt]
   to detect the end of the format, we use [String.unsafe_get] and
   rely on the fact that we'll get a "nul" character if we access
   one past the end of the string.  These "nul" characters are then
   caught by the [_ -> bad_format] clauses below.
   Don't do this at home, kids. *)

let scan_format fmt pos cont_s cont_a cont_t cont_f =
  let rec scan_flags widths i =
    match String.unsafe_get fmt i with
    | '*' ->
        Obj.magic(fun w -> scan_flags (w :: widths) (succ i))
    | '0'..'9' | '.' | '#' | '-' | ' ' | '+' -> scan_flags widths (succ i)
    | _ -> scan_conv widths i
  and scan_conv widths i =
    match String.unsafe_get fmt i with
    | '%' ->
        cont_s "%" (succ i)
    | 's' | 'S' as conv ->
        Obj.magic (fun (s: string) ->
          let s = if conv = 's' then s else "\"" ^ String.escaped s ^ "\"" in
          if i = succ pos (* optimize for common case %s *)
          then cont_s s (succ i)
          else cont_s (format_string (extract_format fmt pos i widths) s)
                      (succ i))
    | 'c' | 'C' as conv ->
        Obj.magic (fun (c: char) ->
          if conv = 'c'
          then cont_s (String.make 1 c) (succ i)
          else cont_s ("'" ^ Char.escaped c ^ "'") (succ i))
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' as conv ->
        Obj.magic(fun (n: int) ->
          cont_s (format_int_with_conv conv
                    (extract_format fmt pos i widths) n)
                 (succ i))
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' as conv ->
        Obj.magic(fun (f: float) ->
          let s =
            if conv = 'F' then string_of_float f else
            format_float (extract_format fmt pos i widths) f in
          cont_s s (succ i))
    | 'B' | 'b' ->
        Obj.magic(fun (b: bool) ->
          cont_s (string_of_bool b) (succ i))
    | 'a' ->
        Obj.magic (fun printer arg ->
          cont_a printer arg (succ i))
    | 't' ->
        Obj.magic (fun printer ->
          cont_t printer (succ i))
    | 'l' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: int32) ->
              cont_s (format_int32 (extract_format fmt pos (succ i) widths) n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | 'n' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: nativeint) ->
              cont_s (format_nativeint
                        (extract_format fmt pos (succ i) widths)
                        n)
                     (i + 2))
        | _ ->
            Obj.magic(fun (n: int) ->
              cont_s (format_int_with_conv 'n'
                        (extract_format fmt pos i widths)
                        n)
                     (succ i))
        end
    | 'L' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: int64) ->
              cont_s (format_int64 (extract_format fmt pos (succ i) widths) n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | '!' ->
        Obj.magic (cont_f (succ i))
    | _ ->
        bad_format fmt pos
  in scan_flags [] (pos + 1)

(* Application to [fprintf], etc.  See also [Format.*printf]. *)

let fprintf chan fmt =
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  let rec doprn i =
    if i >= len then Obj.magic () else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t cont_f
    |  c  -> output_char chan c; doprn (succ i)
  and cont_s s i =
    output_string chan s; doprn i
  and cont_a printer arg i =
    printer chan arg; doprn i
  and cont_t printer i =
    printer chan; doprn i
  and cont_f i =
    flush chan; doprn i
  in doprn 0

let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let kprintf kont fmt =
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  let dest = Buffer.create (len + 16) in
  let rec doprn i =
    if i >= len then begin
      let res = Buffer.contents dest in
      Buffer.clear dest;  (* just in case kprintf is partially applied *)
      Obj.magic (kont res)
    end else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t cont_f
    |  c  -> Buffer.add_char dest c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dest s; doprn i
  and cont_a printer arg i =
    Buffer.add_string dest (printer () arg); doprn i
  and cont_t printer i =
    Buffer.add_string dest (printer ()); doprn i
  and cont_f i = doprn i
  in doprn 0

let sprintf fmt = kprintf (fun x -> x) fmt;;

let bprintf dest fmt =
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  let rec doprn i =
    if i >= len then Obj.magic () else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t cont_f
    |  c  -> Buffer.add_char dest c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dest s; doprn i
  and cont_a printer arg i =
    printer dest arg; doprn i
  and cont_t printer i =
    printer dest; doprn i
  and cont_f i = doprn i
  in doprn 0
