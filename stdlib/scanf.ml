(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The run-time library for scanners. *)

(* Scanning buffers. *)
module type SCANNING = sig

type scanbuf;;

val stdib : scanbuf;;
(* The scanning buffer reading from [stdin].
    [stdib] is equivalent to [Scanning.from_channel stdin]. *)

val next_char : scanbuf -> char;;
(* [Scanning.next_char ib] advance the scanning buffer for
    one character.
    If no more character can be read, sets a end of file condition and
    returns '\000'. *)

val invalidate_current_char : scanbuf -> unit;;
(* [Scanning.invalidate_current_char ib] mark the current_char as already
    scanned. *)

val peek_char : scanbuf -> char;;
(* [Scanning.peek_char ib] returns the current char available in
    the buffer or read one if necessary (when the current character is
    already scanned).
    If no character can be read, sets an end of file condition and
    returns '\000'. *)

val checked_peek_char : scanbuf -> char;;
(* Same as above but always returns a valid char or fails:
    instead of returning a null char when the reading method of the
    input buffer has reached an end of file, the function raises exception
    [End_of_file]. *)

val store_char : scanbuf -> char -> int -> int;;
(* [Scanning.store_char ib c lim] adds [c] to the token buffer
    of the scanning buffer. It also advances the scanning buffer for one
    character and returns [lim - 1], indicating the new limit
    for the length of the current token. *)

val skip_char : scanbuf -> int -> int;;
(* [Scanning.skip_char ib lim] ignores the current character. *)

val ignore_char : scanbuf -> int -> int;;
(* [Scanning.ignore_char ib lim] ignores the current character and
   decrements the limit. *)

val token : scanbuf -> string;;
(* [Scanning.token ib] returns the string stored into the token
    buffer of the scanning buffer: it returns the token matched by the
    format. *)

val reset_token : scanbuf -> unit;;
(* [Scanning.reset_token ib] resets the token buffer of
    the given scanning buffer. *)

val char_count : scanbuf -> int;;
(* [Scanning.char_count ib] returns the number of characters
    read so far from the given buffer. *)

val line_count : scanbuf -> int;;
(* [Scanning.line_count ib] returns the number of new line
    characters read so far from the given buffer. *)

val token_count : scanbuf -> int;;
(* [Scanning.token_count ib] returns the number of tokens read
    so far from [ib]. *)

val eof : scanbuf -> bool;;
(* [Scanning.eof ib] returns the end of input condition
    of the given buffer. *)

val end_of_input : scanbuf -> bool;;
(* [Scanning.end_of_input ib] tests the end of input condition
    of the given buffer (if no char has ever been read, an attempt to
    read one is performed). *)

val beginning_of_input : scanbuf -> bool;;
(* [Scanning.beginning_of_input ib] tests the beginning of input
    condition of the given buffer. *)

val name_of_input : scanbuf -> string;;
(* [Scanning.name_of_input ib] returns the name of the character
    source for input buffer [ib]. *)

val from_string : string -> scanbuf;;
val from_channel : in_channel -> scanbuf;;
val from_file : string -> scanbuf;;
val from_file_bin : string -> scanbuf;;
val from_function : (unit -> char) -> scanbuf;;

end;;

module Scanning : SCANNING = struct

(* The run-time library for scanf. *)
type file_name = string;;

type scanbuf = {
  mutable eof : bool;
  mutable current_char : char;
  mutable current_char_is_valid : bool;
  mutable char_count : int;
  mutable line_count : int;
  mutable token_count : int;
  mutable get_next_char : unit -> char;
  tokbuf : Buffer.t;
  file_name : file_name;
};;

let null_char = '\000';;

(* Reads a new character from input buffer.  Next_char never fails,
   even in case of end of input: it then simply sets the end of file
   condition. *)
let next_char ib =
  try
    let c = ib.get_next_char () in
    ib.current_char <- c;
    ib.current_char_is_valid <- true;
    ib.char_count <- ib.char_count + 1;
    if c == '\n' then ib.line_count <- ib.line_count + 1;
    c with
  | End_of_file ->
    let c = null_char in
    ib.current_char <- c;
    ib.current_char_is_valid <- false;
    ib.eof <- true;
    c;;

let peek_char ib =
  if ib.current_char_is_valid then ib.current_char else next_char ib;;

(* Returns a valid current char for the input buffer. In particular
   no irrelevant null character (as set by [next_char] in case of end
   of input) is returned, since [End_of_file] is raised when
   [next_char] sets the end of file condition while trying to read a
   new character. *)
let checked_peek_char ib =
  let c = peek_char ib in
  if ib.eof then raise End_of_file;
  c;;

let end_of_input ib =
  ignore (peek_char ib);
  ib.eof;;

let eof ib = ib.eof;;

let beginning_of_input ib = ib.char_count = 0;;
let name_of_input ib = ib.file_name;;
let char_count ib = ib.char_count;;
let line_count ib = ib.line_count;;
let reset_token ib = Buffer.reset ib.tokbuf;;
let invalidate_current_char ib = ib.current_char_is_valid <- false;;

let token ib =
  let tokbuf = ib.tokbuf in
  let tok = Buffer.contents tokbuf in
  Buffer.clear tokbuf;
  ib.token_count <- ib.token_count + 1;
  tok;;

let token_count ib = ib.token_count;;

let skip_char ib max =
  invalidate_current_char ib;
  max;;

let ignore_char ib max = skip_char ib (max - 1);;

let store_char ib c max =
  Buffer.add_char ib.tokbuf c;
  ignore_char ib max;;

let default_token_buffer_size = 1024;;

let create fname next = {
  eof = false;
  current_char = '\000';
  current_char_is_valid = false;
  char_count = 0;
  line_count = 0;
  token_count = 0;
  get_next_char = next;
  tokbuf = Buffer.create default_token_buffer_size;
  file_name = fname;
};;

let from_string s =
  let i = ref 0 in
  let len = String.length s in
  let next () =
    if !i >= len then raise End_of_file else
    let c = s.[!i] in
    incr i;
    c in
  create "string input" next;;

let from_function = create "function input";;

(* Perform bufferized input to improve efficiency. *)
let file_buffer_size = ref 1024;;

let from_file_channel fname ic =
  let len = !file_buffer_size in
  let buf = String.create len in
  let i = ref 0 in
  let lim = ref 0 in
  let next () =
    if !i < !lim then begin let c = buf.[!i] in incr i; c end else begin
      lim := input ic buf 0 len;
      if !lim = 0 then raise End_of_file else begin
        i := 1;
        buf.[0]
      end
    end in
  create fname next;;

let from_file fname = from_file_channel fname (open_in fname);;
let from_file_bin fname = from_file_channel fname (open_in_bin fname);;

let from_input_channel fname ic =
  let next () = input_char ic in
  create fname next;;

let from_channel = from_input_channel "input channel";;

(* The scanning buffer reading from [stdin].*)
let stdib = from_input_channel "stdin" stdin;;

end;;

(* Formatted input functions. *)

(* Reporting errors. *)
exception Scan_failure of string;;

let bad_input s = raise (Scan_failure s);;
let bad_input_char c = bad_input (String.make 1 c);;

let bad_input_escape c =
  bad_input (Printf.sprintf "illegal escape character %C" c);;

let scanf_bad_input ib = function
  | Scan_failure s | Failure s ->
      let i = Scanning.char_count ib in
      bad_input (Printf.sprintf "scanf: bad input at char number %i: %s" i s)
  | x -> raise x;;

let bad_conversion fmt i c =
  invalid_arg
    (Printf.sprintf
       "scanf: bad conversion %%%c, at char number %i \
        in format string ``%s''" c i fmt);;

let incomplete_format fmt =
  invalid_arg
    (Printf.sprintf "scanf: premature end of format string ``%s''" fmt);;

let bad_float () = bad_input "no dot or exponent part found in float token";;

let format_mismatch_err fmt1 fmt2 =
  Printf.sprintf "format read %S does not match specification %S" fmt1 fmt2;;

let format_mismatch fmt1 fmt2 ib =
  scanf_bad_input ib (Scan_failure (format_mismatch_err fmt1 fmt2));;

(* Checking that 2 format string are type compatible. *)
let compatible_format_type fmt1 fmt2 =
  Printf.summarize_format_type fmt1 = Printf.summarize_format_type fmt2;;

(* Checking that [c] is indeed in the input, then skips it.
   In this case, the character c has been explicitely specified in the
   format as being mandatory in the input; hence we should fail with
   End_of_file in case of end_of_input.
   That's why we use checked_peek_char here. *)
let check_char ib c =
  let ci = Scanning.checked_peek_char ib in
  if ci != c then
    bad_input (Printf.sprintf "looking for %C, found %C" c ci) else
  Scanning.invalidate_current_char ib;;

(* Checks that the current char is indeed one of the stopper characters,
   then skips it.
   Be careful that if ib has no more character this procedure should
   just do nothing (since %s@c defaults to the entire rest of the
   buffer, when no character c can be found in the input). *)
let ignore_stoppers stps ib =
  if stps <> [] && not (Scanning.eof ib) then
  let ci = Scanning.peek_char ib in
  if List.memq ci stps then Scanning.invalidate_current_char ib else
  let sr = String.concat "" (List.map (String.make 1) stps) in
  bad_input
    (Printf.sprintf "looking for one of range %S, found %C" sr ci);;

(* Extracting tokens from ouput token buffer. *)

let token_char ib = (Scanning.token ib).[0];;

let token_string = Scanning.token;;

let token_bool ib =
  match Scanning.token ib with
  | "true" -> true
  | "false" -> false
  | s -> bad_input ("invalid boolean " ^ s);;

(* Extract an integer literal token.
   Since the functions Pervasives.*int*_of_string do not accept a leading +,
   we skip it if necessary. *)
let token_int_literal conv ib =
  let tok =
    match conv with
    | 'd' | 'i' | 'u' -> Scanning.token ib
    | 'o' -> "0o" ^ Scanning.token ib
    | 'x' | 'X' -> "0x" ^ Scanning.token ib
    | 'b' -> "0b" ^ Scanning.token ib
    | _ -> assert false in
  let l = String.length tok in
  if l = 0 || tok.[0] <> '+' then tok else String.sub tok 1 (l - 1);;

(* All the functions that convert a string to a number raise the exception
   Failure when the conversion is not possible.
   This exception is then trapped in kscanf. *)
let token_int conv ib = int_of_string (token_int_literal conv ib);;
let token_float ib = float_of_string (Scanning.token ib);;

(* To scan native ints, int32 and int64 integers.
   We cannot access to conversions to/from strings for those types,
   Nativeint.of_string, Int32.of_string, and Int64.of_string,
   since those modules are not available to Scanf.
   However, we can bind and use the corresponding primitives that are
   available in the runtime. *)
external nativeint_of_string: string -> nativeint = "caml_nativeint_of_string";;
external int32_of_string : string -> int32 = "caml_int32_of_string";;
external int64_of_string : string -> int64 = "caml_int64_of_string";;

let token_nativeint conv ib = nativeint_of_string (token_int_literal conv ib);;
let token_int32 conv ib = int32_of_string (token_int_literal conv ib);;
let token_int64 conv ib = int64_of_string (token_int_literal conv ib);;

(* Scanning numbers. *)

(* Digits scanning functions suppose that one character has been
   checked and is available, since they return at end of file with the
   currently found token selected. The digits scanning functions scan
   a possibly empty sequence of digits, (hence a successful scanning
   from one of those functions does not imply that the token is a
   well-formed number: to get a true number, it is mandatory to check
   that at least one digit is available before calling a digit
   scanning function). *)

(* The decimal case is treated especially for optimization purposes. *)
let rec scan_decimal_digits max ib =
  if max = 0 then max else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then max else
  match c with
  | '0' .. '9' as c ->
     let max = Scanning.store_char ib c max in
     scan_decimal_digits max ib
  | '_' ->
     let max = Scanning.ignore_char ib max in
     scan_decimal_digits max ib
  | _ -> max;;

let scan_decimal_digits_plus max ib =
  let c = Scanning.checked_peek_char ib in
  match c with
  | '0' .. '9' ->
    let max = Scanning.store_char ib c max in
    scan_decimal_digits max ib
  | c -> bad_input_char c;;

let scan_digits_plus digitp max ib =
  (* To scan numbers from other bases, we use a predicate argument to
     scan_digits. *)
  let rec scan_digits max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    match c with
    | c when digitp c ->
       let max = Scanning.store_char ib c max in
       scan_digits max
    | '_' ->
       let max = Scanning.ignore_char ib max in
       scan_digits max
    | _ -> max in

  let c = Scanning.checked_peek_char ib in
  if digitp c then
    let max = Scanning.store_char ib c max in
    scan_digits max
  else bad_input_char c;;

let is_binary_digit = function
  | '0' .. '1' -> true
  | _ -> false;;

let scan_binary_int = scan_digits_plus is_binary_digit;;

let is_octal_digit = function
  | '0' .. '7' -> true
  | _ -> false;;

let scan_octal_int = scan_digits_plus is_octal_digit;;

let is_hexa_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false;;

let scan_hexadecimal_int = scan_digits_plus is_hexa_digit;;

(* Scan a decimal integer. *)
let scan_unsigned_decimal_int = scan_decimal_digits_plus;;

let scan_sign max ib =
  let c = Scanning.checked_peek_char ib in
  match c with
  | '+' -> Scanning.store_char ib c max
  | '-' -> Scanning.store_char ib c max
  | c -> max;;

let scan_optionally_signed_decimal_int max ib =
  let max = scan_sign max ib in
  scan_unsigned_decimal_int max ib;;

(* Scan an unsigned integer that could be given in any (common) basis.
   If digits are prefixed by one of 0x, 0X, 0o, or 0b, the number is
   assumed to be written respectively in hexadecimal, hexadecimal,
   octal, or binary. *)
let scan_unsigned_int max ib =
  match Scanning.checked_peek_char ib with
  | '0' as c ->
      let max = Scanning.store_char ib c max in
      if max = 0 then max else
      let c = Scanning.peek_char ib in
      if Scanning.eof ib then max else
      begin match c with
      | 'x' | 'X' -> scan_hexadecimal_int (Scanning.store_char ib c max) ib
      | 'o' -> scan_octal_int (Scanning.store_char ib c max) ib
      | 'b' -> scan_binary_int (Scanning.store_char ib c max) ib
      | c -> scan_decimal_digits max ib end
  | c -> scan_unsigned_decimal_int max ib;;

let scan_optionally_signed_int max ib =
  let max = scan_sign max ib in
  scan_unsigned_int max ib;;

let scan_int_conv conv max ib =
  match conv with
  | 'b' -> scan_binary_int max ib
  | 'd' -> scan_optionally_signed_decimal_int max ib
  | 'i' -> scan_optionally_signed_int max ib
  | 'o' -> scan_octal_int max ib
  | 'u' -> scan_unsigned_decimal_int max ib
  | 'x' | 'X' -> scan_hexadecimal_int max ib
  | c -> assert false;;

(* Scanning floating point numbers. *)
(* Fractional part is optional and can be reduced to 0 digits. *)
let scan_frac_part max ib =
  if max = 0 then max else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then max else
  match c with
  | '0' .. '9' as c ->
    scan_decimal_digits (Scanning.store_char ib c max) ib
  | _ -> max;;

(* Exp part is optional and can be reduced to 0 digits. *)
let scan_exp_part max ib =
  if max = 0 then max else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then max else
  match c with
  | 'e' | 'E' as c ->
     scan_optionally_signed_decimal_int (Scanning.store_char ib c max) ib
  | _ -> max;;

(* Scan the integer part of a floating point number, (not using the
   Caml lexical convention since the integer part can be empty):
   an optional sign, followed by a possibly empty sequence of decimal
   digits (e.g. -.1). *)
let scan_int_part max ib =
  let max = scan_sign max ib in
  scan_decimal_digits max ib;;

let scan_float max ib =
  let max = scan_int_part max ib in
  if max = 0 then max else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then max else
  match c with
  | '.' ->
     let max = Scanning.store_char ib c max in
     let max = scan_frac_part max ib in
     scan_exp_part max ib
  | c -> scan_exp_part max ib;;

let scan_Float max ib =
  let max = scan_optionally_signed_decimal_int max ib in
  if max = 0 then bad_float () else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then bad_float () else
  match c with
  | '.' ->
     let max = Scanning.store_char ib c max in
     let max = scan_frac_part max ib in
     scan_exp_part max ib
  | 'e' | 'E' ->
     scan_exp_part max ib
  | c -> bad_float ();;

(* Scan a regular string: stops when encountering a space or one of the
   characters in stp. It also stops when the maximum number of
   characters has been read.*)
let scan_string stp max ib =
  let rec loop max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if stp == [] then
      match c with
      | ' ' | '\t' | '\n' | '\r' -> max
      | c -> loop (Scanning.store_char ib c max) else
    if List.memq c stp then Scanning.skip_char ib max else
    loop (Scanning.store_char ib c max) in
  loop max;;

(* Scan a char: peek strictly one character in the input, whatsoever. *)
let scan_char max ib =
  Scanning.store_char ib (Scanning.checked_peek_char ib) max;;

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

(* The integer value corresponding to the facial value of a valid
   decimal digit character. *)
let int_value_of_char c = int_of_char c - 48;;

let char_for_decimal_code c0 c1 c2 =
  let c =
    100 * int_value_of_char c0 +
     10 * int_value_of_char c1 +
          int_value_of_char c2 in
  if c < 0 || c > 255
  then bad_input (Printf.sprintf "bad char \\%c%c%c" c0 c1 c2)
  else char_of_int c;;

(* Called when encountering '\\' as starter of a char.
   Stops before the corresponding '\''. *)
let scan_backslash_char max ib =
  if max = 0 then bad_input "a char" else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then bad_input "a char" else
  match c with
  | '\\' | '\'' | '"' | 'n' | 't' | 'b' | 'r' (* '"' helping Emacs *) ->
     Scanning.store_char ib (char_for_backslash c) max
  | '0' .. '9' as c ->
     let get_digit () =
       let c = Scanning.next_char ib in
       match c with
       | '0' .. '9' as c -> c
       | c -> bad_input_escape c in
     let c0 = c in
     let c1 = get_digit () in
     let c2 = get_digit () in
     Scanning.store_char ib (char_for_decimal_code c0 c1 c2) (max - 2)
  | c -> bad_input_char c;;

let scan_Char max ib =
  let rec loop s max =
   if max = 0 then bad_input "a char" else
   let c = Scanning.checked_peek_char ib in
   if Scanning.eof ib then bad_input "a char" else
   match c, s with
   | '\'', 3 -> loop 2 (Scanning.ignore_char ib max)
   | '\'', 1 -> Scanning.ignore_char ib max
   | '\\', 2 -> loop 1 (scan_backslash_char (Scanning.ignore_char ib max) ib)
   | c, 2 -> loop 1 (Scanning.store_char ib c max)
   | c, _ -> bad_input_escape c in
  loop 3 max;;

let scan_String max ib =
  let rec loop s max =
    if max = 0 then bad_input "a string" else
    let c = Scanning.checked_peek_char ib in
    if Scanning.eof ib then bad_input "a string" else
    match c, s with
    | '"', true (* '"' helping Emacs *) ->
       loop false (Scanning.ignore_char ib max)
    | '"', false (* '"' helping Emacs *) ->
       Scanning.ignore_char ib max
    | '\\', false ->
       skip_spaces true (Scanning.ignore_char ib max)
    | c, false -> loop false (Scanning.store_char ib c max)
    | c, _ -> bad_input_char c
  and skip_spaces s max =
    if max = 0 then bad_input "a string" else
    let c = Scanning.checked_peek_char ib in
    if Scanning.eof ib then bad_input "a string" else
    match c, s with
    | '\n', true
    | ' ', false ->
       skip_spaces false (Scanning.ignore_char ib max)
    | '\\', false -> loop false max
    | c, false -> loop false (Scanning.store_char ib c max)
    | _, _ -> loop false (scan_backslash_char (max - 1) ib) in
  loop true max;;

let scan_bool max ib =
  if max < 4 then bad_input "a boolean" else
  let c = Scanning.checked_peek_char ib in
  if Scanning.eof ib then bad_input "a boolean" else
  let m =
    match c with
    | 't' -> 4
    | 'f' -> 5
    | _ -> bad_input "a boolean" in
  scan_string [] (min max m) ib;;

(* Reading char sets in %[...] conversions. *)
type char_set =
   | Pos_set of string (* Positive (regular) set. *)
   | Neg_set of string (* Negative (complementary) set. *);;

(* Char sets are read as sub-strings in the format string. *)
let read_char_set fmt i =
  let lim = String.length fmt - 1 in

  let rec find_in_set j =
    if j > lim then incomplete_format fmt else
    match fmt.[j] with
    | ']' -> j
    | c -> find_in_set (j + 1)

  and find_set i =
    if i > lim then incomplete_format fmt else
    match fmt.[i] with
    | ']' -> find_in_set (i + 1)
    | c -> find_in_set i in

  if i > lim then incomplete_format fmt else
  match fmt.[i] with
  | '^' ->
     let i = i + 1 in
     let j = find_set i in
     j, Neg_set (String.sub fmt i (j - i))
  | _ ->
     let j = find_set i in
     j, Pos_set (String.sub fmt i (j - i));;

(* Char sets are now represented as bitvects that are represented as
   byte strings. *)

(* Bit manipulations into bytes. *)
let set_bit_of_byte byte idx b =
  (b lsl idx) lor (byte land (* mask idx *) (lnot (1 lsl idx)));;

let get_bit_of_byte byte idx = (byte lsr idx) land 1;;

(* Bit manipulations in vectors of bytes represented as strings. *)
let set_bit_of_range r c b =
  let idx = c land 0x7 in
  let ydx = c lsr 3 in
  let byte = r.[ydx] in
  r.[ydx] <- char_of_int (set_bit_of_byte (int_of_char byte) idx b);;

let get_bit_of_range r c =
  let idx = c land 0x7 in
  let ydx = c lsr 3 in
  let byte = r.[ydx] in
  get_bit_of_byte (int_of_char byte) idx;;

(* Char sets represented as bitvects represented as fixed length byte
   strings. *)
(* Create a full or empty set of chars. *)
let make_range bit =
  let c = char_of_int (if bit = 0 then 0 else 0xFF) in
  String.make 32 c;;

(* Test is a char belongs to a set of chars. *)
let get_char_in_range r c = get_bit_of_range r (int_of_char c);;

let bit_not b = (lnot b) land 1;;

(* Build the bit vector corresponding to the set of characters
   that belongs to the string argument [set].
   (In the Scanf module [set] is always a sub-string of the format). *)
let make_char_bit_vect bit set =
  let r = make_range (bit_not bit) in
  let lim = String.length set - 1 in
  let rec loop bit rp i =
    if i <= lim then
    match set.[i] with
    | '-' when rp ->
       (* if i = 0 then rp is false (since the initial call is
          loop bit false 0). Hence i >= 1 and the following is safe. *)
       let c1 = set.[i - 1] in
       let i = i + 1 in
       if i > lim then loop bit false (i - 1) else
       let c2 = set.[i] in
       for j = int_of_char c1 to int_of_char c2 do
         set_bit_of_range r j bit done;
       loop bit false (i + 1)
    | c ->
       set_bit_of_range r (int_of_char set.[i]) bit;
       loop bit true (i + 1) in
  loop bit false 0;
  r;;

(* Compute the predicate on chars corresponding to a char set. *)
let make_pred bit set stp =
  let r = make_char_bit_vect bit set in
  List.iter
    (fun c -> set_bit_of_range r (int_of_char c) (bit_not bit)) stp;
  (fun c -> get_char_in_range r c);;

let make_setp stp char_set =
  match char_set with
  | Pos_set set ->
      begin match String.length set with
      | 0 -> (fun c -> 0)
      | 1 ->
          let p = set.[0] in
          (fun c -> if c == p then 1 else 0)
      | 2 ->
          let p1 = set.[0] and p2 = set.[1] in
          (fun c -> if c == p1 || c == p2 then 1 else 0)
      | 3 ->
          let p1 = set.[0] and p2 = set.[1] and p3 = set.[2] in
          if p2 = '-' then make_pred 1 set stp else
          (fun c -> if c == p1 || c == p2 || c == p3 then 1 else 0)
      | n -> make_pred 1 set stp
      end
  | Neg_set set ->
      begin match String.length set with
      | 0 -> (fun c -> 1)
      | 1 ->
          let p = set.[0] in
          (fun c -> if c != p then 1 else 0)
      | 2 ->
          let p1 = set.[0] and p2 = set.[1] in
          (fun c -> if c != p1 && c != p2 then 1 else 0)
      | 3 ->
          let p1 = set.[0] and p2 = set.[1] and p3 = set.[2] in
          if p2 = '-' then make_pred 0 set stp else
          (fun c -> if c != p1 && c != p2 && c != p3 then 1 else 0)
      | n -> make_pred 0 set stp
      end;;

let setp_table = Hashtbl.create 7;;

let add_setp stp char_set setp =
  let char_set_tbl =
    try Hashtbl.find setp_table char_set with
    | Not_found ->
        let char_set_tbl = Hashtbl.create 3 in
        Hashtbl.add setp_table char_set char_set_tbl;
        char_set_tbl in
  Hashtbl.add char_set_tbl stp setp;;

let find_setp stp char_set =
  try Hashtbl.find (Hashtbl.find setp_table char_set) stp with
  | Not_found ->
     let setp = make_setp stp char_set in
     add_setp stp char_set setp;
     setp;;

let scan_chars_in_char_set stp char_set max ib =
  let rec loop_pos1 cp1 max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if c == cp1
    then loop_pos1 cp1 (Scanning.store_char ib c max)
    else max
  and loop_pos2 cp1 cp2 max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if c == cp1 || c == cp2
    then loop_pos2 cp1 cp2 (Scanning.store_char ib c max)
    else max
  and loop_pos3 cp1 cp2 cp3 max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if c == cp1 || c == cp2 || c == cp3
    then loop_pos3 cp1 cp2 cp3 (Scanning.store_char ib c max)
    else max
  and loop_neg1 cp1 max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if c != cp1
    then loop_neg1 cp1 (Scanning.store_char ib c max)
    else max
  and loop_neg2 cp1 cp2 max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if c != cp1 && c != cp2
    then loop_neg2 cp1 cp2 (Scanning.store_char ib c max)
    else max
  and loop_neg3 cp1 cp2 cp3 max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if c != cp1 && c != cp2 && c != cp3
    then loop_neg3 cp1 cp2 cp3 (Scanning.store_char ib c max)
    else max
  and loop setp max =
    if max = 0 then max else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then max else
    if setp c == 1
    then loop setp (Scanning.store_char ib c max)
    else max in

  let max =
    match char_set with
    | Pos_set set ->
        begin match String.length set with
        | 0 -> loop (fun c -> 0) max
        | 1 -> loop_pos1 set.[0] max
        | 2 -> loop_pos2 set.[0] set.[1] max
        | 3 when set.[1] != '-' -> loop_pos3 set.[0] set.[1] set.[2] max
        | n -> loop (find_setp stp char_set) max end
    | Neg_set set ->
        begin match String.length set with
        | 0 -> loop (fun c -> 1) max
        | 1 -> loop_neg1 set.[0] max
        | 2 -> loop_neg2 set.[0] set.[1] max
        | 3 when set.[1] != '-' -> loop_neg3 set.[0] set.[1] set.[2] max
        | n -> loop (find_setp stp char_set) max end in
  ignore_stoppers stp ib;
  max;;

let get_count t ib =
  match t with
  | 'l' -> Scanning.line_count ib
  | 'n' -> Scanning.char_count ib
  | _ -> Scanning.token_count ib;;

let rec skip_whites ib =
  let c = Scanning.peek_char ib in
  if not (Scanning.eof ib) then begin
    match c with
    | ' ' | '\t' | '\n' | '\r' ->
      Scanning.invalidate_current_char ib; skip_whites ib
    | _ -> ()
  end;;

external format_to_string :
 ('a, 'b, 'c, 'd) format4 -> string = "%identity";;
external string_to_format :
 string -> ('a, 'b, 'c, 'd) format4 = "%identity";;

(* The [kscanf] main scanning function.
   It takes as arguments:
     - an input buffer [ib] from which to read characters,
     - an error handling function [ef],
     - a format [fmt] that specifies what to read in the input,
     - and a function [f] to pass the tokens read to.

   Then [kscanf] scans the format and the buffer in parallel to find
   out tokens as specified by the format; when it founds one token, it
   converts it as specified, remembers the converted value as a future
   argument to the function [f], and continues scanning.

   If the entire scanning succeeds (i.e. the format string has been
   exhausted and the buffer has provided tokens according to the
   format string), the tokens are applied to [f].

   If the scanning or some conversion fails, the main scanning function
   aborts and applies the scanning buffer and a string that explains
   the error to the error handling function [ef] (the error continuation). *)
let kscanf ib ef fmt f =
  let fmt = format_to_string fmt in
  let lim = String.length fmt - 1 in

  let return v = Obj.magic v () in
  let delay f x () = f x in
  let stack f = delay (return f) in
  let no_stack f x = f in

  let rec scan_fmt f i =
    if i > lim then f else
    match fmt.[i] with
    | ' ' -> skip_whites ib; scan_fmt f (i + 1)
    | '%' ->
        if i > lim then incomplete_format fmt else
        scan_conversion false max_int f (i + 1)
    | '@' ->
        let i = i + 1 in
        if i > lim then incomplete_format fmt else begin
        check_char ib fmt.[i];
        scan_fmt f (i + 1) end
    | c -> check_char ib c; scan_fmt f (i + 1)

  and scan_conversion skip max f i =
    let stack = if skip then no_stack else stack in
    match fmt.[i] with
    | '%' as conv ->
        check_char ib conv; scan_fmt f (i + 1)
    | 'c' when max = 0 ->
        let c = Scanning.checked_peek_char ib in
        scan_fmt (stack f c) (i + 1)
    | 'c' | 'C' as conv ->
        if max <> 1 && max <> max_int then bad_conversion fmt i conv else
        let _x =
          if conv = 'c' then scan_char max ib else scan_Char max ib in
        scan_fmt (stack f (token_char ib)) (i + 1)
    | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' as conv ->
        let _x = scan_int_conv conv max ib in
        scan_fmt (stack f (token_int conv ib)) (i + 1)
    | 'f' | 'g' | 'G' | 'e' | 'E' ->
        let _x = scan_float max ib in
        scan_fmt (stack f (token_float ib)) (i + 1)
    | 'F' ->
        let _x = scan_Float max ib in
        scan_fmt (stack f (token_float ib)) (i + 1)
    | 's' ->
        let i, stp = scan_fmt_stoppers (i + 1) in
        let _x = scan_string stp max ib in
        scan_fmt (stack f (token_string ib)) (i + 1)
    | '[' ->
        let i, char_set = read_char_set fmt (i + 1) in
        let i, stp = scan_fmt_stoppers (i + 1) in
        let _x = scan_chars_in_char_set stp char_set max ib in
        scan_fmt (stack f (token_string ib)) (i + 1)
    | 'S' ->
        let _x = scan_String max ib in
        scan_fmt (stack f (token_string ib)) (i + 1)
    | 'B' | 'b' ->
        let _x = scan_bool max ib in
        scan_fmt (stack f (token_bool ib)) (i + 1)
    | 'l' | 'n' | 'L' as typ ->
        let i = i + 1 in
        if i > lim then scan_fmt (stack f (get_count typ ib)) i else begin
        match fmt.[i] with
        | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' as conv ->
            let _x = scan_int_conv conv max ib in
            begin match typ with
            | 'l' -> scan_fmt (stack f (token_int32 conv ib)) (i + 1)
            | 'n' -> scan_fmt (stack f (token_nativeint conv ib)) (i + 1)
            | _ -> scan_fmt (stack f (token_int64 conv ib)) (i + 1) end
        | c -> scan_fmt (stack f (get_count typ ib)) i end
    | 'N' as conv ->
        scan_fmt (stack f (get_count conv ib)) (i + 1)
    | '!' ->
        if Scanning.end_of_input ib then scan_fmt f (i + 1)
        else bad_input "end of input not found"
    | '_' ->
        if i > lim then incomplete_format fmt else
        scan_conversion true max f (i + 1)
    | '0' .. '9' as conv ->
        let rec read_width accu i =
          if i > lim then accu, i else
          match fmt.[i] with
          | '0' .. '9' as c ->
             let accu = 10 * accu + int_value_of_char c in
             read_width accu (i + 1)
          | _ -> accu, i in
        let max, i = read_width (int_value_of_char conv) (i + 1) in
        if i > lim then incomplete_format fmt else begin
        match fmt.[i] with
        | '.' ->
          let p, i = read_width 0 (i + 1) in
          scan_conversion skip (max + p + 1) f i
        | _ -> scan_conversion skip max f i end
    | '(' | '{' as conv ->
        let i = succ i in
        let j =
          Printf.sub_format incomplete_format bad_conversion conv fmt i + 1 in
        let mf = String.sub fmt i (j - i - 2) in
        let _x = scan_String max ib in
        let rf = token_string ib in
        if not (compatible_format_type mf rf)
          then format_mismatch rf mf ib else
        if conv = '{' then scan_fmt (stack f rf) j else
        let nf = scan_fmt (Obj.magic rf) 0 in
        scan_fmt (stack f nf) j
    | c -> bad_conversion fmt i c

  and scan_fmt_stoppers i =
    if i > lim then i - 1, [] else
    match fmt.[i] with
    | '@' when i < lim -> let i = i + 1 in i, [fmt.[i]]
    | '@' when i = lim -> incomplete_format fmt
    | _ -> i - 1, [] in

  Scanning.reset_token ib;

  let v =
    try scan_fmt (fun () -> f) 0 with
    | (Scan_failure _ | Failure _ | End_of_file) as exc ->
        stack (delay ef ib) exc in
  return v;;

let bscanf ib = kscanf ib scanf_bad_input;;

let fscanf ic = bscanf (Scanning.from_channel ic);;

let sscanf s = bscanf (Scanning.from_string s);;

let scanf fmt = bscanf Scanning.stdib fmt;;

let bscanf_format ib fmt f =
  let fmt = format_to_string fmt in
  let fmt1 = ignore (scan_String max_int ib); token_string ib in
  if not (compatible_format_type fmt1 fmt) then
    format_mismatch fmt1 fmt ib else
  let fresh_fmt1 = String.copy fmt1 in
  f (string_to_format fresh_fmt1);;

let sscanf_format s fmt =
  let fmt = format_to_string fmt in
  let fmt1 = s in
  if not (compatible_format_type fmt1 fmt) then
    bad_input (format_mismatch_err fmt1 fmt) else
  let fresh_fmt1 = String.copy fmt1 in
  string_to_format fresh_fmt1;;
