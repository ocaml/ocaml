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
(** The scanning buffer reading from [stdin].
    [stdib] is equivalent to [Scanning.from_channel stdin]. *)

val next_char : scanbuf -> unit;;
(** [Scanning.next_char scanbuf] advance the scanning buffer for
    one character.
    If no more character can be read, sets a end of file condition and
    returns '\000'. *)

val peek_char : scanbuf -> char;;
(** [Scanning.peek_char scanbuf] returns the current char available in
    the buffer. *)

val cautious_peek_char : scanbuf -> char;;
(** [Scanning.cautious_peek_char scanbuf] returns the current char
    available in the buffer or tries to read one if none has ever been
    read. 
    If no character can be read, sets a end of file condition and
    returns '\000'. *)

val checked_peek_char : scanbuf -> char;;
(** Same as above but always returns a valid char or fails:
    instead of returning a null char when the reading method of the
    input buffer has reached an end of file, the function raises exception
    [End_of_file]. *)

val store_char : scanbuf -> char -> int -> int;;
(** [Scanning.store_char scanbuf c lim] adds [c] to the token buffer
    of the scanning buffer. It also advances the scanning buffer for one
    character and returns [lim - 1], indicating the new limit
    for the length of the current token. *)

val skip_char : scanbuf -> char -> int -> int;;
(** [Scanning.skip_char scanbuf c lim] is similar to [store_char] but
    it ignores (does not store in the token buffer) the character [c]. *)

val token : scanbuf -> string;;
(** [Scanning.token scanbuf] returns the string stored into the token
    buffer of the scanning buffer: it returns the token matched by the
    format. *)

val reset_token : scanbuf -> unit;;
(** [Scanning.reset_token scanbuf] resets the token buffer of
    the given scanning buffer. *)

val char_count : scanbuf -> int;;
(** [Scanning.char_count scanbuf] returns the number of characters
    read so far from the given buffer. *)

val line_count : scanbuf -> int;;
(** [Scanning.line_count scanbuf] returns the number of new line
    characters read so far from the given buffer. *)

val token_count : scanbuf -> int;;
(** [Scanning.token_count scanbuf] returns the number of tokens read
    so far from [scanbuf]. *)

val eof : scanbuf -> bool;;
(** [Scanning.eof scanbuf] returns the current value of the end of input
    condition of the given buffer, no validity test is performed. *)

val end_of_input : scanbuf -> bool;;
(** [Scanning.end_of_input scanbuf] tests the end of input condition
    of the given buffer. *)

val beginning_of_input : scanbuf -> bool;;
(** [Scanning.beginning_of_input scanbuf] tests the beginning of input
    condition of the given buffer. *)

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
  mutable bof : bool;
  mutable cur_char : char;
  mutable char_count : int;
  mutable line_count : int;
  mutable token_count : int;
  mutable get_next_char : unit -> char;
  tokbuf : Buffer.t;
  file_name : file_name;
};;

(* Reads a new character from input buffer.  Next_char never fails,
   even in case of end of input: it then simply sets the end of file
   condition. *)
let next_char ib =
  try
   let c = ib.get_next_char () in
   ib.cur_char <- c;
   ib.char_count <- ib.char_count + 1;
   if c == '\n' then ib.line_count <- ib.line_count + 1
  with End_of_file ->
   ib.cur_char <- '\000';
   ib.eof <- true;;

let cautious_peek_char ib =
  if ib.bof then begin
    next_char ib;
    if ib.char_count > 0 then ib.bof <- false end;
  ib.cur_char;;

(* Returns a valid current char for the input buffer.  In particular
   no irrelevant null character (as set by [next_char] in case of end
   of input) is returned, since [End_of_file] is raised when
   [next_char] sets the end of file condition while trying to read a
   new character. *)
let checked_peek_char ib =
  let c = cautious_peek_char ib in
  if ib.eof then raise End_of_file;
  c;;

let peek_char ib = ib.cur_char;;
let eof ib = ib.eof;;
let beginning_of_input ib = ib.bof;;
let end_of_input ib =
  let c = cautious_peek_char ib in
  ib.eof;;
let char_count ib = ib.char_count;;
let line_count ib = ib.line_count;;
let reset_token ib = Buffer.reset ib.tokbuf;;

let token ib =
  let tokbuf = ib.tokbuf in
  let tok = Buffer.contents tokbuf in
  Buffer.clear tokbuf;
  ib.token_count <- ib.token_count + 1;
  tok;;

let token_count ib = ib.token_count;;

let store_char ib c max =
  Buffer.add_char ib.tokbuf c;
  next_char ib;
  max - 1;;

let skip_char ib c max =
  next_char ib;
  max - 1;;

let default_token_buffer_size = 1024;;

let create fname next = {
  eof = false;
  bof = true;
  cur_char = '\000';
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
  create "string" next;;

let from_function = create "function";;

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

let from_channel = from_input_channel "in_channel";;

let stdib = from_input_channel "stdin" stdin;;
(** The scanning buffer reading from [stdin].*)

end;;

(** Formatted input functions. *)

(* Reporting errors. *)
exception Scan_failure of string;;

let bad_input s = raise (Scan_failure s);;
let bad_input_char c = bad_input (String.make 1 c);;

let bad_input_escape c =
  bad_input (Printf.sprintf "illegal escape character %C" c);;

let scanf_bad_input ib = function
  | Scan_failure s | Failure s ->
      let i = Scanning.char_count ib in
      bad_input (Printf.sprintf "scanf: bad input at char number %i: %S" i s)
  | x -> raise x;;

let bad_format fmt i fc =
  invalid_arg
    (Printf.sprintf
       "scanf: bad conversion %%%c, at char number %i in format %S" fc i fmt);;

let bad_float () = bad_input "no dot or exponent part found in float token";;

(* Checking that the current char is indeed one of range, then skip it. *)
let check_char_in range ib =
  if range <> [] && not (Scanning.end_of_input ib) then
  let ci = Scanning.checked_peek_char ib in
  if List.memq ci range then Scanning.next_char ib else
  let sr = String.concat "" (List.map (String.make 1) range) in
  bad_input
    (Printf.sprintf "looking for one of range %S, found %C" sr ci);;

(* Checking that [c] is indeed in the input, then skip it. *)
let check_char ib c =
  let ci = Scanning.checked_peek_char ib in
  if ci != c
  then bad_input (Printf.sprintf "looking for %C, found %C" c ci)
  else Scanning.next_char ib;;

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
let scan_decimal_digits max ib =
  let rec loop inside max =
    if max = 0 || Scanning.eof ib then max else
    match Scanning.cautious_peek_char ib with
    | '0' .. '9' as c ->
        let max = Scanning.store_char ib c max in
        loop true max
    | '_' as c when inside ->
       let max = Scanning.skip_char ib c max in
       loop true max
    | c -> max in
  loop false max;;

(* To scan numbers from other bases, we use a predicate argument to
   scan_digits. *)
let scan_digits digitp max ib =
  let rec loop inside max =
    if max = 0 || Scanning.eof ib then max else
    match Scanning.cautious_peek_char ib with
    | c when digitp c ->
       let max = Scanning.store_char ib c max in
       loop true max
    | '_' as c when inside ->
       let max = Scanning.skip_char ib c max in
       loop true max
    | _ -> max in
  loop false max;;

let scan_digits_plus digitp max ib =
  let c = Scanning.checked_peek_char ib in
  if digitp c then
    let max = Scanning.store_char ib c max in
    scan_digits digitp max ib
  else bad_input_char c;;

let is_binary_digit = function
  | '0' .. '1' -> true
  | _ -> false;;

let scan_binary_digits = scan_digits is_binary_digit;;
let scan_binary_int = scan_digits_plus is_binary_digit;;

let is_octal_digit = function
  | '0' .. '7' -> true
  | _ -> false;;

let scan_octal_digits = scan_digits is_octal_digit;;
let scan_octal_int = scan_digits_plus is_octal_digit;;

let is_hexa_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false;;

let scan_hexadecimal_digits = scan_digits is_hexa_digit;;
let scan_hexadecimal_int = scan_digits_plus is_hexa_digit;;

(* Scan a decimal integer. *)
let scan_unsigned_decimal_int max ib =
  match Scanning.checked_peek_char ib with
  | '0' .. '9' as c ->
      let max = Scanning.store_char ib c max in
      scan_decimal_digits max ib
  | c -> bad_input_char c;;

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
      if max = 0 || Scanning.eof ib then max else
      let c = Scanning.peek_char ib in
      begin match c with
      | 'x' | 'X' -> scan_hexadecimal_digits (Scanning.store_char ib c max) ib
      | 'o' -> scan_octal_digits (Scanning.store_char ib c max) ib
      | 'b' -> scan_binary_digits (Scanning.store_char ib c max) ib
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
  if max = 0 || Scanning.eof ib then max else
  scan_decimal_digits max ib;;

(* Exp part is optional and can be reduced to 0 digits. *)
let scan_exp_part max ib =
  if max = 0 || Scanning.eof ib then max else
  let c = Scanning.peek_char ib in
  match c with
  | 'e' | 'E' as c ->
     scan_optionally_signed_decimal_int (Scanning.store_char ib c max) ib
  | _ -> max;;

(* An optional sign followed by a possibly empty sequence of decimal digits. *)
let scan_optionally_signed_decimal_digits max ib =
  let max = scan_sign max ib in
  scan_decimal_digits max ib;;

(* Scan the integer part of a floating point number, (not using the
   Caml lexical convention since the integer part can be empty). *)
let scan_int_part = scan_optionally_signed_decimal_digits;;

let scan_float max ib =
  let max = scan_int_part max ib in
  if max = 0 || Scanning.eof ib then max else
  let c = Scanning.peek_char ib in
  match c with
  | '.' ->
     let max = Scanning.store_char ib c max in
     let max = scan_frac_part max ib in
     scan_exp_part max ib
  | c -> scan_exp_part max ib;;

let scan_Float max ib =
  let max = scan_optionally_signed_decimal_int max ib in
  if max = 0 || Scanning.eof ib then bad_float () else
  let c = Scanning.peek_char ib in
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
    if max = 0 || Scanning.end_of_input ib then max else
    let c = Scanning.checked_peek_char ib in
    if stp == [] then
      match c with
      | ' ' | '\t' | '\n' | '\r' -> max
      | c -> loop (Scanning.store_char ib c max) else
    if List.mem c stp then max else
    loop (Scanning.store_char ib c max) in
  let max = loop max in
  check_char_in stp ib;
  max;;

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
  if max = 0 || Scanning.eof ib then bad_input "a char" else
  let c = Scanning.peek_char ib in
  match c with
  | '\\' | '\'' | '"' | 'n' | 't' | 'b' | 'r' (* '"' helping Emacs *) ->
     Scanning.store_char ib (char_for_backslash c) max
  | '0' .. '9' as c ->
     let get_digit () =
       Scanning.next_char ib;
       let c = Scanning.peek_char ib in
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
   if max = 0 || Scanning.eof ib then bad_input "a char" else
   let c = Scanning.checked_peek_char ib in
   match c, s with
   | '\'', 3 -> Scanning.next_char ib; loop 2 (max - 1)
   | '\'', 1 -> Scanning.next_char ib; max - 1
   | '\\', 2 -> Scanning.next_char ib;
                loop 1 (scan_backslash_char (max - 1) ib)
   | c, 2 -> loop 1 (Scanning.store_char ib c max)
   | c, _ -> bad_input_escape c in
  loop 3 max;;

let scan_String max ib =
  let rec loop s max =
    if max = 0 || Scanning.eof ib then bad_input "a string" else
    let c = Scanning.checked_peek_char ib in
    match c, s with
    | '"', true (* '"' helping Emacs *) ->
       Scanning.next_char ib; loop false (max - 1)
    | '"', false (* '"' helping Emacs *) ->
       Scanning.next_char ib; max - 1
    | '\\', false ->
       Scanning.next_char ib; skip_spaces true (max - 1)
    | c, false -> loop false (Scanning.store_char ib c max)
    | c, _ -> bad_input_char c
  and skip_spaces s max =
    if max = 0 || Scanning.eof ib then bad_input "a string" else
    let c = Scanning.checked_peek_char ib in
    match c, s with
    | '\n', true
    | ' ', false ->
       Scanning.next_char ib; skip_spaces false (max - 1)
    | '\\', false -> loop false max
    | c, false -> loop false (Scanning.store_char ib c max)
    | _, _ -> loop false (scan_backslash_char (max - 1) ib) in
  loop true max;;

let scan_bool max ib =
  if max < 4 || Scanning.eof ib then bad_input "a boolean" else
  let m =
    match Scanning.checked_peek_char ib with
    | 't' -> 4
    | 'f' -> 5
    | _ -> 0 in
  scan_string [] (min max m) ib;;

(* Reading char sets in %[...] conversions. *)
type char_set =
   | Pos_set of string (* Positive (regular) set. *)
   | Neg_set of string (* Negative (complementary) set. *);;

(* Char sets are read as sub-strings in the format string. *)
let read_char_set fmt i =
  let lim = String.length fmt - 1 in

  let rec find_in_set j =
    if j > lim then bad_format fmt j fmt.[lim - 1] else
    match fmt.[j] with
    | ']' -> j
    | c -> find_in_set (j + 1)

  and find_set i =
    if i > lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | ']' -> find_in_set (i + 1)
    | c -> find_in_set i in

  if i > lim then bad_format fmt i fmt.[lim - 1] else
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

(* Build the bit vector corresponding to a char set read in the format. *)
let make_bv bit set =
  let r = make_range (bit_not bit) in
  let lim = String.length set - 1 in
  let rec loop bit rp i =
    if i <= lim then
    match set.[i] with
    | '-' when rp ->
       (* if i = 0 then rp is false (since the initial call is loop bit false 0)
          hence i >= 1 and the following is safe. *)
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
  let r = make_bv bit set in
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
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if c == cp1
    then loop_pos1 cp1 (Scanning.store_char ib c max)
    else max
  and loop_pos2 cp1 cp2 max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if c == cp1 || c == cp2
    then loop_pos2 cp1 cp2 (Scanning.store_char ib c max)
    else max
  and loop_pos3 cp1 cp2 cp3 max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if c == cp1 || c == cp2 || c == cp3
    then loop_pos3 cp1 cp2 cp3 (Scanning.store_char ib c max)
    else max
  and loop_neg1 cp1 max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if c != cp1
    then loop_neg1 cp1 (Scanning.store_char ib c max)
    else max
  and loop_neg2 cp1 cp2 max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if c != cp1 && c != cp2
    then loop_neg2 cp1 cp2 (Scanning.store_char ib c max)
    else max
  and loop_neg3 cp1 cp2 cp3 max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if c != cp1 && c != cp2 && c != cp3
    then loop_neg3 cp1 cp2 cp3 (Scanning.store_char ib c max)
    else max
  and loop setp max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if setp c == 1 then loop setp (Scanning.store_char ib c max) else
    max in

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
  check_char_in stp ib;
  max;;

let get_count t ib =
  match t with
  | 'l' -> Scanning.line_count ib
  | 'n' -> Scanning.char_count ib
  | _ -> Scanning.token_count ib;;

let skip_whites ib =
  let rec loop = function
  | ' ' | '\t' | '\n' | '\r' ->
     Scanning.next_char ib;
     if not (Scanning.eof ib) then loop (Scanning.peek_char ib)
  | _ -> () in
  if not (Scanning.eof ib) then
  loop (Scanning.cautious_peek_char ib);;

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
  let fmt = string_of_format fmt in
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
        if i > lim then bad_format fmt i '%' else
        scan_conversion false max_int f (i + 1)
    | '@' as t ->
        let i = i + 1 in
        if i > lim then bad_format fmt (i - 1) t else begin
        check_char ib fmt.[i];
        scan_fmt f (i + 1) end
    | c -> check_char ib c; scan_fmt f (i + 1)

  and scan_conversion skip max f i =
    let stack = if skip then no_stack else stack in
    match fmt.[i] with
    | '%' as c ->
        check_char ib c; scan_fmt f (i + 1)
    | 'c' when max = 0 ->
        let c = Scanning.checked_peek_char ib in
        scan_fmt (stack f c) (i + 1)
    | 'c' | 'C' as conv ->
        if max <> 1 && max <> max_int then bad_format fmt i conv else
        let x =
          if conv = 'c' then scan_char max ib else scan_Char max ib in
        scan_fmt (stack f (token_char ib)) (i + 1)
    | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' as conv ->
        let x = scan_int_conv conv max ib in
        scan_fmt (stack f (token_int conv ib)) (i + 1)
    | 'f' | 'g' | 'G' | 'e' | 'E' ->
        let x = scan_float max ib in
        scan_fmt (stack f (token_float ib)) (i + 1)
    | 'F' ->
        let x = scan_Float max ib in
        scan_fmt (stack f (token_float ib)) (i + 1)
    | 's' ->
        let i, stp = scan_fmt_stoppers (i + 1) in
        let x = scan_string stp max ib in
        scan_fmt (stack f (token_string ib)) (i + 1)
    | '[' ->
        let i, char_set = read_char_set fmt (i + 1) in
        let i, stp = scan_fmt_stoppers (i + 1) in
        let x = scan_chars_in_char_set stp char_set max ib in
        scan_fmt (stack f (token_string ib)) (i + 1)
    | 'S' ->
        let x = scan_String max ib in
        scan_fmt (stack f (token_string ib)) (i + 1)
    | 'B' | 'b' ->
        let x = scan_bool max ib in
        scan_fmt (stack f (token_bool ib)) (i + 1)
    | 'l' | 'n' | 'L' as t ->
        let i = i + 1 in
        if i > lim then scan_fmt (stack f (get_count t ib)) i else begin
        match fmt.[i] with
        | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' as conv ->
            let x = scan_int_conv conv max ib in
            begin match t with
            | 'l' -> scan_fmt (stack f (token_int32 conv ib)) (i + 1)
            | 'L' -> scan_fmt (stack f (token_int64 conv ib)) (i + 1)
            | _ -> scan_fmt (stack f (token_nativeint conv ib)) (i + 1) end
        | c -> scan_fmt (stack f (get_count t ib)) i end
    | 'N' as t ->
        scan_fmt (stack f (get_count t ib)) (i + 1)
    | '!' as c ->
        if Scanning.end_of_input ib then scan_fmt f (i + 1)
        else bad_input "end of input not found"
    | '_' ->
        if i > lim then bad_format fmt i fmt.[lim - 1] else
        scan_conversion true max f (i + 1)
    | '0' .. '9' as c ->
        let rec read_width accu i =
          if i > lim then accu, i else
          match fmt.[i] with
          | '0' .. '9' as c ->
             let accu = 10 * accu + int_value_of_char c in
             read_width accu (i + 1)
          | _ -> accu, i in
        let max, i = read_width (int_value_of_char c) (i + 1) in
        if i > lim then bad_format fmt i fmt.[lim - 1] else
        scan_conversion skip max f i
    | c -> bad_format fmt i c

  and scan_fmt_stoppers i =
    if i > lim then i - 1, [] else
    match fmt.[i] with
    | '@' when i < lim -> let i = i + 1 in i, [fmt.[i]]
    | '@' as c when i = lim -> bad_format fmt i c
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
