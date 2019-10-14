(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open CamlinternalFormatBasics
open CamlinternalFormat

(* alias to avoid warning for ambiguity between
   Stdlib.format6
   and CamlinternalFormatBasics.format6

   (the former is in fact an alias for the latter,
    but the ambiguity warning doesn't care)
*)
type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6


(* The run-time library for scanners. *)

(* Scanning buffers. *)
module type SCANNING = sig

  type in_channel

  type scanbuf = in_channel

  type file_name = string

  val stdin : in_channel
  (* The scanning buffer reading from [Stdlib.stdin].
     [stdib] is equivalent to [Scanning.from_channel Stdlib.stdin]. *)

  val stdib : in_channel
  (* An alias for [Scanf.stdin], the scanning buffer reading from
     [Stdlib.stdin]. *)

  val next_char : scanbuf -> char
  (* [Scanning.next_char ib] advance the scanning buffer for
     one character.
     If no more character can be read, sets a end of file condition and
     returns '\000'. *)

  val invalidate_current_char : scanbuf -> unit
  (* [Scanning.invalidate_current_char ib] mark the current_char as already
     scanned. *)

  val peek_char : scanbuf -> char
  (* [Scanning.peek_char ib] returns the current char available in
     the buffer or reads one if necessary (when the current character is
     already scanned).
     If no character can be read, sets an end of file condition and
     returns '\000'. *)

  val checked_peek_char : scanbuf -> char
  (* Same as [Scanning.peek_char] above but always returns a valid char or
     fails: instead of returning a null char when the reading method of the
     input buffer has reached an end of file, the function raises exception
     [End_of_file]. *)

  val store_char : int -> scanbuf -> char -> int
  (* [Scanning.store_char lim ib c] adds [c] to the token buffer
     of the scanning buffer [ib]. It also advances the scanning buffer for
     one character and returns [lim - 1], indicating the new limit for the
     length of the current token. *)

  val skip_char : int -> scanbuf -> int
  (* [Scanning.skip_char lim ib] ignores the current character. *)

  val ignore_char : int -> scanbuf -> int
  (* [Scanning.ignore_char ib lim] ignores the current character and
     decrements the limit. *)

  val token : scanbuf -> string
  (* [Scanning.token ib] returns the string stored into the token
     buffer of the scanning buffer: it returns the token matched by the
     format. *)

  val reset_token : scanbuf -> unit
  (* [Scanning.reset_token ib] resets the token buffer of
     the given scanning buffer. *)

  val char_count : scanbuf -> int
  (* [Scanning.char_count ib] returns the number of characters
     read so far from the given buffer. *)

  val line_count : scanbuf -> int
  (* [Scanning.line_count ib] returns the number of new line
     characters read so far from the given buffer. *)

  val token_count : scanbuf -> int
  (* [Scanning.token_count ib] returns the number of tokens read
     so far from [ib]. *)

  val eof : scanbuf -> bool
  (* [Scanning.eof ib] returns the end of input condition
     of the given buffer. *)

  val end_of_input : scanbuf -> bool
  (* [Scanning.end_of_input ib] tests the end of input condition
     of the given buffer (if no char has ever been read, an attempt to
     read one is performed). *)

  val beginning_of_input : scanbuf -> bool
  (* [Scanning.beginning_of_input ib] tests the beginning of input
     condition of the given buffer. *)

  val name_of_input : scanbuf -> string
  (* [Scanning.name_of_input ib] returns the name of the character
     source for input buffer [ib]. *)

  val open_in : file_name -> in_channel
  val open_in_bin : file_name -> in_channel
  val from_file : file_name -> in_channel
  val from_file_bin : file_name -> in_channel
  val from_string : string -> in_channel
  val from_function : (unit -> char) -> in_channel
  val from_channel : Stdlib.in_channel -> in_channel

  val close_in : in_channel -> unit

  val memo_from_channel : Stdlib.in_channel -> in_channel
  (* Obsolete. *)

end


module Scanning : SCANNING = struct

  (* The run-time library for scanf. *)

  type file_name = string

  type in_channel_name =
    | From_channel of Stdlib.in_channel
    | From_file of file_name * Stdlib.in_channel
    | From_function
    | From_string


  type in_channel = {
    mutable ic_eof : bool;
    mutable ic_current_char : char;
    mutable ic_current_char_is_valid : bool;
    mutable ic_char_count : int;
    mutable ic_line_count : int;
    mutable ic_token_count : int;
    mutable ic_get_next_char : unit -> char;
    ic_token_buffer : Buffer.t;
    ic_input_name : in_channel_name;
  }


  type scanbuf = in_channel

  let null_char = '\000'

  (* Reads a new character from input buffer.
     Next_char never fails, even in case of end of input:
     it then simply sets the end of file condition. *)
  let next_char ib =
    try
      let c = ib.ic_get_next_char () in
      ib.ic_current_char <- c;
      ib.ic_current_char_is_valid <- true;
      ib.ic_char_count <- succ ib.ic_char_count;
      if c = '\n' then ib.ic_line_count <- succ ib.ic_line_count;
      c with
    | End_of_file ->
      let c = null_char in
      ib.ic_current_char <- c;
      ib.ic_current_char_is_valid <- false;
      ib.ic_eof <- true;
      c


  let peek_char ib =
    if ib.ic_current_char_is_valid
    then ib.ic_current_char
    else next_char ib


  (* Returns a valid current char for the input buffer. In particular
     no irrelevant null character (as set by [next_char] in case of end
     of input) is returned, since [End_of_file] is raised when
     [next_char] sets the end of file condition while trying to read a
     new character. *)
  let checked_peek_char ib =
    let c = peek_char ib in
    if ib.ic_eof then raise End_of_file;
    c


  let end_of_input ib =
    ignore (peek_char ib);
    ib.ic_eof


  let eof ib = ib.ic_eof

  let beginning_of_input ib = ib.ic_char_count = 0

  let name_of_input ib =
    match ib.ic_input_name with
    | From_channel _ic -> "unnamed Stdlib input channel"
    | From_file (fname, _ic) -> fname
    | From_function -> "unnamed function"
    | From_string -> "unnamed character string"


  let char_count ib =
    if ib.ic_current_char_is_valid
    then ib.ic_char_count - 1
    else ib.ic_char_count


  let line_count ib = ib.ic_line_count

  let reset_token ib = Buffer.reset ib.ic_token_buffer

  let invalidate_current_char ib = ib.ic_current_char_is_valid <- false

  let token ib =
    let token_buffer = ib.ic_token_buffer in
    let tok = Buffer.contents token_buffer in
    Buffer.clear token_buffer;
    ib.ic_token_count <- succ ib.ic_token_count;
    tok


  let token_count ib = ib.ic_token_count

  let skip_char width ib =
    invalidate_current_char ib;
    width


  let ignore_char width ib = skip_char (width - 1) ib

  let store_char width ib c =
    Buffer.add_char ib.ic_token_buffer c;
    ignore_char width ib


  let default_token_buffer_size = 1024

  let create iname next = {
    ic_eof = false;
    ic_current_char = null_char;
    ic_current_char_is_valid = false;
    ic_char_count = 0;
    ic_line_count = 0;
    ic_token_count = 0;
    ic_get_next_char = next;
    ic_token_buffer = Buffer.create default_token_buffer_size;
    ic_input_name = iname;
  }


  let from_string s =
    let i = ref 0 in
    let len = String.length s in
    let next () =
      if !i >= len then raise End_of_file else
      let c = s.[!i] in
      incr i;
      c in
    create From_string next


  let from_function = create From_function

  (* Scanning from an input channel. *)

  (* Position of the problem:

     We cannot prevent the scanning mechanism to use one lookahead character,
     if needed by the semantics of the format string specifications (e.g. a
     trailing 'skip space' specification in the format string); in this case,
     the mandatory lookahead character is indeed read from the input and not
     used to return the token read. It is thus mandatory to be able to store
     an unused lookahead character somewhere to get it as the first character
     of the next scan.

     To circumvent this problem, all the scanning functions get a low level
     input buffer argument where they store the lookahead character when
     needed; additionally, the input buffer is the only source of character of
     a scanner. The [scanbuf] input buffers are defined in module {!Scanning}.

     Now we understand that it is extremely important that related and
     successive calls to scanners indeed read from the same input buffer.
     In effect, if a scanner [scan1] is reading from [ib1] and stores an
     unused lookahead character [c1] into its input buffer [ib1], then
     another scanner [scan2] not reading from the same buffer [ib1] will miss
     the character [c1], seemingly vanished in the air from the point of view
     of [scan2].

     This mechanism works perfectly to read from strings, from files, and from
     functions, since in those cases, allocating two buffers reading from the
     same source is unnatural.

     Still, there is a difficulty in the case of scanning from an input
     channel. In effect, when scanning from an input channel [ic], this channel
     may not have been allocated from within this library. Hence, it may be
     shared (two functions of the user's program may successively read from
     [ic]). This is highly error prone since, one of the function may seek the
     input channel, while the other function has still an unused lookahead
     character in its input buffer. In conclusion, you should never mix direct
     low level reading and high level scanning from the same input channel.

  *)

  (* Perform bufferized input to improve efficiency. *)
  let file_buffer_size = ref 1024

  (* The scanner closes the input channel at end of input. *)
  let scan_close_at_end ic = Stdlib.close_in ic; raise End_of_file

  (* The scanner does not close the input channel at end of input:
     it just raises [End_of_file]. *)
  let scan_raise_at_end _ic = raise End_of_file

  let from_ic scan_close_ic iname ic =
    let len = !file_buffer_size in
    let buf = Bytes.create len in
    let i = ref 0 in
    let lim = ref 0 in
    let eof = ref false in
    let next () =
      if !i < !lim then begin let c = Bytes.get buf !i in incr i; c end else
      if !eof then raise End_of_file else begin
        lim := input ic buf 0 len;
        if !lim = 0 then begin eof := true; scan_close_ic ic end else begin
          i := 1;
          Bytes.get buf 0
        end
      end in
    create iname next


  let from_ic_close_at_end = from_ic scan_close_at_end
  let from_ic_raise_at_end = from_ic scan_raise_at_end

  (* The scanning buffer reading from [Stdlib.stdin].
     One could try to define [stdib] as a scanning buffer reading a character
     at a time (no bufferization at all), but unfortunately the top-level
     interaction would be wrong. This is due to some kind of
     'race condition' when reading from [Stdlib.stdin],
     since the interactive compiler and [Scanf.scanf] will simultaneously
     read the material they need from [Stdlib.stdin]; then, confusion
     will result from what should be read by the top-level and what should be
     read by [Scanf.scanf].
     This is even more complicated by the one character lookahead that
     [Scanf.scanf] is sometimes obliged to maintain: the lookahead character
     will be available for the next [Scanf.scanf] entry, seemingly coming from
     nowhere.
     Also no [End_of_file] is raised when reading from stdin: if not enough
     characters have been read, we simply ask to read more. *)
  let stdin =
    from_ic scan_raise_at_end
      (From_file ("-", Stdlib.stdin)) Stdlib.stdin


  let stdib = stdin

  let open_in_file open_in fname =
    match fname with
    | "-" -> stdin
    | fname ->
      let ic = open_in fname in
      from_ic_close_at_end (From_file (fname, ic)) ic


  let open_in = open_in_file Stdlib.open_in
  let open_in_bin = open_in_file Stdlib.open_in_bin

  let from_file = open_in
  let from_file_bin = open_in_bin

  let from_channel ic =
    from_ic_raise_at_end (From_channel ic) ic


  let close_in ib =
    match ib.ic_input_name with
    | From_channel ic ->
      Stdlib.close_in ic
    | From_file (_fname, ic) -> Stdlib.close_in ic
    | From_function | From_string -> ()


  (*
     Obsolete: a memo [from_channel] version to build a [Scanning.in_channel]
     scanning buffer out of a [Stdlib.in_channel].
     This function was used to try to preserve the scanning
     semantics for the (now obsolete) function [fscanf].
     Given that all scanner must read from a [Scanning.in_channel] scanning
     buffer, [fscanf] must read from one!
     More precisely, given [ic], all successive calls [fscanf ic] must read
     from the same scanning buffer.
     This obliged this library to allocated scanning buffers that were
     not properly garbage collectable, hence leading to memory leaks.
     If you need to read from a [Stdlib.in_channel] input channel
     [ic], simply define a [Scanning.in_channel] formatted input channel as in
     [let ib = Scanning.from_channel ic], then use [Scanf.bscanf ib] as usual.
  *)
  let memo_from_ic =
    let memo = ref [] in
    (fun scan_close_ic ic ->
     try List.assq ic !memo with
     | Not_found ->
       let ib =
         from_ic scan_close_ic (From_channel ic) ic in
       memo := (ic, ib) :: !memo;
       ib)


  (* Obsolete: see {!memo_from_ic} above. *)
  let memo_from_channel = memo_from_ic scan_raise_at_end

end


(* Formatted input functions. *)

type ('a, 'b, 'c, 'd) scanner =
     ('a, Scanning.in_channel, 'b, 'c, 'a -> 'd, 'd) format6 -> 'c


(* Reporting errors. *)
exception Scan_failure of string

let bad_input s = raise (Scan_failure s)

let bad_input_escape c =
  bad_input (Printf.sprintf "illegal escape character %C" c)


let bad_token_length message =
  bad_input
    (Printf.sprintf
       "scanning of %s failed: \
        the specified length was too short for token"
       message)


let bad_end_of_input message =
  bad_input
    (Printf.sprintf
       "scanning of %s failed: \
        premature end of file occurred before end of token"
       message)


let bad_float () =
  bad_input "no dot or exponent part found in float token"


let bad_hex_float () =
  bad_input "not a valid float in hexadecimal notation"


let character_mismatch_err c ci =
  Printf.sprintf "looking for %C, found %C" c ci


let character_mismatch c ci =
  bad_input (character_mismatch_err c ci)


let rec skip_whites ib =
  let c = Scanning.peek_char ib in
  if not (Scanning.eof ib) then begin
    match c with
    | ' ' | '\t' | '\n' | '\r' ->
      Scanning.invalidate_current_char ib; skip_whites ib
    | _ -> ()
  end


(* Checking that [c] is indeed in the input, then skips it.
   In this case, the character [c] has been explicitly specified in the
   format as being mandatory in the input; hence we should fail with
   [End_of_file] in case of end_of_input.
   (Remember that [Scan_failure] is raised only when (we can prove by
   evidence) that the input does not match the format string given. We must
   thus differentiate [End_of_file] as an error due to lack of input, and
   [Scan_failure] which is due to provably wrong input. I am not sure this is
   worth the burden: it is complex and somehow subliminal; should be clearer
   to fail with Scan_failure "Not enough input to complete scanning"!)

   That's why, waiting for a better solution, we use checked_peek_char here.
   We are also careful to treat "\r\n" in the input as an end of line marker:
   it always matches a '\n' specification in the input format string. *)
let rec check_char ib c =
  match c with
  | ' ' -> skip_whites ib
  | '\n' -> check_newline ib
  | c -> check_this_char ib c

and check_this_char ib c =
  let ci = Scanning.checked_peek_char ib in
  if ci = c then Scanning.invalidate_current_char ib else
  character_mismatch c ci

and check_newline ib =
  let ci = Scanning.checked_peek_char ib in
  match ci with
  | '\n' -> Scanning.invalidate_current_char ib
  | '\r' -> Scanning.invalidate_current_char ib; check_this_char ib '\n'
  | _ -> character_mismatch '\n' ci


(* Extracting tokens from the output token buffer. *)

let token_char ib = (Scanning.token ib).[0]

let token_string = Scanning.token

let token_bool ib =
  match Scanning.token ib with
  | "true" -> true
  | "false" -> false
  | s -> bad_input (Printf.sprintf "invalid boolean '%s'" s)


(* The type of integer conversions. *)
type integer_conversion =
  | B_conversion (* Unsigned binary conversion *)
  | D_conversion (* Signed decimal conversion *)
  | I_conversion (* Signed integer conversion *)
  | O_conversion (* Unsigned octal conversion *)
  | U_conversion (* Unsigned decimal conversion *)
  | X_conversion (* Unsigned hexadecimal conversion *)


let integer_conversion_of_char = function
  | 'b' -> B_conversion
  | 'd' -> D_conversion
  | 'i' -> I_conversion
  | 'o' -> O_conversion
  | 'u' -> U_conversion
  | 'x' | 'X' -> X_conversion
  | _ -> assert false


(* Extract an integer literal token.
   Since the functions Stdlib.*int*_of_string do not accept a leading +,
   we skip it if necessary. *)
let token_int_literal conv ib =
  let tok =
    match conv with
    | D_conversion | I_conversion -> Scanning.token ib
    | U_conversion -> "0u" ^ Scanning.token ib
    | O_conversion -> "0o" ^ Scanning.token ib
    | X_conversion -> "0x" ^ Scanning.token ib
    | B_conversion -> "0b" ^ Scanning.token ib in
  let l = String.length tok in
  if l = 0 || tok.[0] <> '+' then tok else String.sub tok 1 (l - 1)


(* All the functions that convert a string to a number raise the exception
   Failure when the conversion is not possible.
   This exception is then trapped in [kscanf]. *)
let token_int conv ib = int_of_string (token_int_literal conv ib)

let token_float ib = float_of_string (Scanning.token ib)

(* To scan native ints, int32 and int64 integers.
   We cannot access to conversions to/from strings for those types,
   Nativeint.of_string, Int32.of_string, and Int64.of_string,
   since those modules are not available to [Scanf].
   However, we can bind and use the corresponding primitives that are
   available in the runtime. *)
external nativeint_of_string : string -> nativeint
  = "caml_nativeint_of_string"

external int32_of_string : string -> int32
  = "caml_int32_of_string"

external int64_of_string : string -> int64
  = "caml_int64_of_string"


let token_nativeint conv ib = nativeint_of_string (token_int_literal conv ib)
let token_int32 conv ib = int32_of_string (token_int_literal conv ib)
let token_int64 conv ib = int64_of_string (token_int_literal conv ib)

(* Scanning numbers. *)

(* Digits scanning functions suppose that one character has been checked and
   is available, since they return at end of file with the currently found
   token selected.

   Put it in another way, the digits scanning functions scan for a possibly
   empty sequence of digits, (hence, a successful scanning from one of those
   functions does not imply that the token is a well-formed number: to get a
   true number, it is mandatory to check that at least one valid digit is
   available before calling one of the digit scanning functions). *)

(* The decimal case is treated especially for optimization purposes. *)
let rec scan_decimal_digit_star width ib =
  if width = 0 then width else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then width else
  match c with
  | '0' .. '9' as c ->
    let width = Scanning.store_char width ib c in
    scan_decimal_digit_star width ib
  | '_' ->
    let width = Scanning.ignore_char width ib in
    scan_decimal_digit_star width ib
  | _ -> width


let scan_decimal_digit_plus width ib =
  if width = 0 then bad_token_length "decimal digits" else
  let c = Scanning.checked_peek_char ib in
  match c with
  | '0' .. '9' ->
    let width = Scanning.store_char width ib c in
    scan_decimal_digit_star width ib
  | c ->
    bad_input (Printf.sprintf "character %C is not a decimal digit" c)


(* To scan numbers from other bases, we use a predicate argument to
   scan digits. *)
let scan_digit_star digitp width ib =
  let rec scan_digits width ib =
    if width = 0 then width else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then width else
    match c with
    | c when digitp c ->
      let width = Scanning.store_char width ib c in
      scan_digits width ib
    | '_' ->
      let width = Scanning.ignore_char width ib in
      scan_digits width ib
    | _ -> width in
  scan_digits width ib


let scan_digit_plus basis digitp width ib =
  (* Ensure we have got enough width left,
     and read at least one digit. *)
  if width = 0 then bad_token_length "digits" else
  let c = Scanning.checked_peek_char ib in
  if digitp c then
    let width = Scanning.store_char width ib c in
    scan_digit_star digitp width ib
  else
    bad_input (Printf.sprintf "character %C is not a valid %s digit" c basis)


let is_binary_digit = function
  | '0' .. '1' -> true
  | _ -> false


let scan_binary_int = scan_digit_plus "binary" is_binary_digit

let is_octal_digit = function
  | '0' .. '7' -> true
  | _ -> false


let scan_octal_int = scan_digit_plus "octal" is_octal_digit

let is_hexa_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false


let scan_hexadecimal_int = scan_digit_plus "hexadecimal" is_hexa_digit

(* Scan a decimal integer. *)
let scan_unsigned_decimal_int = scan_decimal_digit_plus

let scan_sign width ib =
  let c = Scanning.checked_peek_char ib in
  match c with
  | '+' -> Scanning.store_char width ib c
  | '-' -> Scanning.store_char width ib c
  | _ -> width


let scan_optionally_signed_decimal_int width ib =
  let width = scan_sign width ib in
  scan_unsigned_decimal_int width ib


(* Scan an unsigned integer that could be given in any (common) basis.
   If digits are prefixed by one of 0x, 0X, 0o, or 0b, the number is
   assumed to be written respectively in hexadecimal, hexadecimal,
   octal, or binary. *)
let scan_unsigned_int width ib =
  match Scanning.checked_peek_char ib with
  | '0' as c ->
    let width = Scanning.store_char width ib c in
    if width = 0 then width else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then width else
    begin match c with
    | 'x' | 'X' -> scan_hexadecimal_int (Scanning.store_char width ib c) ib
    | 'o' -> scan_octal_int (Scanning.store_char width ib c) ib
    | 'b' -> scan_binary_int (Scanning.store_char width ib c) ib
    | _ -> scan_decimal_digit_star width ib end
  | _ -> scan_unsigned_decimal_int width ib


let scan_optionally_signed_int width ib =
  let width = scan_sign width ib in
  scan_unsigned_int width ib


let scan_int_conversion conv width ib =
  match conv with
  | B_conversion -> scan_binary_int width ib
  | D_conversion -> scan_optionally_signed_decimal_int width ib
  | I_conversion -> scan_optionally_signed_int width ib
  | O_conversion -> scan_octal_int width ib
  | U_conversion -> scan_unsigned_decimal_int width ib
  | X_conversion -> scan_hexadecimal_int width ib


(* Scanning floating point numbers. *)

(* Fractional part is optional and can be reduced to 0 digits. *)
let scan_fractional_part width ib =
  if width = 0 then width else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then width else
  match c with
  | '0' .. '9' as c ->
    scan_decimal_digit_star (Scanning.store_char width ib c) ib
  | _ -> width


(* Exp part is optional and can be reduced to 0 digits. *)
let scan_exponent_part width ib =
  if width = 0 then width else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then width else
  match c with
  | 'e' | 'E' as c ->
    scan_optionally_signed_decimal_int (Scanning.store_char width ib c) ib
  | _ -> width


(* Scan the integer part of a floating point number, (not using the
   OCaml lexical convention since the integer part can be empty):
   an optional sign, followed by a possibly empty sequence of decimal
   digits (e.g. -.1). *)
let scan_integer_part width ib =
  let width = scan_sign width ib in
  scan_decimal_digit_star width ib


(*
   For the time being we have (as found in scanf.mli):
   the field width is composed of an optional integer literal
   indicating the maximal width of the token to read.
   Unfortunately, the type-checker let the user write an optional precision,
   since this is valid for printf format strings.

   Thus, the next step for Scanf is to support a full width and precision
   indication, more or less similar to the one for printf, possibly extended
   to the specification of a [max, min] range for the width of the token read
   for strings. Something like the following spec for scanf.mli:

   The optional [width] is an integer indicating the maximal
   width of the token read. For instance, [%6d] reads an integer,
   having at most 6 characters.

   The optional [precision] is a dot [.] followed by an integer:

   - in the floating point number conversions ([%f], [%e], [%g], [%F], [%E],
   and [%F] conversions, the [precision] indicates the maximum number of
   digits that may follow the decimal point. For instance, [%.4f] reads a
   [float] with at most 4 fractional digits,

   - in the string conversions ([%s], [%S], [%\[ range \]]), and in the
   integer number conversions ([%i], [%d], [%u], [%x], [%o], and their
   [int32], [int64], and [native_int] correspondent), the [precision]
   indicates the required minimum width of the token read,

   - on all other conversions, the width and precision specify the [max, min]
   range for the width of the token read.
*)
let scan_float width precision ib =
  let width = scan_integer_part width ib in
  if width = 0 then width, precision else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then width, precision else
  match c with
  | '.' ->
    let width = Scanning.store_char width ib c in
    let precision = min width precision in
    let width = width - (precision - scan_fractional_part precision ib) in
    scan_exponent_part width ib, precision
  | _ ->
    scan_exponent_part width ib, precision


let check_case_insensitive_string width ib error str =
  let lowercase c =
    match c with
    | 'A' .. 'Z' ->
      char_of_int (int_of_char c - int_of_char 'A' + int_of_char 'a')
    | _ -> c in
  let len = String.length str in
  let width = ref width in
  for i = 0 to len - 1 do
    let c = Scanning.peek_char ib in
    if lowercase c <> lowercase str.[i] then error ();
    if !width = 0 then error ();
    width := Scanning.store_char !width ib c;
  done;
  !width


let scan_hex_float width precision ib =
  if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
  let width = scan_sign width ib in
  if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
  match Scanning.peek_char ib with
  | '0' as c -> (
    let width = Scanning.store_char width ib c in
    if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
    let width = check_case_insensitive_string width ib bad_hex_float "x" in
    if width = 0 || Scanning.end_of_input ib then width else
      let width = match Scanning.peek_char ib with
        | '.' | 'p' | 'P' -> width
        | _ -> scan_hexadecimal_int width ib in
      if width = 0 || Scanning.end_of_input ib then width else
        let width = match Scanning.peek_char ib with
          | '.' as c -> (
            let width = Scanning.store_char width ib c in
            if width = 0 || Scanning.end_of_input ib then width else
              match Scanning.peek_char ib with
              | 'p' | 'P' -> width
              | _ ->
                let precision = min width precision in
                width - (precision - scan_hexadecimal_int precision ib)
          )
          | _ -> width in
        if width = 0 || Scanning.end_of_input ib then width else
          match Scanning.peek_char ib with
          | 'p' | 'P' as c ->
            let width = Scanning.store_char width ib c in
            if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
            scan_optionally_signed_decimal_int width ib
          | _ -> width
  )
  | 'n' | 'N' as c ->
    let width = Scanning.store_char width ib c in
    if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
    check_case_insensitive_string width ib bad_hex_float "an"
  | 'i' | 'I' as c ->
    let width = Scanning.store_char width ib c in
    if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
    check_case_insensitive_string width ib bad_hex_float "nfinity"
  | _ -> bad_hex_float ()


let scan_caml_float_rest width precision ib =
  if width = 0 || Scanning.end_of_input ib then bad_float ();
  let width = scan_decimal_digit_star width ib in
  if width = 0 || Scanning.end_of_input ib then bad_float ();
  let c = Scanning.peek_char ib in
  match c with
  | '.' ->
    let width = Scanning.store_char width ib c in
    (* The effective width available for scanning the fractional part is
       the minimum of declared precision and width left. *)
    let precision = min width precision in
    (* After scanning the fractional part with [precision] provisional width,
       [width_precision] is left. *)
    let width_precision = scan_fractional_part precision ib in
    (* Hence, scanning the fractional part took exactly
       [precision - width_precision] chars. *)
    let frac_width = precision - width_precision in
    (* And new provisional width is [width - width_precision. *)
    let width = width - frac_width in
    scan_exponent_part width ib
  | 'e' | 'E' ->
    scan_exponent_part width ib
  | _ -> bad_float ()


let scan_caml_float width precision ib =
  if width = 0 || Scanning.end_of_input ib then bad_float ();
  let width = scan_sign width ib in
  if width = 0 || Scanning.end_of_input ib then bad_float ();
  match Scanning.peek_char ib with
  | '0' as c -> (
    let width = Scanning.store_char width ib c in
    if width = 0 || Scanning.end_of_input ib then bad_float ();
    match Scanning.peek_char ib with
    | 'x' | 'X' as c -> (
      let width = Scanning.store_char width ib c in
      if width = 0 || Scanning.end_of_input ib then bad_float ();
      let width = scan_hexadecimal_int width ib in
      if width = 0 || Scanning.end_of_input ib then bad_float ();
      let width = match Scanning.peek_char ib with
        | '.' as c -> (
          let width = Scanning.store_char width ib c in
          if width = 0 || Scanning.end_of_input ib then width else
            match Scanning.peek_char ib with
            | 'p' | 'P' -> width
            | _ ->
              let precision = min width precision in
              width - (precision - scan_hexadecimal_int precision ib)
        )
        | 'p' | 'P' -> width
        | _ -> bad_float () in
      if width = 0 || Scanning.end_of_input ib then width else
        match Scanning.peek_char ib with
        | 'p' | 'P' as c ->
          let width = Scanning.store_char width ib c in
          if width = 0 || Scanning.end_of_input ib then bad_hex_float ();
          scan_optionally_signed_decimal_int width ib
        | _ -> width
    )
    | _ ->
      scan_caml_float_rest width precision ib
  )
  | '1' .. '9' as c ->
    let width = Scanning.store_char width ib c in
    if width = 0 || Scanning.end_of_input ib then bad_float ();
    scan_caml_float_rest width precision ib
(* Special case of nan and infinity:
  | 'i' ->
  | 'n' ->
*)
  | _ -> bad_float ()


(* Scan a regular string:
   stops when encountering a space, if no scanning indication has been given;
   otherwise, stops when encountering the characters in the scanning
   indication [stp].
   It also stops at end of file or when the maximum number of characters has
   been read. *)
let scan_string stp width ib =
  let rec loop width =
    if width = 0 then width else
    let c = Scanning.peek_char ib in
    if Scanning.eof ib then width else
      match stp with
      | Some c' when c = c' -> Scanning.skip_char width ib
      | Some _ -> loop (Scanning.store_char width ib c)
      | None ->
        match c with
        | ' ' | '\t' | '\n' | '\r' -> width
        | _ -> loop (Scanning.store_char width ib c) in
  loop width


(* Scan a char: peek strictly one character in the input, whatsoever. *)
let scan_char width ib =
  (* The case width = 0 could not happen here, since it is tested before
     calling scan_char, in the main scanning function.
    if width = 0 then bad_token_length "a character" else *)
  Scanning.store_char width ib (Scanning.checked_peek_char ib)


let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c


(* The integer value corresponding to the facial value of a valid
   decimal digit character. *)
let decimal_value_of_char c = int_of_char c - int_of_char '0'

let char_for_decimal_code c0 c1 c2 =
  let c =
    100 * decimal_value_of_char c0 +
     10 * decimal_value_of_char c1 +
          decimal_value_of_char c2 in
  if c < 0 || c > 255 then
    bad_input
      (Printf.sprintf
         "bad character decimal encoding \\%c%c%c" c0 c1 c2) else
  char_of_int c


(* The integer value corresponding to the facial value of a valid
   hexadecimal digit character. *)
let hexadecimal_value_of_char c =
  let d = int_of_char c in
  (* Could also be:
    if d <= int_of_char '9' then d - int_of_char '0' else
    if d <= int_of_char 'F' then 10 + d - int_of_char 'A' else
    if d <= int_of_char 'f' then 10 + d - int_of_char 'a' else assert false
  *)
  if d >= int_of_char 'a' then
    d - 87 (* 10 + int_of_char c - int_of_char 'a' *) else
  if d >= int_of_char 'A' then
    d - 55  (* 10 + int_of_char c - int_of_char 'A' *) else
    d - int_of_char '0'


let char_for_hexadecimal_code c1 c2 =
  let c =
    16 * hexadecimal_value_of_char c1 +
         hexadecimal_value_of_char c2 in
  if c < 0 || c > 255 then
    bad_input
      (Printf.sprintf "bad character hexadecimal encoding \\%c%c" c1 c2) else
  char_of_int c


(* Called in particular when encountering '\\' as starter of a char.
   Stops before the corresponding '\''. *)
let check_next_char message width ib =
  if width = 0 then bad_token_length message else
  let c = Scanning.peek_char ib in
  if Scanning.eof ib then bad_end_of_input message else
  c


let check_next_char_for_char = check_next_char "a Char"
let check_next_char_for_string = check_next_char "a String"

let scan_backslash_char width ib =
  match check_next_char_for_char width ib with
  | '\\' | '\'' | '\"' | 'n' | 't' | 'b' | 'r' as c ->
    Scanning.store_char width ib (char_for_backslash c)
  | '0' .. '9' as c ->
    let get_digit () =
      let c = Scanning.next_char ib in
      match c with
      | '0' .. '9' as c -> c
      | c -> bad_input_escape c in
    let c0 = c in
    let c1 = get_digit () in
    let c2 = get_digit () in
    Scanning.store_char (width - 2) ib (char_for_decimal_code c0 c1 c2)
  | 'x' ->
    let get_digit () =
      let c = Scanning.next_char ib in
      match c with
      | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' as c -> c
      | c -> bad_input_escape c in
    let c1 = get_digit () in
    let c2 = get_digit () in
    Scanning.store_char (width - 2) ib (char_for_hexadecimal_code c1 c2)
  | c ->
    bad_input_escape c


(* Scan a character (an OCaml token). *)
let scan_caml_char width ib =

  let rec find_start width =
    match Scanning.checked_peek_char ib with
    | '\'' -> find_char (Scanning.ignore_char width ib)
    | c -> character_mismatch '\'' c

  and find_char width =
    match check_next_char_for_char width ib with
    | '\\' ->
      find_stop (scan_backslash_char (Scanning.ignore_char width ib) ib)
    | c ->
      find_stop (Scanning.store_char width ib c)

  and find_stop width =
    match check_next_char_for_char width ib with
    | '\'' -> Scanning.ignore_char width ib
    | c -> character_mismatch '\'' c in

  find_start width


(* Scan a delimited string (an OCaml token). *)
let scan_caml_string width ib =

  let rec find_start width =
    match Scanning.checked_peek_char ib with
    | '\"' -> find_stop (Scanning.ignore_char width ib)
    | c -> character_mismatch '\"' c

  and find_stop width =
    match check_next_char_for_string width ib with
    | '\"' -> Scanning.ignore_char width ib
    | '\\' -> scan_backslash (Scanning.ignore_char width ib)
    | c -> find_stop (Scanning.store_char width ib c)

  and scan_backslash width =
    match check_next_char_for_string width ib with
    | '\r' -> skip_newline (Scanning.ignore_char width ib)
    | '\n' -> skip_spaces (Scanning.ignore_char width ib)
    | _ -> find_stop (scan_backslash_char width ib)

  and skip_newline width =
    match check_next_char_for_string width ib with
    | '\n' -> skip_spaces (Scanning.ignore_char width ib)
    | _ -> find_stop (Scanning.store_char width ib '\r')

  and skip_spaces width =
    match check_next_char_for_string width ib with
    | ' ' -> skip_spaces (Scanning.ignore_char width ib)
    | _ -> find_stop width in

  find_start width


(* Scan a boolean (an OCaml token). *)
let scan_bool ib =
  let c = Scanning.checked_peek_char ib in
  let m =
    match c with
    | 't' -> 4
    | 'f' -> 5
    | c ->
      bad_input
        (Printf.sprintf "the character %C cannot start a boolean" c) in
  scan_string None m ib


(* Scan a string containing elements in char_set and terminated by scan_indic
   if provided. *)
let scan_chars_in_char_set char_set scan_indic width ib =
  let rec scan_chars i stp =
    let c = Scanning.peek_char ib in
    if i > 0 && not (Scanning.eof ib) &&
       is_in_char_set char_set c &&
       int_of_char c <> stp then
      let _ = Scanning.store_char max_int ib c in
      scan_chars (i - 1) stp in
  match scan_indic with
  | None -> scan_chars width (-1);
  | Some c ->
    scan_chars width (int_of_char c);
    if not (Scanning.eof ib) then
      let ci = Scanning.peek_char ib in
      if c = ci
      then Scanning.invalidate_current_char ib
      else character_mismatch c ci


(* The global error report function for [Scanf]. *)
let scanf_bad_input ib = function
  | Scan_failure s | Failure s ->
    let i = Scanning.char_count ib in
    bad_input (Printf.sprintf "scanf: bad input at char number %i: %s" i s)
  | x -> raise x


(* Get the content of a counter from an input buffer. *)
let get_counter ib counter =
  match counter with
  | Line_counter -> Scanning.line_count ib
  | Char_counter -> Scanning.char_count ib
  | Token_counter -> Scanning.token_count ib


(* Compute the width of a padding option (see "%42{" and "%123("). *)
let width_of_pad_opt pad_opt = match pad_opt with
  | None -> max_int
  | Some width -> width


let stopper_of_formatting_lit fmting =
  if fmting = Escaped_percent then '%', "" else
    let str = string_of_formatting_lit fmting in
    let stp = str.[1] in
    let sub_str = String.sub str 2 (String.length str - 2) in
    stp, sub_str


(******************************************************************************)
                           (* Reader management *)

(* A call to take_format_readers on a format is evaluated into functions
   taking readers as arguments and aggregate them into an heterogeneous list *)
(* When all readers are taken, finally pass the list of the readers to the
   continuation k. *)
let rec take_format_readers : type a c d e f .
    ((d, e) heter_list -> e) -> (a, Scanning.in_channel, c, d, e, f) fmt ->
    d =
fun k fmt -> match fmt with
  | Reader fmt_rest ->
    fun reader ->
      let new_k readers_rest = k (Cons (reader, readers_rest)) in
      take_format_readers new_k fmt_rest
  | Char rest                        -> take_format_readers k rest
  | Caml_char rest                   -> take_format_readers k rest
  | String (_, rest)                 -> take_format_readers k rest
  | Caml_string (_, rest)            -> take_format_readers k rest
  | Int (_, _, _, rest)              -> take_format_readers k rest
  | Int32 (_, _, _, rest)            -> take_format_readers k rest
  | Nativeint (_, _, _, rest)        -> take_format_readers k rest
  | Int64 (_, _, _, rest)            -> take_format_readers k rest
  | Float (_, _, _, rest)            -> take_format_readers k rest
  | Bool (_, rest)                   -> take_format_readers k rest
  | Alpha rest                       -> take_format_readers k rest
  | Theta rest                       -> take_format_readers k rest
  | Flush rest                       -> take_format_readers k rest
  | String_literal (_, rest)         -> take_format_readers k rest
  | Char_literal (_, rest)           -> take_format_readers k rest
  | Custom (_, _, rest)              -> take_format_readers k rest

  | Scan_char_set (_, _, rest)       -> take_format_readers k rest
  | Scan_get_counter (_, rest)       -> take_format_readers k rest
  | Scan_next_char rest              -> take_format_readers k rest

  | Formatting_lit (_, rest)         -> take_format_readers k rest
  | Formatting_gen (Open_tag (Format (fmt, _)), rest) ->
      take_format_readers k (concat_fmt fmt rest)
  | Formatting_gen (Open_box (Format (fmt, _)), rest) ->
      take_format_readers k (concat_fmt fmt rest)

  | Format_arg (_, _, rest)          -> take_format_readers k rest
  | Format_subst (_, fmtty, rest)    ->
     take_fmtty_format_readers k (erase_rel (symm fmtty)) rest
  | Ignored_param (ign, rest)        -> take_ignored_format_readers k ign rest

  | End_of_format                    -> k Nil

(* Take readers associated to an fmtty coming from a Format_subst "%(...%)". *)
and take_fmtty_format_readers : type x y a c d e f .
    ((d, e) heter_list -> e) -> (a, Scanning.in_channel, c, d, x, y) fmtty ->
      (y, Scanning.in_channel, c, x, e, f) fmt -> d =
fun k fmtty fmt -> match fmtty with
  | Reader_ty fmt_rest ->
    fun reader ->
      let new_k readers_rest = k (Cons (reader, readers_rest)) in
      take_fmtty_format_readers new_k fmt_rest fmt
  | Ignored_reader_ty fmt_rest ->
    fun reader ->
      let new_k readers_rest = k (Cons (reader, readers_rest)) in
      take_fmtty_format_readers new_k fmt_rest fmt
  | Char_ty rest                -> take_fmtty_format_readers k rest fmt
  | String_ty rest              -> take_fmtty_format_readers k rest fmt
  | Int_ty rest                 -> take_fmtty_format_readers k rest fmt
  | Int32_ty rest               -> take_fmtty_format_readers k rest fmt
  | Nativeint_ty rest           -> take_fmtty_format_readers k rest fmt
  | Int64_ty rest               -> take_fmtty_format_readers k rest fmt
  | Float_ty rest               -> take_fmtty_format_readers k rest fmt
  | Bool_ty rest                -> take_fmtty_format_readers k rest fmt
  | Alpha_ty rest               -> take_fmtty_format_readers k rest fmt
  | Theta_ty rest               -> take_fmtty_format_readers k rest fmt
  | Any_ty rest                 -> take_fmtty_format_readers k rest fmt
  | Format_arg_ty (_, rest)     -> take_fmtty_format_readers k rest fmt
  | End_of_fmtty                -> take_format_readers k fmt
  | Format_subst_ty (ty1, ty2, rest) ->
    let ty = trans (symm ty1) ty2 in
    take_fmtty_format_readers k (concat_fmtty ty rest) fmt

(* Take readers associated to an ignored parameter. *)
and take_ignored_format_readers : type x y a c d e f .
    ((d, e) heter_list -> e) -> (a, Scanning.in_channel, c, d, x, y) ignored ->
      (y, Scanning.in_channel, c, x, e, f) fmt -> d =
fun k ign fmt -> match ign with
  | Ignored_reader ->
    fun reader ->
      let new_k readers_rest = k (Cons (reader, readers_rest)) in
      take_format_readers new_k fmt
  | Ignored_char                    -> take_format_readers k fmt
  | Ignored_caml_char               -> take_format_readers k fmt
  | Ignored_string _                -> take_format_readers k fmt
  | Ignored_caml_string _           -> take_format_readers k fmt
  | Ignored_int (_, _)              -> take_format_readers k fmt
  | Ignored_int32 (_, _)            -> take_format_readers k fmt
  | Ignored_nativeint (_, _)        -> take_format_readers k fmt
  | Ignored_int64 (_, _)            -> take_format_readers k fmt
  | Ignored_float (_, _)            -> take_format_readers k fmt
  | Ignored_bool _                  -> take_format_readers k fmt
  | Ignored_format_arg _            -> take_format_readers k fmt
  | Ignored_format_subst (_, fmtty) -> take_fmtty_format_readers k fmtty fmt
  | Ignored_scan_char_set _         -> take_format_readers k fmt
  | Ignored_scan_get_counter _      -> take_format_readers k fmt
  | Ignored_scan_next_char          -> take_format_readers k fmt

(******************************************************************************)
                          (* Generic scanning *)

(* Make a generic scanning function. *)
(* Scan a stream according to a format and readers obtained by
   take_format_readers, and aggregate scanned values into an
   heterogeneous list. *)
(* Return the heterogeneous list of scanned values. *)
let rec make_scanf : type a c d e f.
    Scanning.in_channel -> (a, Scanning.in_channel, c, d, e, f) fmt ->
      (d, e) heter_list -> (a, f) heter_list =
fun ib fmt readers -> match fmt with
  | Char rest ->
    let _ = scan_char 0 ib in
    let c = token_char ib in
    Cons (c, make_scanf ib rest readers)
  | Caml_char rest ->
    let _ = scan_caml_char 0 ib in
    let c = token_char ib in
    Cons (c, make_scanf ib rest readers)

  | String (pad, Formatting_lit (fmting_lit, rest)) ->
    let stp, str = stopper_of_formatting_lit fmting_lit in
    let scan width _ ib = scan_string (Some stp) width ib in
    let str_rest = String_literal (str, rest) in
    pad_prec_scanf ib str_rest readers pad No_precision scan token_string
  | String (pad, Formatting_gen (Open_tag (Format (fmt', _)), rest)) ->
    let scan width _ ib = scan_string (Some '{') width ib in
    pad_prec_scanf ib (concat_fmt fmt' rest) readers pad No_precision scan
                   token_string
  | String (pad, Formatting_gen (Open_box (Format (fmt', _)), rest)) ->
    let scan width _ ib = scan_string (Some '[') width ib in
    pad_prec_scanf ib (concat_fmt fmt' rest) readers pad No_precision scan
                   token_string
  | String (pad, rest) ->
    let scan width _ ib = scan_string None width ib in
    pad_prec_scanf ib rest readers pad No_precision scan token_string

  | Caml_string (pad, rest) ->
    let scan width _ ib = scan_caml_string width ib in
    pad_prec_scanf ib rest readers pad No_precision scan token_string
  | Int (iconv, pad, prec, rest) ->
    let c = integer_conversion_of_char (char_of_iconv iconv) in
    let scan width _ ib = scan_int_conversion c width ib in
    pad_prec_scanf ib rest readers pad prec scan (token_int c)
  | Int32 (iconv, pad, prec, rest) ->
    let c = integer_conversion_of_char (char_of_iconv iconv) in
    let scan width _ ib = scan_int_conversion c width ib in
    pad_prec_scanf ib rest readers pad prec scan (token_int32 c)
  | Nativeint (iconv, pad, prec, rest) ->
    let c = integer_conversion_of_char (char_of_iconv iconv) in
    let scan width _ ib = scan_int_conversion c width ib in
    pad_prec_scanf ib rest readers pad prec scan (token_nativeint c)
  | Int64 (iconv, pad, prec, rest) ->
    let c = integer_conversion_of_char (char_of_iconv iconv) in
    let scan width _ ib = scan_int_conversion c width ib in
    pad_prec_scanf ib rest readers pad prec scan (token_int64 c)
  | Float ((_, (Float_F | Float_CF)), pad, prec, rest) ->
    pad_prec_scanf ib rest readers pad prec scan_caml_float token_float
  | Float ((_, (Float_f | Float_e | Float_E | Float_g | Float_G)),
           pad, prec, rest) ->
    pad_prec_scanf ib rest readers pad prec scan_float token_float
  | Float ((_, (Float_h | Float_H)), pad, prec, rest) ->
    pad_prec_scanf ib rest readers pad prec scan_hex_float token_float
  | Bool (pad, rest) ->
    let scan _ _ ib = scan_bool ib in
    pad_prec_scanf ib rest readers pad No_precision scan token_bool
  | Alpha _ ->
    invalid_arg "scanf: bad conversion \"%a\""
  | Theta _ ->
    invalid_arg "scanf: bad conversion \"%t\""
  | Custom _ ->
    invalid_arg "scanf: bad conversion \"%?\" (custom converter)"
  | Reader fmt_rest ->
    begin match readers with
    | Cons (reader, readers_rest) ->
        let x = reader ib in
        Cons (x, make_scanf ib fmt_rest readers_rest)
    | Nil ->
        invalid_arg "scanf: missing reader"
    end
  | Flush rest ->
    if Scanning.end_of_input ib then make_scanf ib rest readers
    else bad_input "end of input not found"

  | String_literal (str, rest) ->
    String.iter (check_char ib) str;
    make_scanf ib rest readers
  | Char_literal (chr, rest) ->
    check_char ib chr;
    make_scanf ib rest readers

  | Format_arg (pad_opt, fmtty, rest) ->
    let _ = scan_caml_string (width_of_pad_opt pad_opt) ib in
    let s = token_string ib in
    let fmt =
      try format_of_string_fmtty s fmtty
      with Failure msg -> bad_input msg
    in
    Cons (fmt, make_scanf ib rest readers)
  | Format_subst (pad_opt, fmtty, rest) ->
    let _ = scan_caml_string (width_of_pad_opt pad_opt) ib in
    let s = token_string ib in
    let fmt, fmt' =
      try
        let Fmt_EBB fmt = fmt_ebb_of_string s in
        let Fmt_EBB fmt' = fmt_ebb_of_string s in
        (* TODO: find a way to avoid reparsing twice *)

        (* TODO: these type-checks below *can* fail because of type
           ambiguity in presence of ignored-readers: "%_r%d" and "%d%_r"
           are typed in the same way.

           # Scanf.sscanf "\"%_r%d\"3" "%(%d%_r%)" ignore
             (fun fmt n -> string_of_format fmt, n)
           Exception: CamlinternalFormat.Type_mismatch.

           We should properly catch this exception.
        *)
        type_format fmt (erase_rel fmtty),
        type_format fmt' (erase_rel (symm fmtty))
      with Failure msg -> bad_input msg
    in
    Cons (Format (fmt, s),
          make_scanf ib (concat_fmt fmt' rest) readers)

  | Scan_char_set (width_opt, char_set, Formatting_lit (fmting_lit, rest)) ->
    let stp, str = stopper_of_formatting_lit fmting_lit in
    let width = width_of_pad_opt width_opt in
    scan_chars_in_char_set char_set (Some stp) width ib;
    let s = token_string ib in
    let str_rest = String_literal (str, rest) in
    Cons (s, make_scanf ib str_rest readers)
  | Scan_char_set (width_opt, char_set, rest) ->
    let width = width_of_pad_opt width_opt in
    scan_chars_in_char_set char_set None width ib;
    let s = token_string ib in
    Cons (s, make_scanf ib rest readers)
  | Scan_get_counter (counter, rest) ->
    let count = get_counter ib counter in
    Cons (count, make_scanf ib rest readers)
  | Scan_next_char rest ->
    let c = Scanning.checked_peek_char ib in
    Cons (c, make_scanf ib rest readers)

  | Formatting_lit (formatting_lit, rest) ->
    String.iter (check_char ib) (string_of_formatting_lit formatting_lit);
    make_scanf ib rest readers
  | Formatting_gen (Open_tag (Format (fmt', _)), rest) ->
    check_char ib '@'; check_char ib '{';
    make_scanf ib (concat_fmt fmt' rest) readers
  | Formatting_gen (Open_box (Format (fmt', _)), rest) ->
    check_char ib '@'; check_char ib '[';
    make_scanf ib (concat_fmt fmt' rest) readers

  | Ignored_param (ign, rest) ->
    let Param_format_EBB fmt' = param_format_of_ignored_format ign rest in
    begin match make_scanf ib fmt' readers with
    | Cons (_, arg_rest) -> arg_rest
    | Nil -> assert false
    end

  | End_of_format ->
    Nil

(* Case analysis on padding and precision. *)
(* Reject formats containing "%*" or "%.*". *)
(* Pass padding and precision to the generic scanner `scan'. *)
and pad_prec_scanf : type a c d e f x y z t .
    Scanning.in_channel -> (a, Scanning.in_channel, c, d, e, f) fmt ->
      (d, e) heter_list -> (x, y) padding -> (y, z -> a) precision ->
      (int -> int -> Scanning.in_channel -> t) ->
      (Scanning.in_channel -> z) ->
      (x, f) heter_list =
fun ib fmt readers pad prec scan token -> match pad, prec with
  | No_padding, No_precision ->
    let _ = scan max_int max_int ib in
    let x = token ib in
    Cons (x, make_scanf ib fmt readers)
  | No_padding, Lit_precision p ->
    let _ = scan max_int p ib in
    let x = token ib in
    Cons (x, make_scanf ib fmt readers)
  | Lit_padding ((Right | Zeros), w), No_precision ->
    let _ = scan w max_int ib in
    let x = token ib in
    Cons (x, make_scanf ib fmt readers)
  | Lit_padding ((Right | Zeros), w), Lit_precision p ->
    let _ = scan w p ib in
    let x = token ib in
    Cons (x, make_scanf ib fmt readers)
  | Lit_padding (Left, _), _ ->
    invalid_arg "scanf: bad conversion \"%-\""
  | Lit_padding ((Right | Zeros), _), Arg_precision ->
    invalid_arg "scanf: bad conversion \"%*\""
  | Arg_padding _, _ ->
    invalid_arg "scanf: bad conversion \"%*\""
  | No_padding, Arg_precision ->
    invalid_arg "scanf: bad conversion \"%*\""

(******************************************************************************)
            (* Defining [scanf] and various flavors of [scanf] *)

type 'a kscanf_result = Args of 'a | Exc of exn

let kscanf ib ef (Format (fmt, str)) =
  let rec apply : type a b . a -> (a, b) heter_list -> b =
    fun f args -> match args with
    | Cons (x, r) -> apply (f x) r
    | Nil -> f
  in
  let k readers f =
    Scanning.reset_token ib;
    match try Args (make_scanf ib fmt readers) with
      | (Scan_failure _ | Failure _ | End_of_file) as exc -> Exc exc
      | Invalid_argument msg ->
        invalid_arg (msg ^ " in format \"" ^ String.escaped str ^ "\"")
    with
      | Args args -> apply f args
      | Exc exc -> ef ib exc
  in
  take_format_readers k fmt

(***)

let kbscanf = kscanf
let bscanf ib fmt = kbscanf ib scanf_bad_input fmt

let ksscanf s ef fmt = kbscanf (Scanning.from_string s) ef fmt
let sscanf s fmt = kbscanf (Scanning.from_string s) scanf_bad_input fmt

let scanf fmt = kscanf Scanning.stdib scanf_bad_input fmt

(***)

(* Scanning format strings. *)
let bscanf_format :
  Scanning.in_channel -> ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g =
  fun ib format f ->
    let _ = scan_caml_string max_int ib in
    let str = token_string ib in
    let fmt' =
      try format_of_string_format str format
      with Failure msg -> bad_input msg in
    f fmt'


let sscanf_format :
  string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g =
  fun s format f -> bscanf_format (Scanning.from_string s) format f


let format_from_string s fmt =
  sscanf_format ("\"" ^ String.escaped s ^ "\"") fmt (fun x -> x)


let unescaped s =
  sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)


(* Deprecated *)
let kfscanf ic ef fmt = kbscanf (Scanning.memo_from_channel ic) ef fmt
let fscanf ic fmt = kscanf (Scanning.memo_from_channel ic) scanf_bad_input fmt
