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
    Set a end of file condition if no character can be read. *)

val peek_char : scanbuf -> char;;
(** [Scanning.peek_char scanbuf] returns the current char available in
    the input. *)

val cautious_peek_char : scanbuf -> char;;
(** [Scanning.cautious_peek_char scanbuf] returns the current char available in
    the input or tries to read one if none has ever been read. *)

val checked_peek_char : scanbuf -> char;;
(** Same as above but always returns a valid char instead of a null 
    char when the reading method of the input buffer has reached end of
    file. *)

val store_char : scanbuf -> char -> int -> int;;
(** [Scanning.store_char scanbuf c lim] adds [c] to the token buffer
    of the scanning buffer. It also advances the scanning buffer for one
    character and returns [lim - 1], indicating that there
    is one less character to read. *)

val char_count : scanbuf -> int;;
(** [Scanning.char_count scanbuf] returns the number of characters read
    from the given buffer. *)

val token : scanbuf -> string;;
(** [Scanning.token scanbuf] returns the string stored into the token
    buffer of the scanning buffer: it returns the token matched by the
    format. *)

val reset_token : scanbuf -> unit;;
(** [Scanning.reset_token scanbuf] resets the token buffer of
    the given scanning buffer. *)

val token_count : scanbuf -> int;;
(** [Scanning.token_count scanbuf] returns the number of tokens read
    so far from [scanbuf]. *)

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
type scanbuf = {
  mutable eof : bool;
  mutable bof : bool;
  mutable cur_char : char;
  mutable char_count : int;
  mutable token_count : int;
  mutable get_next_char : unit -> char;
  tokbuf : Buffer.t;
};;

(* Reads a new character from input buffer, sets the end of file
   condition if necessary. *)
let next_char ib =
  try
   ib.cur_char <- ib.get_next_char ();
   ib.char_count <- ib.char_count + 1
  with End_of_file ->
   ib.cur_char <- '\000';
   ib.eof <- true;;

let cautious_peek_char ib =
  if ib.bof then begin next_char ib; ib.bof <- false end;
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
let end_of_input ib = ib.eof;;
let beginning_of_input ib = ib.bof;;
let char_count ib = ib.char_count;;
let reset_token ib = Buffer.reset ib.tokbuf;;

let token ib =
  let tokbuf = ib.tokbuf in
  let tok = Buffer.contents tokbuf in
  Buffer.clear tokbuf;
  ib.token_count <- 1 + ib.token_count;
  tok;;

let token_count ib = ib.token_count;;

let store_char ib c max =
  Buffer.add_char ib.tokbuf c;
  next_char ib;
  max - 1;;

let default_token_buffer_size = 1024;;

let create next =
  let ib = {
    bof = true;
    eof = false;
    cur_char = '\000';
    char_count = 0;
    get_next_char = next;
    tokbuf = Buffer.create default_token_buffer_size;
    token_count = 0;
    } in
  ib;;

let from_string s =
  let i = ref 0 in
  let len = String.length s in
  let next () =
    if !i >= len then raise End_of_file else
    let c = s.[!i] in
    incr i;
    c in
  create next;;

let from_function = create;;

(* Perform bufferized input to improve efficiency. *)
let file_buffer_size = ref 1024;;

let from_file_channel ic =
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
  create next;;

let from_file fname = from_file_channel (open_in fname);;
let from_file_bin fname = from_file_channel (open_in_bin fname);;

let from_channel ic =
  let next () = input_char ic in
  create next;;

let stdib = from_channel stdin;;
(** The scanning buffer reading from [stdin].*)

end;;

(** Formatted input functions. *)

(* Reporting errors. *)
exception Scan_failure of string;;

let bad_input s = raise (Scan_failure s);;
let bad_input_char c = bad_input (String.make 1 c);;

let bad_input_escape c =
  bad_input (Printf.sprintf "illegal escape character %c" c);;

let scanf_bad_input ib = function
  | Scan_failure s | Failure s ->
      let i = Scanning.char_count ib in
      bad_input (Printf.sprintf "scanf: bad input at char number %i: %s" i s)
  | x -> raise x;;

let bad_format fmt i fc =
  invalid_arg
    (Printf.sprintf
       "scanf: bad format %c, at char number %i of format %s" fc i fmt);;

(* Checking that the current char is indeed one of range, then skip it. *)
let check_char_in ib range =
  let ci = Scanning.checked_peek_char ib in
  if List.mem ci range then Scanning.next_char ib else
  bad_input (Printf.sprintf "looking for one of %s, found %c" "a range" ci);;

(* Checking that [c] is indeed in the input, then skip it. *)
let check_char ib c =
  let ci = Scanning.checked_peek_char ib in
  if ci = c then Scanning.next_char ib else
  bad_input (Printf.sprintf "looking for %c, found %c" c ci);;

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
let signed_int_of_string s =
  let tok =
    let l = String.length s in
    if l = 0 || s.[0] <> '+' then s else String.sub s 1 (l - 1) in
  int_of_string tok;;

let token_int conv ib = int_of_string (token_int_literal conv ib);;
let token_float ib = float_of_string (Scanning.token ib);;

(* To scan native ints, int32 and int64 integers.
   We cannot access to conversions to/from strings for those types,
   Nativeint.of_string, Int32.of_string, and Int64.of_string,
   since those modules are not available to scanf.
   However, we can bind and use the corresponding primitives that are
   available in the runtime. *)
external nativeint_of_string: string -> nativeint = "nativeint_of_string";;
external int32_of_string : string -> int32 = "int32_of_string";;
external int64_of_string : string -> int64 = "int64_of_string";;

let token_nativeint conv ib = nativeint_of_string (token_int_literal conv ib);;
let token_int32 conv ib = int32_of_string (token_int_literal conv ib);;
let token_int64 conv ib = int64_of_string (token_int_literal conv ib);;

(* Scanning numbers. *)

(* The decimal case is optimized. *)
let rec scan_decimal_digits max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  match Scanning.checked_peek_char ib with
  | '0' .. '9' as c ->
      let max = Scanning.store_char ib c max in
      scan_decimal_digits max ib
  | c -> max;;

(* Other cases uses a predicate argument to scan_digits. *)
let rec scan_digits digitp max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  match Scanning.checked_peek_char ib with
  | c when digitp c ->
     let max = Scanning.store_char ib c max in
     scan_digits digitp max ib
  | _ -> max;;

let scan_binary_digits =
  let is_binary = function
  | '0' .. '1' -> true
  | _ -> false in
  scan_digits is_binary;;

let scan_octal_digits =
  let is_octal = function
  | '0' .. '7' -> true
  | _ -> false in
  scan_digits is_octal;;

let scan_hexadecimal_digits =
  let is_hexa = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false in
  scan_digits is_hexa;;

(* Decimal integers. *)
let scan_unsigned_decimal_int max ib =
  if max = 0 || Scanning.end_of_input ib then bad_input "decimal digit" else
  scan_decimal_digits max ib;;

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
      if max = 0 || Scanning.end_of_input ib then max else
      let c = Scanning.peek_char ib in
      begin match c with
      | 'x' | 'X' -> scan_hexadecimal_digits (Scanning.store_char ib c max) ib
      | 'o' -> scan_octal_digits (Scanning.store_char ib c max) ib
      | 'b' -> scan_binary_digits (Scanning.store_char ib c max) ib
      | c -> scan_decimal_digits max ib end
  | c -> scan_decimal_digits max ib;;

let scan_optionally_signed_int max ib =
  let max = scan_sign max ib in
  if max = 0 || Scanning.end_of_input ib then bad_input "bad int" else
  scan_unsigned_int max ib;;

let scan_int conv max ib =
  match conv with
  | 'b' -> scan_binary_digits max ib
  | 'd' -> scan_optionally_signed_decimal_int max ib
  | 'i' -> scan_optionally_signed_int max ib
  | 'o' -> scan_octal_digits max ib
  | 'u' -> scan_unsigned_decimal_int max ib
  | 'x' | 'X' -> scan_hexadecimal_digits max ib
  | c -> assert false;;

(* Scanning floating point numbers. *)
(* Fractional part is optional and can be reduced to 0 digits. *)
let scan_frac_part max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  scan_unsigned_decimal_int max ib;;

(* Exp part is optional and can be reduced to 0 digits. *)
let scan_exp_part max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  let c = Scanning.peek_char ib in
  match c with
  | 'e' | 'E' as c ->
     scan_optionally_signed_int (Scanning.store_char ib c max) ib
  | _ -> max;;

let scan_float max ib =
  let max = scan_optionally_signed_decimal_int max ib in
  if max = 0 || Scanning.end_of_input ib then max else
  let c = Scanning.peek_char ib in
  match c with
  | '.' ->
     let max = Scanning.store_char ib c max in
     let max = scan_frac_part max ib in
     scan_exp_part max ib
  | c -> scan_exp_part max ib;;

(* Scan a regular string: it stops with a space or one of the
   characters in stp. It also stops when the maximum number of
   characters has been read.*)
let scan_string stp max ib =
  let rec loop max =
    if max = 0 || Scanning.end_of_input ib then max else
    let c = Scanning.checked_peek_char ib in
    if stp = [] then
      match c with
      | ' ' | '\t' | '\n' | '\r' -> max
      | c -> loop (Scanning.store_char ib c max) else
    if List.mem c stp then max else
    loop (Scanning.store_char ib c max) in
  let max = loop max in
  if stp <> [] then check_char_in ib stp;
  max;;

(* Scan a char: peek strictly one character in the input, whatsoever. *)
let scan_char max ib =
  Scanning.store_char ib (Scanning.checked_peek_char ib) max;;

let char_for_backslash =
  match Sys.os_type with
  | "Unix" | "Win32" | "Cygwin" ->
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | "MacOS" ->
      begin function
      | 'n' -> '\013'
      | 'r' -> '\010'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | x -> assert false;;

let char_for_decimal_code c0 c1 c2 =
  let c =
    100 * (int_of_char c0 - 48) +
     10 * (int_of_char c1 - 48) +
          (int_of_char c2 - 48) in
  if c < 0 || c > 255
  then bad_input (Printf.sprintf "bad char \\%c%c%c" c0 c1 c2)
  else char_of_int c;;

(* Called when encountering '\\' as starter of a char.
   Stops before the corresponding '\''. *)
let scan_backslash_char max ib =
  if max = 0 || Scanning.end_of_input ib then bad_input "a char" else
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
   if max = 0 || Scanning.end_of_input ib then bad_input "a char" else
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
    if max = 0 || Scanning.end_of_input ib then bad_input "a string" else
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
    if max = 0 || Scanning.end_of_input ib then bad_input "a string" else
    let c = Scanning.checked_peek_char ib in
    match c, s with
    | '\n', true
    | ' ', false ->
       Scanning.next_char ib; skip_spaces false (max - 1)
    | '\\', false -> loop false max
(*    | '\\', true*)
    | c, false -> loop false (Scanning.store_char ib c max)
(*    | ' ', _ -> bad_input_char c*)
    | _, _ -> loop false (scan_backslash_char (max - 1) ib) in
  loop true max;;

let scan_bool max ib =
  if max < 4 || Scanning.end_of_input ib then bad_input "a boolean" else
  let m =
    match Scanning.checked_peek_char ib with
    | 't' -> 4
    | 'f' -> 5
    | _ -> 0 in
  scan_string [] (min max m) ib;;

type char_set =
   | Pos_set of string
   | Neg_set of string;;

let read_char_set fmt i =
  let lim = String.length fmt - 1 in

  let rec find_in_set i j =
    if j > lim then bad_format fmt j fmt.[lim - 1] else
    match fmt.[j] with
    | ']' -> String.sub fmt i (j - i), j
    | c -> find_in_set i (j + 1)

  and find_set_sign i =
    if i > lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | '^' -> let set, i = find_set (i + 1) in i, Neg_set set
    | _ -> let set, i = find_set i in i, Pos_set set

  and find_set i =
    if i > lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | ']' -> find_in_set i (i + 1)
    | c -> find_in_set i i in

  find_set_sign i;;

let make_setp stp char_set =
  let make_predv set =
    let v = Array.make 256 false in
    let lim = String.length set - 1 in
    let rec loop b i =
      if i <= lim then
      match set.[i] with
      | '-' when b ->
         (* if i = 0 then b is false (since the initial call is loop false 0)
            hence i >= 1 and the following is safe. *)
         let c1 = set.[i - 1] in
         let i = i + 1 in
         if i > lim then loop false (i - 1) else
         let c2 = set.[i] in
         for j = int_of_char c1 to int_of_char c2 do v.(j) <- true done;
         loop false (i + 1)
      | c -> v.(int_of_char set.[i]) <- true; loop true (i + 1) in
    loop false 0;
    v in
  match char_set with
  | Pos_set set ->
      let v = make_predv set in
      List.iter (fun c -> v.(int_of_char c) <- false) stp;
      (fun c -> v.(int_of_char c))
  | Neg_set set ->
      let v = make_predv set in
      List.iter (fun c -> v.(int_of_char c) <- true) stp;
      (fun c -> not (v.(int_of_char c)));;

let scan_chars_in_char_set stp char_set max ib =
  let setp = make_setp stp char_set in
  let rec loop max =
    let c = Scanning.cautious_peek_char ib in
    if max = 0 || Scanning.end_of_input ib then max else
    if setp c then loop (Scanning.store_char ib c max) else
    max in
  let max = loop max in
  if stp <> [] then check_char_in ib stp;
  max;;

let skip_whites ib =
  let rec loop = function
  | ' ' | '\t' | '\n' | '\r' ->
     Scanning.next_char ib;
     if not (Scanning.end_of_input ib) then loop (Scanning.peek_char ib)
  | _ -> () in
  if not (Scanning.end_of_input ib) then
  loop (Scanning.cautious_peek_char ib);;

(* Main scanning function:
   it takes an input buffer, a format and a function.
   Then it scans the format and the buffer in parallel to find out
   tokens as specified by the format. When it founds one token, it converts
   it as specified, remembers the converted value as a future
   argument to the function [f], and continues scanning.

   If the entire scanning succeeds (i.e. the format string has been
   exhausted and the buffer has provided tokens according to the
   format string), the tokens are applied to [f].

   If the scanning or some conversion fails, the scanning function
   aborts and applies the scanning buffer and a string that explains
   the error to the error continuation [ef]. *)
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
    | '%' -> scan_fmt_width f (i + 1)
    | '@' as t ->
        let i = i + 1 in
        if i > lim then bad_format fmt (i - 1) t else check_input fmt.[i] f i
    | c -> check_input c f i

  and check_input c f i =
    check_char ib c;
    scan_fmt f (i + 1)

  and scan_fmt_width f i =
    if i > lim then bad_format fmt i '%' else
    match fmt.[i] with
    | '_' -> scan_fmt_fixed_width true f (i + 1)
    | _ -> scan_fmt_fixed_width false f i

  and scan_fmt_fixed_width skip f i =
    if i > lim then bad_format fmt i '%' else
    match fmt.[i] with
    | '0' .. '9' as c ->
        let rec read_width accu i =
          if i > lim then accu, i else
          match fmt.[i] with
          | '0' .. '9' as c ->
              let accu = 10 * accu + (int_of_char c - int_of_char '0') in
              read_width accu (i + 1)
          | _ -> accu, i in
        let max, j = read_width 0 i in
        scan_fmt_conversion skip max f j
    | _ -> scan_fmt_conversion skip max_int f i

  and scan_fmt_conversion skip max f i =
    let stack = if skip then no_stack else stack in
    if i > lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | '%' as c ->
        check_input c f i
    | 'c' when max = 0 ->
        let c = Scanning.checked_peek_char ib in
        scan_fmt (stack f c) (i + 1)
    | 'c' | 'C' as conv ->
        let x =
          if conv = 'c' then scan_char max ib else scan_Char max ib in        
        scan_fmt (stack f (token_char ib)) (i + 1)
    | 'b' | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' as conv ->
        let x = scan_int conv max ib in
        scan_fmt (stack f (token_int conv ib)) (i + 1)
    | 'f' | 'g' | 'G' | 'e' | 'E' | 'F' ->
        let x = scan_float max ib in
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
    | 'B' ->
        let x = scan_bool max ib in
        scan_fmt (stack f (token_bool ib)) (i + 1)
    | 'l' | 'n' | 'L' as t ->
        let i = i + 1 in
        if i > lim then
          let x = Scanning.char_count ib in
          scan_fmt (stack f x) i else begin
        match fmt.[i] with
        | 'b' | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' as conv ->
            let x = scan_int conv max ib in
            begin match t with
            | 'l' -> scan_fmt (stack f (token_int32 conv ib)) (i + 1)
            | 'L' -> scan_fmt (stack f (token_int64 conv ib)) (i + 1)
            | _ -> scan_fmt (stack f (token_nativeint conv ib)) (i + 1) end
        | c ->
            let x = Scanning.char_count ib in
            scan_fmt (stack f x) i end
    | 'N' ->
        let x = Scanning.token_count ib in
        scan_fmt (stack f x) (i + 1)
    | 'r' ->
        Obj.magic (fun reader arg ->
          let x = reader ib arg in
          scan_fmt (stack f x) (succ i))
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
