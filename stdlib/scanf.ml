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

(* $Id $ *)

(** Formatted input functions. *)

let bad_input ib s =
  let i = Scanning.char_count ib in
  invalid_arg
    (Printf.sprintf
      "scanf: bad input at char number %i, while scanning %s" i s);;

let bad_format fmt i fc =
  invalid_arg
    (Printf.sprintf
       "scanf: bad format %c, at char number %i of format %s" fc i fmt);;

let token_int ib =
 let s = Scanning.token ib in
 try Pervasives.int_of_string s
 with Failure "int_of_string" -> bad_input ib s;;

let token_bool ib =
  match Scanning.token ib with
  | "true" -> true
  | "false" -> false
  | s -> bad_input ib ("a boolean, found " ^ s);;

let token_char ib =
  (Scanning.token ib).[0];;

let token_float ib =
  let s = Scanning.token ib in
  float_of_string s;;

(* Scanning numbers. *)

let scan_sign max ib =
  let c = Scanning.peek_char ib in
  match c with
  | '+' -> Scanning.store_char ib c max
  | '-' -> Scanning.store_char ib c max
  | c -> max;;

(* Decimal case is optimized. *)
let rec scan_decimal_digits max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  match Scanning.peek_char ib with
  | '0' .. '9' as c ->
      let max = Scanning.store_char ib c max in
      scan_decimal_digits max ib
  | c -> max;;

(* Other cases uses a predicate argument to scan_digits. *)
let rec scan_digits digitp max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  match Scanning.peek_char ib with
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
  | '0' .. '8' -> true
  | _ -> false in
  scan_digits is_octal;;

let scan_hexadecimal_digits =
  let is_hexa = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false in
  scan_digits is_hexa;;

let scan_Hexadecimal_digits =
  let is_Hexa = function
  | '0' .. '9' | 'A' .. 'F' -> true
  | _ -> false in
  scan_digits is_Hexa;;

(* Decimal integers. *)
let scan_unsigned_decimal_int max ib =
  if max = 0 || Scanning.end_of_input ib then bad_input ib "an int" else
  scan_decimal_digits max ib;;

let scan_optionally_signed_decimal_int max ib =
  let max = scan_sign max ib in
  scan_unsigned_decimal_int max ib;;

(* Scan an unsigned integer that could be given in any (common) basis.
   If digits are prefixed by 0b for one of x, X, o, b the number is
   assumed to be written respectively in hexadecimal, hexadecimal,
   octal, or binary. *)
let scan_unsigned_int max ib =
  match Scanning.peek_char ib with
  | '0' as c ->
      let max = Scanning.store_char ib c max in
      if max = 0 || Scanning.end_of_input ib then max else
      let c = Scanning.peek_char ib in
      begin match c with
      | 'x' -> scan_hexadecimal_digits (Scanning.store_char ib c max) ib
      | 'X' -> scan_Hexadecimal_digits (Scanning.store_char ib c max) ib
      | 'o' -> scan_octal_digits (Scanning.store_char ib c max) ib
      | 'b' -> scan_binary_digits (Scanning.store_char ib c max) ib
      | c -> scan_decimal_digits max ib end
  | c -> scan_decimal_digits max ib;;

let scan_optionally_signed_int max ib =
  let max = scan_sign max ib in
  if max = 0 || Scanning.end_of_input ib then bad_input ib "an int" else
  scan_unsigned_int max ib;;

let read_optionally_signed_decimal_int max ib =
  let max = scan_optionally_signed_decimal_int max ib in
  token_int ib;;

let read_unsigned_decimal_int max ib =
  let max = scan_unsigned_decimal_int max ib in
  token_int ib;;

let read_optionally_signed_int max ib =
  let max = scan_optionally_signed_int max ib in
  token_int ib;;

let read_unsigned_octal_int max ib =
 let max = scan_octal_digits max ib in
 token_int ib;;

let read_unsigned_hexadecimal_int max ib =
  let max = scan_hexadecimal_digits max ib in
  token_int ib;;

let read_unsigned_Hexadecimal_int max ib =
 let max = scan_Hexadecimal_digits max ib in
 token_int ib;;

(* Scanning floating point numbers. *)
let scan_frac_part max ib = scan_unsigned_decimal_int max ib;;

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

let read_float max ib =
  let max = scan_float max ib in
  token_float ib;;

let rec scan_string max ib =
  if max = 0 || Scanning.end_of_input ib then max else
  match Scanning.peek_char ib with
  | ' ' | '\t' | '\n' | '\r' -> max
  | c -> scan_string (Scanning.store_char ib c max) ib;;

let read_string max ib =
  let max = scan_string max ib in
  Scanning.token ib;;

let scan_char max ib =
  if max = 0 || Scanning.end_of_input ib then bad_input ib "a char" else
  let c = Scanning.peek_char ib in
  Scanning.store_char ib c max;;

let read_char max ib =
  let max = scan_char max ib in
  token_char ib;;

let scan_bool max ib =
  let m =
    match Scanning.peek_char ib with
    | 't' -> 4
    | 'f' -> 5
    | _ -> 0 in
  scan_string (min max m) ib;;

let read_bool max ib =
  let max = scan_bool max ib in
  token_bool ib;;

type char_set = Pos_set of string | Neg_set of string;;

let read_char_set fmt i =
  let lim = String.length fmt in

  let rec find_in_set i j =
    if j >= lim then bad_format fmt j fmt.[lim - 1] else
    match fmt.[j] with
    | ']' -> String.sub fmt i (j - i), succ j
    | c -> find_in_set i (j + 1)

  and find_set_sign i =
    if i >= lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | '^' -> let set, j = find_set (i + 1) in j, Neg_set set
    | _ -> let set, j = find_set i in j, Pos_set set

  and find_set i =
    if i >= lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | ']' -> find_in_set i (i + 1)
    | c -> find_in_set i i in

  find_set_sign i;;

let make_setp char_set =
  let make_predv set =
    let v = Array.make 256 false in
    let lim = String.length set - 1 in
    let rec loop b i =
      if i <= lim then
      match set.[i] with
      | '-' when b ->
         (* if i = 0 then b is false (the initial call is loop false 0)
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
      (fun c -> v.(int_of_char c))
  | Neg_set set ->
      let v = make_predv set in
      (fun c -> not (v.(int_of_char c)));;

let scan_chars_in_char_set char_set max ib =
  let setp = make_setp char_set in
  let rec loop max ib =
    if max = 0 || Scanning.end_of_input ib then max else
    let c = Scanning.peek_char ib in
    if setp c then loop (Scanning.store_char ib c max) ib else max in
  loop max ib
;;

let read_chars_in_char_set char_set max ib =
  let max = scan_chars_in_char_set char_set max ib in
  Scanning.token ib;;
   
(* To scan native ints, int32 and int64 integers.
Nativeint.of_string
Int32.of_string
Int64.of_string
 *)
let scan_int_long t c max ib =
  failwith "Not yet impmented";;

let rec skip_whites ib =
  if not (Scanning.end_of_input ib) then
  match Scanning.peek_char ib with
  | ' ' | '\r' | '\t' | '\n' -> Scanning.next_char ib; skip_whites ib
  | _ -> ();;

(* Main scanning function:
   it takes an input buffer, a format and a function.
   Then it scans the format and the buffer in parallel to find out
   values as specified by the format. When it founds some it applies it
   to function f in turn and continue. *) 
let scanf_fun ib (fmt : ('a, 'b, 'c) format) f =
  let fmt = (Obj.magic fmt : string) in
  let lim = String.length fmt in

  let rec scan f i =
    if i >= lim then Obj.magic f else
    match fmt.[i] with
    | '%' -> scan_width f (i + 1)
    | ' ' | '\r' | '\t' | '\n' -> skip_whites ib; scan f (i + 1)
    | fc when Scanning.end_of_input ib -> bad_format fmt i fc
    | fc when Scanning.peek_char ib = fc ->
        Scanning.next_char ib; scan f (i + 1)
    | fc -> bad_format fmt i fc

  and scan_width f i =
    if i >= lim then bad_format fmt i '%' else
    match fmt.[i] with
    | '0' .. '9' as c ->
         let rec read_width accu i =
           if i >= lim then accu, i else
           match fmt.[i] with
           | '0' .. '9' as c ->
               let accu = 10 * accu + (int_of_char c - int_of_char '0') in
               read_width accu (i + 1)
           | _ -> accu, i in
         let max, j = read_width 0 i in
         scan_conversion max f j
    | _ -> scan_conversion max_int f i

  and scan_conversion max f i =
    if i >= lim then bad_format fmt i fmt.[lim - 1] else
    match fmt.[i] with
    | 'c' -> let x = read_char max ib in scan (Obj.magic f x) (i + 1)
    | c ->
       skip_whites ib;
       match c with
       | '%' as fc when Scanning.peek_char ib = fc ->
           Scanning.next_char ib; scan f (i + 1)
       | '%' as fc -> bad_format fmt i fc
       | 'd' ->
           let x = read_optionally_signed_decimal_int max ib in
           scan (Obj.magic f x) (i + 1)
       | 'i' ->
           let x = read_optionally_signed_int max ib in
           scan (Obj.magic f x) (i + 1)
       | 'o' ->
           let x = read_unsigned_octal_int max ib in
           scan (Obj.magic f x) (i + 1)
       | 'u' ->
           let x = read_unsigned_decimal_int max ib in
           scan (Obj.magic f x) (i + 1)
       | 'x' ->
           let x = read_unsigned_hexadecimal_int max ib in
           scan (Obj.magic f x) (i + 1)
       | 'X' ->
           let x = read_unsigned_Hexadecimal_int max ib in
           scan (Obj.magic f x) (i + 1)
       | 'f' | 'g' | 'G' | 'e' | 'E' ->
           let x = read_float max ib in scan (Obj.magic f x) (i + 1)
       | 's' -> let x = read_string max ib in scan (Obj.magic f x) (i + 1)
       | 'b' ->
           let x = read_bool 4 ib in scan (Obj.magic f x) (i + 1)
       | '[' ->
           let j, char_set = read_char_set fmt (i + 1) in
           let x = read_chars_in_char_set char_set max ib in
           scan (Obj.magic f x) (j + 1)
       | 'l' | 'n' | 'L' as t ->
           let i = i + 1 in
           if i >= lim then bad_format fmt i t else begin
           match fmt.[i] with
           | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' as c ->
              let x = scan_int_long t c max ib in
              scan (Obj.magic f x) (i + 1)
           | _ -> bad_format fmt i end
       | 'N' ->
           let x = Scanning.char_count ib in
           scan (Obj.magic f x) (i + 1)
       | c -> bad_format fmt i c in
  Scanning.reset_token ib;
  scan f 0;;

let bscanf ib =
 (Obj.magic scanf_fun :
  Scanning.scanbuf ->
  ('a, Scanning.scanbuf, 'b) format ->
  ('a -> 'b)) ib;;

let fscanf ic fmt =
  let ib = Scanning.from_channel ic in
  bscanf ib fmt;;

let scanf fmt = fscanf stdin fmt;;

let sscanf s fmt =
  let ib = Scanning.from_string s in
  bscanf ib fmt;;
