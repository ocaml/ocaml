(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf;;

let locfmt =
  match Sys.os_type with
  | "MacOS" -> ("File \"%s\"; line %d; characters %d to %d ### %s"
                : ('a, 'b, 'c) format)
  | _ -> ("File \"%s\", line %d, characters %d-%d: %s" : ('a, 'b, 'c) format)
;;

let field x i =
  let f = Obj.field x i in
  if not (Obj.is_block f) then
    sprintf "%d" (Obj.magic f : int)           (* can also be a char *)
  else if Obj.tag f = 252 then
    sprintf "\"%s\"" (String.escaped (Obj.magic f : string))
  else if Obj.tag f = 253 then
    string_of_float (Obj.magic f : float)
  else
    "_"
;;
let rec other_fields x i =
  if i >= Obj.size x then ""
  else sprintf ", %s%s" (field x i) (other_fields x (i+1))
;;
let fields x =
  match Obj.size x with
  | 0 -> ""
  | 1 -> ""
  | 2 -> sprintf "(%s)" (field x 1)
  | n -> sprintf "(%s%s)" (field x 1) (other_fields x 2)
;;

let to_string = function
  | Out_of_memory -> "Out of memory";
  | Stack_overflow -> "Stack overflow";
  | Match_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Pattern matching failed";
  | Assert_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Assertion failed";
  | x ->
      let x = Obj.repr x in
      let constructor = (Obj.magic(Obj.field (Obj.field x 0) 0) : string) in
      constructor ^ (fields x)
;;

let print fct arg =
  try
    fct arg
  with x ->
    eprintf "Uncaught exception: %s\n" (to_string x);
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    eprintf "Uncaught exception: %s\n" (to_string x);
    exit 2
