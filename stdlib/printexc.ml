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

let prerr_loc file first_char last_char msg =
  prerr_string "File \"";
  prerr_string file;
  prerr_string "\", line 0, characters "; prerr_int first_char;
  prerr_char '-'; prerr_int last_char; prerr_string ": ";
  prerr_string msg; prerr_char '\n'

let print_exn = function
    Out_of_memory ->
      prerr_string "Out of memory\n"
  | Match_failure(file, first_char, last_char) ->
      prerr_loc file first_char last_char "Pattern matching failed";
  | Assert_failure(file, first_char, last_char) ->
      prerr_loc file first_char last_char "Assertion failed";
  | x ->
      prerr_string "Uncaught exception: ";
      prerr_string (Obj.magic(Obj.field (Obj.field (Obj.repr x) 0) 0));
      if Obj.size (Obj.repr x) > 1 then begin
        prerr_char '(';
        for i = 1 to Obj.size (Obj.repr x) - 1 do
          if i > 1 then prerr_string ", ";
          let arg = Obj.field (Obj.repr x) i in
          if not (Obj.is_block arg) then
            prerr_int (Obj.magic arg : int)
          else if Obj.tag arg = 252 then begin
            prerr_char '"';
            prerr_string (Obj.magic arg : string);
            prerr_char '"'
          end else
            prerr_char '_'
        done;
        prerr_char ')'
      end;
      prerr_char '\n'

let print fct arg =
  try
    fct arg
  with x ->
    print_exn x;
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    print_exn x;
    exit 2
