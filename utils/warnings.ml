(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Please keep them in alphabetical order *)

type t =                             (* A is all *)
  | Comment of string                (* C *)
  | Deprecated                       (* D *)
  | Fragile_pat of string            (* E *)
  | Partial_application              (* F *)
  | Labels_omitted                   (* L *)
  | Method_override of string list   (* M *)
  | Partial_match of string          (* P *)
  | Statement_type                   (* S *)
  | Unused_match                     (* U *)
  | Unused_pat                       (* U *)
  | Hide_instance_variable of string (* V *)
  | Other of string                  (* X *)
;;

let letter = function        (* 'a' is all *)
  | Comment _ ->                'c'
  | Deprecated ->               'd'
  | Fragile_pat _ ->            'e'
  | Partial_application ->      'f'
  | Labels_omitted ->           'l'
  | Method_override _ ->        'm'
  | Partial_match _ ->          'p'
  | Statement_type ->           's'
  | Unused_match|Unused_pat ->  'u'
  | Hide_instance_variable _ -> 'v'
  | Other _ ->                  'x'
;;

let active = Array.create 27 true;;
let error = Array.create 27 false;;

let translate c =
  if c >= 'A' && c <= 'Z' then
    (Char.code c - Char.code 'A', true)
  else if c >= 'a' && c <= 'z' then
    (Char.code c - Char.code 'a', false)
  else
    (26, false)
;;

let is_active x =
  let (n, _) = translate (letter x) in
  active.(n)
;;

let is_error x =
  let (n, _) = translate (letter x) in
  error.(n)
;;

let parse_options iserr s =
  let flags = if iserr then error else active in
  for i = 0 to String.length s - 1 do
    if s.[i] = 'A' then Array.fill flags 0 (Array.length flags) true
    else if s.[i] = 'a' then Array.fill flags 0 (Array.length flags) false
    else begin
      let (n, fl) = translate s.[i] in
      flags.(n) <- fl;
    end;
  done
;;

let () = parse_options false "el";;

let message = function
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a value that is not matched:\n" ^ s
  | Unused_match -> "this match case is unused."
  | Unused_pat   -> "this pattern is unused."
  | Fragile_pat "" ->
      "this pattern is fragile. It would hide\n\
       the addition of new constructors to the data types it matches."
  | Fragile_pat s ->
      "this pattern is fragile. It would hide\n\
       the addition of new constructors to the data types it matches.\n\
       Here is an example of a more robust pattern:\n" ^ s
  | Labels_omitted ->
      "labels were omitted in the application of this function."
  | Method_override slist ->
      String.concat " "
        ("the following methods are overriden \
          by the inherited class:\n " :: slist)
  | Hide_instance_variable lab ->
      "this definition of an instance variable " ^ lab ^
      " hides a previously\ndefined instance variable of the same name."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Statement_type ->
      "this expression should have type unit."
  | Comment s -> "this is " ^ s ^ "."
  | Deprecated -> "this syntax is deprecated."
  | Other s -> s
;;

let nerrors = ref 0;;

let print ppf w =
  let msg = message w in
  let newlines = ref 0 in
  for i = 0 to String.length msg - 1 do
    if msg.[i] = '\n' then incr newlines;
  done;
  let (out, flush, newline, space) =
    Format.pp_get_all_formatter_output_functions ppf ()
  in
  let countnewline x = incr newlines; newline x in
  Format.pp_set_all_formatter_output_functions ppf out flush countnewline space;
  Format.fprintf ppf "%s" msg;
  Format.pp_print_flush ppf ();
  Format.pp_set_all_formatter_output_functions ppf out flush newline space;
  let (n, _) = translate (letter w) in
  if error.(n) then incr nerrors;
  !newlines
;;

exception Errors of int;;

let check_fatal () =
  if !nerrors > 0 then begin
    let e = Errors !nerrors in
    nerrors := 0;
    raise e;
  end;
;;
