(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*        Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Input_handling
open Primitives
module Lexer = Debugger_lexer

(* Ask user a yes or no question. *)
let yes_or_no message =
  if !interactif then
    let finally =
      let old_prompt = !current_prompt in
      fun () -> stop_user_input (); current_prompt := old_prompt
    in
      Fun.protect ~finally @@ fun () ->
        current_prompt := message ^ " ? (y or n) ";
        let answer =
          let rec ask () =
            resume_user_input ();
            let line =
              string_trim (Lexer.line (Lexing.from_function read_user_input))
            in
              match (if String.length line > 0 then line.[0] else ' ') with
                'y' -> true
              | 'n' -> false
              | _ ->
                stop_user_input ();
                print_string "Please answer y or n.";
                print_newline ();
                ask ()
          in
            ask ()
        in
          answer
  else
    false
