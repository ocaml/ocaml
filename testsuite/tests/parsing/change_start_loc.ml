(* TEST
flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/toplevel"
include ocamlcommon
*)
let position = Lexing.{ (* This corresponds to File "file.ml", line 100, character 10 *)
    pos_fname = "------should not appear------";
    pos_lnum = 100;
    pos_bol = 1000;
    pos_cnum = 1010;
}

(* We need to show, that just changing lex_curr_p is not enough.
   See wrong columns in output for 'Incomplete version'. *)
let set_position_incomplete lexbuf position =
  let open Lexing in
  lexbuf.lex_curr_p  <- {position with pos_fname = lexbuf.lex_curr_p.pos_fname}

(* "Testing framework" *)
let print_error_in_parse set_position_variant =
    try
        let _ =
            let lexbuf = Lexing.from_string ")f x" in (* contains error in chars 0-1, line 0 *)
            set_position_variant lexbuf position;
            Lexing.set_filename lexbuf "file.ml"; (* also testing set_filename *)
            Parse.expression lexbuf in ()
    with e -> Location.report_exception Format.std_formatter e

let _ =
    print_string "Incomplete version:\n";
    print_error_in_parse set_position_incomplete;
    print_string "Good version:\n";
    print_error_in_parse Lexing.set_position
