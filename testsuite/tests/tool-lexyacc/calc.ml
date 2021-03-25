(* TEST
   modules = "calc_parser.mly calc_lexer.mll"
   ocamllex_flags = " -q "
   ocamlyacc_flags = " -q "
   readonly_files = "calc_input.txt"
   stdin = "calc_input.txt"
*)
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Calc_parser.main Calc_lexer.token lexbuf in
        print_int result; print_newline(); flush stdout
    done
  with Calc_lexer.Eof ->
    exit 0
