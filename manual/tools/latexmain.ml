let main () =
  latexscan__main (lexing__create_lexer_channel stdin);;

printexc__f main (); exit 0;;
