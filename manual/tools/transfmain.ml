let main() =
  let lexbuf = lexing__create_lexer_channel std_in in
  if vect_length sys__command_line >= 2 & sys__command_line.(1) = "-html"
  then htmltransf__main lexbuf
  else transf__main lexbuf;
  exit 0;;

printexc__f main ();;
