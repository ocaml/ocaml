let main() =
  let lexbuf = Lexing.from_channel stdin in
  Transf.main lexbuf;
  exit 0;;

Printexc.print main ();;
