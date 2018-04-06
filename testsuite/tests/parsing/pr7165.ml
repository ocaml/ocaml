(* TEST
   flags = "-dparsetree"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* this is a lexer directive with an out-of-bound integer;
   it should result in a lexing error instead of an
   uncaught exception as in PR#7165 *)
#9342101923012312312 ""
