(* TEST
  {
    setup-ocamlc.byte-build-env;
    all_modules = "test_stdlib_todo_flag_error.ml";
    flags = "-alert @todo";
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    check-ocaml-output;
  }
*)


Fun.todo ();;
