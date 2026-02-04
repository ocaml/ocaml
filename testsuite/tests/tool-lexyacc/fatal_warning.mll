(* TEST
  ocamllex_flags = " -q -w @missing-case ";
  ocamllex_exit_status = "3";
*)
(*
   Check that making a warning fatal works
*)

rule missing_case = parse
| _  { () }
