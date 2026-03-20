(* TEST
  ocamllex_flags = " -q -w -missing-case ";
*)
(*
   Check that disabling a warning works
*)

rule missing_case = parse
| _  { () }
