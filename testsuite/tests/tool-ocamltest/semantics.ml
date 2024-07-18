(* TEST
{
  readonly_files="conditions.test conditions.reference";
  program="conditions.test";
  output="conditions.result";
  flags="-e";
  ocamltest;
  reference="conditions.reference";
  check-program-output;
}
*)
