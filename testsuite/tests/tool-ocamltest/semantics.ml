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

{
  readonly_files="negation.test negation.reference";
  program="negation.test";
  output="negation.result";
  flags="-e";
  ocamltest;
  reference="negation.reference";
  check-program-output;
}
*)
