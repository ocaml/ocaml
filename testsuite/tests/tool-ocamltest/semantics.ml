(* TEST
{
  readonly_files="negation.test negation.reference";
  program="negation.test";
  output="negation.result";
  flags="-e";
  ocamltest;
  reference="negation.reference";
  check-program-output;
}

{
  readonly_files="and.test and.reference";
  program="and.test";
  output="and.result";
  flags="-e";
  ocamltest;
  reference="and.reference";
  check-program-output;
}

{
  readonly_files="or.test or.reference";
  program="or.test";
  output="or.result";
  flags="-e";
  ocamltest;
  reference="or.reference";
  check-program-output;
}

{
  readonly_files="if.test if.reference";
  program="if.test";
  output="if.result";
  flags="-e";
  ocamltest;
  reference="if.reference";
  check-program-output;
}
*)
