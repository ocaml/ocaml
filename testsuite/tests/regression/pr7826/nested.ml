(* TEST
   native-compiler;
   no-flambda;
   timeout = "60";
   readonly_files = "gen.ml check.ml";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/gen";
   all_modules = "gen.ml";
   ocamlopt.byte;
   program = "${test_build_directory}/check";
   all_modules = "check.ml";
   ocamlopt.byte;
   program = "${test_build_directory}/nested";
   all_modules = "nested.ml";
   flags = "-pp ${test_build_directory}/gen -dprofile";
   ocamlopt.byte;
   script = "${test_build_directory}/check ${test_build_directory_prefix}/ocamlopt.byte/ocamlopt.byte.output";
   script;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   reference = "${test_source_directory}/nested.reference";
   run;
   check-program-output;
*)
(* This file is empty on purpose: [gen.ml] is used as the preprocessor (-pp),
   and it prints the program compiled below.

   gen.ml prints a chain of 8000 nested local functions, each enclosing the
   next, with the innermost closing over the outer parameter.  Compiling that
   is linear in the nesting depth once the closure pass stops recomputing every
   group's free variables at each enclosing level, and quadratic before that:
   the generate phase allocates about 1 GB here, against about 200 GB without
   the fix, so check.ml asserts on that allocation (see #7826).

   The timeout is a hang guard, not the oracle -- the allocation is the
   assertion, because it does not depend on how fast the machine is.

   Gated no-flambda (flambda takes a different closure path, with its own
   quadratic in Lift_constants) and to the native compiler: the bytecode path
   computes the same thing per level and is a separate site, still quadratic.
*)
