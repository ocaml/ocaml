(* TEST
(* This tests the -H flag.

   The basic structure is that libc depends on libb, which depends on liba.  We
   want to test a few things:

   - Compiling libc with -I liba allows the compiler to see the type definitions
     in liba and allows c.ml to reference it directly.

   - Compiling libc with -H liba allows the compiler to see the type definitions
     in liba, but doesn't allow c.ml to reference it directly.

   - If -H and -I are are passed for two different versions of liba, the -I one
     takes priority.

   - If -H is passed twice with two different versions of liba, the first takes
     priority.

   The liba_alt directory has an alternate versions of liba used for testing the
   precedence order of the includes.
*)

subdirectories = "liba liba_alt libb libc";
setup-ocamlc.byte-build-env;

flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlc.byte;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlc.byte;

flags = "-I liba -I libb -nocwd";
module = "libb/b.ml";
ocamlc.byte;
{
  (* Test hiding A completely *)
  flags = "-I libb -nocwd";
  module = "libc/c2.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Test hiding A completely, but using it *)
  flags = "-I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/not_included.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  (* Test transitive use of A's cmi with -I. *)
  flags = "-I liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Test transitive use of A's cmi with -H. *)
  flags = "-H liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Test direct use of A cmi with -H. *)
  flags = "-H liba -I libb -nocwd";
  module = "libc/c3.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/cant_reference_hidden.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* The next four tests check that -I takes priority over -H regardless of the
   order on the command line.
*)
{
  flags = "-H liba_alt -I liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  flags = "-I liba -H liba_alt -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  not-windows;
  flags = "-H liba -I liba_alt -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  not-windows;
  flags = "-I liba_alt -H liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* The next two tests show that earlier -Hs take priority over later -Hs *)
{
  not-windows;
  flags = "-H liba_alt -H liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  flags = "-H liba -H liba_alt -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that a hidden `A` doesn't become visible as a result of the typechecker
   using it. *)
{
  flags = "-H liba -I libb -nocwd";
  module = "libc/c4.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/hidden_stays_hidden.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Test that type-directed constructor disambiguation works through -H (at
   least, for now). *)
{
  flags = "-H liba -I libb -nocwd";
  module = "libc/c5.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

*)
