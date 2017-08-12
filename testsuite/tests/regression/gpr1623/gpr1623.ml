(* TEST
  arguments = "???"
  *)

(* On Windows the runtime expand windows wildcards (asterisks and
 * question marks).
 *
 * This file is a non-regression test for github's PR#1623.
 *
 * On Windows 64bits, a segfault was triggered when one argument consists
 * only of wildcards.
 *
 * The source code of this test is empty: we just check the arguments
 * expansion.
 * *)
