(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Abstract Syntax Tree for the Tests Specification Language *)

type statement =
  | Assignment of string * string (* variable = value *)
  | Include of string (* import environemnt *)

type environment_description = statement list

type test_kind =
  | Declared_test
  | New_test of string list (* declare a new test and use it *)

type test_spec = {
  test_name : string;
  test_kind: test_kind;
  test_environment: environment_description;
}

type program = {
  root_environment : environment_description;
  tests: test_spec list
}
