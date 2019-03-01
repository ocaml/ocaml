(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
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

type 'a located = {
  node : 'a;
  loc : Location.t
}

type environment_statement =
  | Assignment of bool * string located * string located (* variable = value *)
  | Append of string located * string located (* variable += value *)
  | Include of string located (* include named environment *)

type tsl_item =
  | Environment_statement of environment_statement located
  | Test of
    int (* test depth *) *
    string located (* test name *) *
    string located list (* environment modifiers *)

type tsl_block = tsl_item list

val make_identifier : ?loc:Location.t -> string -> string located
val make_string : ?loc:Location.t -> string -> string located
val make_environment_statement :
  ?loc:Location.t -> environment_statement -> environment_statement located
