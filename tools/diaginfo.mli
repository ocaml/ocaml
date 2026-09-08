(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(** [ocamldiaginfo] provides a way to print metadata information about all
    diagnostics printed by the compiler and REPL

  - [ocamldiaginfo -history] prints the full history of diagnostics across all
  versions
  - [ocamldiaginfo -schema <name>] prints the schema of a diagnostic, by default
  in an annotated ADT format or as a json schema with the [-schema-format json]
  flag
  - [ocamldiaginfo -list] prints all known schema
*)
