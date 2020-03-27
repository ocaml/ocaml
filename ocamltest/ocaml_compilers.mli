(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Descriptions of the OCaml compilers and toplevels *)

type t = {
  name : string -> string;
  flags : string;
  directory : string;
  backend : Ocaml_backends.t;
  exit_status_variabe : Variables.t;
  reference_variable : Variables.t;
  output_variable : Variables.t
}

val ocamlc_byte : t

val ocamlc_opt : t

val ocamlopt_byte : t

val ocamlopt_opt : t

val ocaml : t

val ocamlnat : t

val expected_exit_status : Environments.t -> t -> int

val reference_filename : Environments.t -> string -> t -> string
