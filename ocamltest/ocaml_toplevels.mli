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

(* Descriptions of the OCaml toplevels *)

module type Toplevel = sig
  include Ocaml_tools.Tool
  val backend : Ocaml_backends.t
  val compiler : Ocaml_compilers.compiler
end

type toplevel = (module Toplevel)

val toplevel :
  name : string ->
  flags : string ->
  directory : string ->
  exit_status_variable : Variables.t ->
  reference_variable : Variables.t ->
  output_variable : Variables.t ->
  backend : Ocaml_backends.t ->
  compiler : Ocaml_compilers.compiler ->
  toplevel

val ocaml : toplevel

val ocamlnat : toplevel
