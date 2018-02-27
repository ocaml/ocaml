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

class toplevel :
  name : (string -> string) ->
  flags : string ->
  directory : string ->
  exit_status_variable : Variables.t ->
  reference_variable : Variables.t ->
  output_variable : Variables.t ->
  backend : Ocaml_backends.t ->
  is_toplevel : bool ->
  is_native : bool ->
object inherit Ocaml_tools.tool
  method backend : Ocaml_backends.t
  method is_toplevel : bool
  method is_native : bool
end

val ocaml : toplevel

val ocamlnat : toplevel
