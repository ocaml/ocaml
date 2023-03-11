(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Cambium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** An OCamldoc generator to retrieve information in "todo" tags and
   generate an html page with all todo items. *)

module Naming = Odoc_html.Naming
module Html : Odoc_html.Html_generator

module Generator :
  sig
    class scanner :
      < html_of_text : ?with_p:bool -> Buffer.t -> Odoc_info.text -> unit;
        .. > ->
      object
        inherit Odoc_scan.scanner
        method buffer : Buffer.t
      end
    class html : Html.html
  end
