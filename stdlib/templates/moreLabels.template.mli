(**************************************************************************)
(*        ^o3                                                             *)
(* ~/\_/\_|)                       OCaml                                  *)
(* |/=_=\|                                                                *)
(* "     "                                                                *)
(*                Jacques Garrigue, Kyoto University RIMS                 *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* NOTE: Do not edit this file directly. Edit templates/ and run
 tools/sync_stdlib_docs *)

(** Extra labeled libraries.

   This meta-module provides labelized versions of the {!Hashtbl}, {!Map} and
   {!Set} modules.

   This module is intended to be used through [open MoreLabels] which replaces
   {!Hashtbl}, {!Map}, and {!Set} with their labeled counterparts.

   For example:
   {[
     open MoreLabels

     Hashtbl.iter ~f:(fun ~key ~data -> g key data) table
   ]}
*)

module Hashtbl : sig
HASHTBL
end

module Map : sig
MAP
end

module Set : sig
SET
end
