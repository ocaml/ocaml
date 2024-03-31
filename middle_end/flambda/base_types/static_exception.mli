(**************************************************************************)
(*        ^o3                                                             *)
(* ~/\_/\_|)                       OCaml                                  *)
(* |/=_=\|                                                                *)
(* "     "                                                                *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** An identifier that is used to label static exceptions.  Its
    uniqueness properties are unspecified. *)

include Identifiable.S

val create : unit -> t

val to_int : t -> int
