(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt             *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t
external create: unit -> t = "caml_ml_mutex_new"
external lock: t -> unit = "caml_ml_mutex_lock"
external try_lock: t -> bool = "caml_ml_mutex_try_lock"
external unlock: t -> unit = "caml_ml_mutex_unlock"

(* private re-export *)
external reraise : exn -> 'a = "%reraise"

(* cannot inline, otherwise flambda might move code around. *)
let[@inline never] protect m f =
  lock m;
  match f() with
  | x ->
    unlock m; x
  | exception e ->
    (* NOTE: [unlock] does not poll for asynchronous exceptions *)
    unlock m;
    reraise e
