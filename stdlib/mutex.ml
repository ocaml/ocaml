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

let[@inline] protect m f =
  lock m;
  match f() with
  | x ->
    unlock m; x
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    unlock m;
    Printexc.raise_with_backtrace e bt
