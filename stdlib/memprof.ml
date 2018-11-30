(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Jacques-Henri Joudan, projet Gallium, INRIA Paris          *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type alloc_kind =
  | Minor
  | Major
  | Major_postponed
  | Serialized

type sample_info = {
    n_samples: int; kind: alloc_kind; tag: int;
    size: int; callstack: Printexc.raw_backtrace;
}

type 'a callback = sample_info -> (Obj.t, 'a) Ephemeron.K1.t option

type 'a ctrl = {
    sampling_rate : float;
    callstack_size : int;
    callback : 'a callback
}

let stopped_ctrl = {
    sampling_rate = 0.; callstack_size = 0;
    callback = fun _ -> assert false
}

external set_ctrl : 'a ctrl -> unit = "caml_memprof_set"

let start = set_ctrl
let stop () = set_ctrl stopped_ctrl
