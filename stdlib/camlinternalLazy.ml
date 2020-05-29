(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Internals of forcing lazy values. *)

type 'a t = 'a lazy_t

exception Undefined

(* [update_tag blk old new] updates the tag [blk] from [old] to [new] using a
 * CAS loop (in order to handle concurrent conflicts with the GC marking).
 * Returns [true] if the update is successful. Return [false] if the tag of
 * [blk] is not [old]. *)

external update_tag : Obj.t -> int -> int -> bool = "caml_obj_update_tag"

external make_forward : Obj.t -> Obj.t -> unit = "caml_obj_make_forward"

external domain_self : unit -> int = "caml_ml_domain_id"

(* Assumes [blk] is a block with tag forcing *)
let do_force_block blk =
  let b = Obj.repr blk in
  let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
  Obj.set_field b 0 (Obj.repr (domain_self ()));
  try
    let result = closure () in
    make_forward b (Obj.repr result);
    result
  with e ->
    Obj.set_field b 0 (Obj.repr (fun () -> raise e));
    assert (update_tag b Obj.forcing_tag Obj.lazy_tag);
    raise e

(* Assumes [blk] is a block with tag forcing *)
let do_force_val_block blk =
  let b = Obj.repr blk in
  let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
  Obj.set_field b 0 (Obj.repr (domain_self ()));
  let result = closure () in
  make_forward b (Obj.repr result);
  result

type status = Racy | Forcing

(* Assumes [blk] is a block with tag lazy *)
let update_tag_forcing (blk : 'arg lazy_t) =
  let b = Obj.repr blk in
  if not (update_tag b Obj.lazy_tag Obj.forcing_tag) then
    (* blk has tag either
        + Obj.forcing_tag -- currently being forced by this domain or
                             another concurrent domain (or)
        + Obj.forward_tag -- was being forced by another domain which has since
                             completed the evaluation and updated the lazy. *)
    let forcing_domain_id : int = Obj.obj (Obj.field b 0) in
    let my_domain_id = domain_self () in
    (* XXX KC: Need a fence here to prevent the tag read from being reordered
     * before reading the first field of [b] *)
    if Obj.tag b = Obj.forcing_tag && forcing_domain_id = my_domain_id then
      raise Undefined
    else Racy
  else Forcing

let force_gen_lazy_block ~only_val blk =
  match update_tag_forcing blk with
  | Racy -> raise Undefined
  | Forcing when only_val -> do_force_val_block blk
  | Forcing -> do_force_block blk

(* used in the %lazy_force primitive *)
let force_lazy_block blk = force_gen_lazy_block ~only_val:false blk

let try_force_gen_lazy_block ~only_val blk =
  match update_tag_forcing blk with
  | Racy -> None
  | Forcing when only_val -> Some (do_force_val_block blk)
  | Forcing -> Some (do_force_block blk)

(* [force_gen ~only_val:false] is not used, since [Lazy.force] is
   declared as a primitive whose code inlines the tag tests of its
   argument. This function is here for the sake of completeness, and
   for debugging purpose. *)
let force_gen ~only_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> Obj.lazy_tag && t <> Obj.forcing_tag then (Obj.obj x : 'arg)
  else force_gen_lazy_block ~only_val lzv

let try_force_gen ~only_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then Some (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> Obj.lazy_tag && t <> Obj.forcing_tag then Some (Obj.obj x : 'arg)
  else try_force_gen_lazy_block ~only_val lzv
