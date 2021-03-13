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
exception RacyLazy

(* [update_to_forcing blk] expects [blk] to be a lazy value with one of the
 * tags -- [Obj.lazy_tag], [Obj.forward_tag], [Obj.forcing_tag] -- or a
 * short-circuited lazy object. This function is implemented in C due to the
 * atomicity requirements wrt the GC.
 *
 * If [blk] happens to be a lazy value with [Obj.lazy_tag], the tag is changed
 * to [Obj.forcing_tag] using compare-and-swap taking care to handle concurrent
 * marking of the header word by a concurrent GC thread. Returns [0] in this
 * case.
 *
 * If [blk] happens to be [Obj.forcing_tag] and the domain that earlier forced
 * this lazy is the current domain, then returns [1]. This condition indicates
 * recursive forcing of this lazy value.
 *
 * If [blk] happens to be [Obj.forcing_tag] and the domain that earlier forced
 * this lazy is a different domain, then returns [2]. This condition indicates
 * a race on forcing this lazy value.
 *
 * If [blk] happens to be [Obj.forward_tag], then returns [3].
 *
 * If [blk] happens to have any other tag or is a primitive value, then returns
 * [4].
 *)
external update_to_forcing : Obj.t -> int = "caml_lazy_update_to_forcing"

(* [reset_to_lazy blk] expects [blk] to be a lazy object with [Obj.forcing_tag]
 * and updates the tag to [Obj.lazy_tag], taking care to handle concurrent
 * marking of this object's header by a concurrent GC thread.
 *)
external reset_to_lazy : Obj.t -> unit = "caml_lazy_reset_to_lazy"

(* [update_to_forward blk] expects [blk] to be a lazy object with
 * [Obj.forcing_tag] and updates the tag to [Obj.forward_tag], taking care to
 * handle concurrent marking of this object's header by a concurrent GC thread.
 *)
external update_to_forward : Obj.t -> unit = "caml_lazy_update_to_forward"

external domain_self : unit -> int = "caml_ml_domain_id"

(* Assumes [blk] is a block with tag forcing *)
let do_force_block blk =
  let b = Obj.repr blk in
  let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
  Obj.set_field b 0 (Obj.repr (domain_self ()));
  try
    let result = closure () in
    Obj.set_field b 0 (Obj.repr result);
    update_to_forward b;
    result
  with e ->
    begin match e with
    | RacyLazy -> Obj.set_field b 0 (Obj.repr closure)
    | _ -> Obj.set_field b 0 (Obj.repr (fun () -> raise e))
    end;
    reset_to_lazy b;
    raise e

(* Assumes [blk] is a block with tag forcing *)
let do_force_val_block blk =
  let b = Obj.repr blk in
  let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
  Obj.set_field b 0 (Obj.repr (domain_self ()));
  let result = closure () in
  Obj.set_field b 0 (Obj.repr result);
  update_to_forward b;
  result

let force_gen_lazy_block ~only_val (blk : 'arg lazy_t) =
  match update_to_forcing (Obj.repr blk) with
  | 0 when only_val -> do_force_val_block blk
  | 0 -> do_force_block blk
  | 1 -> raise Undefined
  | _ ->
      (* We raise [RacyLazy] for all the other cases.
       *
       * If the return value was 2, we should raise RacyLazy. We had observed
       * earlier in [force_gen] that the [blk] was not [Obj.forward_tag] and
       * not short-circuited. But now we observe that the value has either been
       * forwarded (return value 3) or forwarded and short-circuited (4). Both
       * of these indicate a race. *)
      raise RacyLazy

(* used in the %lazy_force primitive *)
let force_lazy_block blk = force_gen_lazy_block ~only_val:false blk

let try_force_gen_lazy_block ~only_val (blk : 'arg lazy_t) =
  match update_to_forcing (Obj.repr blk) with
  | 0 when only_val -> Some (do_force_val_block blk)
  | 0 -> Some (do_force_block blk)
  | 1 -> raise Undefined
  | _ ->
      (* Other cases indicate racy access to lazy for the same reasons outlined
       * in [force_gen_lazy_block]. *)
      None

(* [force_gen ~only_val:false] is not used, since [Lazy.force] is
   declared as a primitive whose code inlines the tag tests of its
   argument. This function is here for the sake of completeness, and
   for debugging purpose. *)
let force_gen ~only_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg)
  else if t <> Obj.lazy_tag && t <> Obj.forcing_tag then (Obj.obj x : 'arg)
  else force_gen_lazy_block ~only_val lzv

let try_force_gen ~only_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then Some (Obj.obj (Obj.field x 0) : 'arg)
  else if t <> Obj.lazy_tag && t <> Obj.forcing_tag then Some (Obj.obj x : 'arg)
  else try_force_gen_lazy_block ~only_val lzv
