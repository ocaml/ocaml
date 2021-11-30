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
 * If [blk] happens to be [Obj.forcing_tag], then either this domain is
 * recursively forcing this lazy value, or the lazy value is concurrently
 * forced by another domain. In both of these cases, return [1].
 *
 * If [blk] happens to be [Obj.forward_tag], then returns [2].
 *
 * If [blk] happens to have any other tag or is a primitive value, then returns
 * [3].
 *)
external update_to_forcing : Obj.t -> int =
  "caml_lazy_update_to_forcing" [@@noalloc]

(* [reset_to_lazy blk] expects [blk] to be a lazy object with [Obj.forcing_tag]
 * and updates the tag to [Obj.lazy_tag], taking care to handle concurrent
 * marking of this object's header by a concurrent GC thread.
 *)
external reset_to_lazy : Obj.t -> unit = "caml_lazy_reset_to_lazy" [@@noalloc]

(* [update_to_forward blk] expects [blk] to be a lazy object with
 * [Obj.forcing_tag] and updates the tag to [Obj.forward_tag], taking care to
 * handle concurrent marking of this object's header by a concurrent GC thread.
 *)
external update_to_forward : Obj.t -> unit =
  "caml_lazy_update_to_forward" [@@noalloc]

(* Assumes [blk] is a block with tag forcing *)
let do_force_block blk =
  let b = Obj.repr blk in
  let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
  Obj.set_field b 0 (Obj.repr ()); (* Release the closure *)
  try
    let result = closure () in
    Obj.set_field b 0 (Obj.repr result);
    update_to_forward b;
    result
  with e ->
    Obj.set_field b 0 (Obj.repr (fun () -> raise e));
    reset_to_lazy b;
    raise e

(* Assumes [blk] is a block with tag forcing *)
let do_force_val_block blk =
  let b = Obj.repr blk in
  let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
  Obj.set_field b 0 (Obj.repr ()); (* Release the closure *)
  let result = closure () in
  Obj.set_field b 0 (Obj.repr result);
  update_to_forward b;
  result

(* Called by [force_gen] *)
let force_gen_lazy_block ~only_val (blk : 'arg lazy_t) =
  (* In the common case, expect the tag to be [lazy_tag], but may be other tags
     due to concurrent forcing of lazy values. *)
  match update_to_forcing (Obj.repr blk) with
  | 0 when only_val -> do_force_val_block blk
  | 0 -> do_force_block blk
  | _ ->
      (* If the return value is 1, then the tag is [forcing_tag]. We are either
         recursively forcing on the same domain, or concurrently forcing from
         multiple domains. Raise [Undefined].

         If the return value is 2, then the tag is [forward_tag]. In
         [force_gen], we checked whether the tag was [forward_tag]. It has
         changed since the last read. Hence, there is another domain which is
         concurrently forcing this lazy. Raise [Undefined].

         If the return value is 3, then the tag is not [forcing_tag] or
         [forward_tag], which we checked in [force_gen]. The tag is not
         [lazy_tag] or otherwise [update_to_forcing] would have succeeded and
         returned [0]. The tag has changed from the expected [lazy_tag] due to
         another domain concurrently, and the GC short circuiting the result.
         Raise [Undefined].
         *)
      raise Undefined

(* used in the %lazy_force primitive *)
let force_lazy_block blk = force_gen_lazy_block ~only_val:false blk

(* [force_gen ~only_val:false] is not used, since [Lazy.force] is
   declared as a primitive whose code inlines the tag tests of its
   argument, except when afl instrumentation is turned on. *)
let force_gen ~only_val (lzv : 'arg lazy_t) =
  let lzv = Sys.opaque_identity lzv in
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  (* START no safe points. If a GC occurs here, then the object [x] may be
     short-circuited, and getting the first field of [x] would get us the wrong
     value. Luckily, the compiler does not insert GC safe points at this place,
     so it is ok. *)
  if t = Obj.forward_tag then
    (Obj.obj (Obj.field x 0) : 'arg)
  (* END no safe points *)
  else if t = Obj.forcing_tag then raise Undefined
  else if t <> Obj.lazy_tag then (Obj.obj x : 'arg)
  else force_gen_lazy_block ~only_val lzv
