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

(* Assume [blk] is a block with tag lazy *)
let force_lazy_block (blk : 'arg lazy_t) =
  let b = Obj.repr blk in
  if not (update_tag b Obj.lazy_tag Obj.forcing_tag) then
    (* blk has tag Obj.forcing_tag *)
    raise Undefined
  else begin
    let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
    try
      let result = closure () in
      make_forward b (Obj.repr result);
      result
    with e ->
      Obj.set_field b 0 (Obj.repr (fun () -> raise e));
      assert (update_tag b Obj.forcing_tag Obj.lazy_tag);
      raise e
  end


(* Assume [blk] is a block with tag lazy *)
let force_val_lazy_block (blk : 'arg lazy_t) =
  let b = Obj.repr blk in
  if not (update_tag b Obj.lazy_tag Obj.forcing_tag) then
    (* blk has tag Obj.forcing_tag *)
    raise Undefined
  else begin
    let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
    let result = closure () in
    make_forward b (Obj.repr result);
    result
  end

(* [force] is not used, since [Lazy.force] is declared as a primitive
   whose code inlines the tag tests of its argument.  This function is
   here for the sake of completeness, and for debugging purpose. *)

let force (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> Obj.lazy_tag && t <> Obj.forcing_tag then (Obj.obj x : 'arg)
  else force_lazy_block lzv


let force_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> Obj.lazy_tag && t <> Obj.forcing_tag then (Obj.obj x : 'arg)
  else force_val_lazy_block lzv
