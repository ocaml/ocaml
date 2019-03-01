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

let raise_undefined = Obj.repr (fun () -> raise Undefined)

external make_forward : Obj.t -> Obj.t -> unit = "caml_obj_make_forward"

(* Assume [blk] is a block with tag lazy *)
let force_lazy_block (blk : 'arg lazy_t) =
  let closure = (Obj.obj (Obj.field (Obj.repr blk) 0) : unit -> 'arg) in
  Obj.set_field (Obj.repr blk) 0 raise_undefined;
  try
    let result = closure () in
    make_forward (Obj.repr blk) (Obj.repr result);
    result
  with e ->
    Obj.set_field (Obj.repr blk) 0 (Obj.repr (fun () -> raise e));
    raise e


(* Assume [blk] is a block with tag lazy *)
let force_val_lazy_block (blk : 'arg lazy_t) =
  let closure = (Obj.obj (Obj.field (Obj.repr blk) 0) : unit -> 'arg) in
  Obj.set_field (Obj.repr blk) 0 raise_undefined;
  let result = closure () in
  make_forward (Obj.repr blk) (Obj.repr result);
  result


(* [force] is not used, since [Lazy.force] is declared as a primitive
   whose code inlines the tag tests of its argument.  This function is
   here for the sake of completeness, and for debugging purpose. *)

let force (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> Obj.lazy_tag then (Obj.obj x : 'arg)
  else force_lazy_block lzv


let force_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> Obj.lazy_tag then (Obj.obj x : 'arg)
  else force_val_lazy_block lzv
