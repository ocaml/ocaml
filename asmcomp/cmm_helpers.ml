(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module V = Backend_var
module VP = Backend_var.With_provenance
open Cmm
open Arch

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let bind_load name arg fn =
  match arg with
  | Cop(Cload _, [Cvar _], _) -> fn arg
  | _ -> bind name arg fn

let bind_nonvar name arg fn =
  match arg with
    Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8
    (* cf. runtime/caml/gc.h *)

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag dbg = Cconst_int (Obj.double_array_tag, dbg)

let block_header tag sz =
  Nativeint.add (Nativeint.shift_left (Nativeint.of_int sz) 10)
                (Nativeint.of_int tag)
(* Static data corresponding to "value"s must be marked black in case we are
   in no-naked-pointers mode.  See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black
let white_closure_header sz = block_header Obj.closure_tag sz
let black_closure_header sz = black_block_header Obj.closure_tag sz
let infix_header ofs = block_header Obj.infix_tag ofs
let float_header = block_header Obj.double_tag (size_float / size_addr)
let floatarray_header len =
  (* Zero-sized float arrays have tag zero for consistency with
     [caml_alloc_float_array]. *)
  assert (len >= 0);
  if len = 0 then block_header 0 0
  else block_header Obj.double_array_tag (len * size_float / size_addr)
let string_header len =
      block_header Obj.string_tag ((len + size_addr) / size_addr)
let boxedint32_header = block_header Obj.custom_tag 2
let boxedint64_header = block_header Obj.custom_tag (1 + 8 / size_addr)
let boxedintnat_header = block_header Obj.custom_tag 2

let alloc_float_header dbg = Cblockheader (float_header, dbg)
let alloc_floatarray_header len dbg = Cblockheader (floatarray_header len, dbg)
let alloc_closure_header sz dbg = Cblockheader (white_closure_header sz, dbg)
let alloc_infix_header ofs dbg = Cblockheader (infix_header ofs, dbg)
let alloc_boxedint32_header dbg = Cblockheader (boxedint32_header, dbg)
let alloc_boxedint64_header dbg = Cblockheader (boxedint64_header, dbg)
let alloc_boxedintnat_header dbg = Cblockheader (boxedintnat_header, dbg)

