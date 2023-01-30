# 2 "asmcomp/loongarch64/reload.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                yala <zhaojunchao@loongson.cn>                          *)
(*                                                                        *)
(*               Copyright © 2008-2023 LOONGSON                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Reloading for the LoongArch *)

let fundecl f =
  (new Reloadgen.reload_generic)#fundecl f
