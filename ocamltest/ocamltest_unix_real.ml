(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 David Allsopp, OCaml Labs, Cambridge.                  *)
(*                                                                        *)
(*   Copyright 2020 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* It's tempting just to have include Unix, but the binary is then quite a bit
   bigger. *)
let has_symlink = Unix.has_symlink
