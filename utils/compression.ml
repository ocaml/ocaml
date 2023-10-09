(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*        Xavier Leroy, Collège de France and Inria project Cambium       *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external zstd_initialize: unit -> bool = "caml_zstd_initialize"

let compression_supported = zstd_initialize ()

external to_channel: out_channel -> 'a -> int list -> unit = "caml_output_value"

let to_channel ch v (flags : Marshal.extern_flags list) =
  (* Add the Compression (constructor 4) to the list of flags *)
  to_channel ch v (3 :: (Obj.magic flags : int list))
