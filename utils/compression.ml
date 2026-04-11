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

type [@warning "-unused-constructor"] extern_flags =
    No_sharing                          (** Don't preserve sharing *)
  | Closures                            (** Send function closures *)
  | Compat_32                           (** Ensure 32-bit compatibility *)
  | Compression                         (** Optional compression *)

external to_bytes_internal: 'a -> extern_flags list -> bytes
                          = "caml_output_value_to_bytes"

let output_value ch v =
  let b = to_bytes_internal v [Compression] in
  output ch b 0 (Bytes.length b)

let input_value = Stdlib.input_value
