(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* File comparison tools *)

type t = {
  reference_filename : string;
  output_filename : string;
}

val default_cmp_tool : string

val default_cmp_flags : string

val compare_files : ?cmp_tool:string -> ?cmp_flags:string -> t -> bool

val check_file : ?cmp_tool:string -> ?cmp_flags:string -> t -> bool
