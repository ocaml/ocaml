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

let default_cmp_tool = "cmp"

let default_cmp_flags = "-q"

let compare_files
  ?(cmp_tool = default_cmp_tool)
  ?(cmp_flags = default_cmp_flags)
  files =
  let command = String.concat " "
  [
    cmp_tool;
    cmp_flags;
    files.reference_filename;
    files.output_filename
  ] in
  match Sys.command command with 0 -> true | _ -> false

let check_file
  ?(cmp_tool = default_cmp_tool)
  ?(cmp_flags = default_cmp_flags)
  files =
  if Sys.file_exists files.reference_filename
  then compare_files ~cmp_tool:cmp_tool ~cmp_flags:cmp_flags files
  else Testlib.file_is_empty files.output_filename
