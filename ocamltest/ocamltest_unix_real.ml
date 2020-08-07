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

(* Unix.has_symlink never raises *)
let has_symlink = Unix.has_symlink

(* Convert Unix_error to Sys_error *)
let wrap f x =
  try f x
  with Unix.Unix_error(err, fn_name, arg) ->
    let msg =
      Printf.sprintf "%s failed on %S with %s"
                     fn_name arg (Unix.error_message err)
    in
      raise (Sys_error msg)

let symlink ?to_dir source = wrap (Unix.symlink ?to_dir source)
let chmod file = wrap (Unix.chmod file)
