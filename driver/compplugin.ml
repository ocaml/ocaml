(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A table to avoid double linking of plugins, especially with OCAMLPARAM *)
let plugins = Hashtbl.create 13

let load plugin_name =

  let plugin_name =
    try
      Compdynlink.adapt_filename plugin_name
    with Invalid_argument _ -> plugin_name
  in

  let plugin_file =
    if Filename.is_implicit plugin_name then
      try
        Compmisc.init_path !Clflags.native_code;
        Load_path.find plugin_name
      with Not_found ->
        failwith (Printf.sprintf "Cannot find plugin %s in load path"
          plugin_name)
    else plugin_name
  in

  if not (Hashtbl.mem plugins plugin_file) then begin
    Compdynlink.loadfile plugin_file;
    Hashtbl.add plugins plugin_file (); (* plugin loaded *)
  end

let () =
  Location.register_error_of_exn (function
  | Compdynlink.Error error ->
    Some (Location.error (
      Printf.sprintf "%s while loading argument of -plugin"
        (Compdynlink.error_message error)))
  | _ -> None);
  Compenv.load_plugin := load
