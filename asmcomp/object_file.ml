(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Current_compilation_unit
  | Another_compilation_unit
  | Startup
  | Shared_startup
  | Runtime_and_external_libs

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Stdlib.compare t1 t2
  let equal t1 t2 = compare t1 t2 = 0
  let hash t = Hashtbl.hash t

  let print ppf t =
    match t with
    | Current_compilation_unit ->
      Format.pp_print_string ppf "Current_compilation_unit"
    | Another_compilation_unit ->
      Format.pp_print_string ppf "Another_compilation_unit"
    | Startup -> Format.pp_print_string ppf "Startup"
    | Shared_startup -> Format.pp_print_string ppf "Shared_startup"
    | Runtime_and_external_libs ->
      Format.pp_print_string ppf "Runtime_and_external_libs"

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

let current_compilation_unit = Current_compilation_unit
let another_compilation_unit = Another_compilation_unit
let startup = Startup
let shared_startup = Shared_startup
let runtime_and_external_libs = Runtime_and_external_libs
