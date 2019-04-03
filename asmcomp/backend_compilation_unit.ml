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
  | Compilation_unit of Compilation_unit.t
  | Startup
  | Shared_startup
  | Runtime_and_external_libs

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Compilation_unit comp_unit1, Compilation_unit comp_unit2 ->
      Compilation_unit.compare comp_unit1 comp_unit2
    | Startup, Startup -> 0
    | Shared_startup, Shared_startup -> 0
    | Runtime_and_external_libs, Runtime_and_external_libs -> 0
    | Compilation_unit _, _ -> -1
    | Startup, (Compilation_unit _) -> 1
    | Startup, _ -> -1
    | Shared_startup, (Compilation_unit _ | Startup) -> 1
    | Shared_startup, _ -> -1
    | Runtime_and_external_libs,
        (Compilation_unit _ | Startup | Shared_startup) -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash t =
    match t with
    | Compilation_unit comp_unit ->
      Hashtbl.hash (0, Compilation_unit.hash comp_unit)
    | Startup -> Hashtbl.hash 1
    | Shared_startup -> Hashtbl.hash 2
    | Runtime_and_external_libs -> Hashtbl.hash 3

  let print ppf t =
    match t with
    | Compilation_unit comp_unit ->
      Format.fprintf ppf "@[<hov 1>(Compilation_unit %a)@]"
        Compilation_unit.print comp_unit
    | Startup -> Format.pp_print_string ppf "Startup"
    | Shared_startup -> Format.pp_print_string ppf "Shared_startup"
    | Runtime_and_external_libs ->
      Format.pp_print_string ppf "Runtime_and_external_libs"

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

let compilation_unit comp_unit = Compilation_unit comp_unit
let startup = Startup
let shared_startup = Shared_startup
let runtime_and_external_libs = Runtime_and_external_libs
