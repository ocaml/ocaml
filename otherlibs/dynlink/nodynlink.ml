#2 "otherlibs/dynlink/nodynlink.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*              Mark Shinwell and Leo White, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DC = Dynlink_common
module DT = Dynlink_types

let not_available _ =
  failwith "No support for native dynlink on this platform"

module Not_available = struct
  module Unit_header = struct
    type t = unit

    let name = not_available
    let crc = not_available

    let interface_imports = not_available
    let implementation_imports = not_available

    let defined_symbols = not_available
    let unsafe_module = not_available
  end

  type handle = unit

  let init = not_available

  let is_native = false
  let adapt_filename = not_available

  let num_globals_inited = not_available

  let fold_initial_units ~init ~f:_ = not_available init

  let run_shared_startup _ = not_available ()
  let run _ ~unit_header:_ ~priv:_ = not_available ()
  let load ~filename:_ ~priv:_ = not_available ()
  let finish = not_available
end

include DC.Make (Not_available)

type linking_error = DT.linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = DT.error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error = DT.Error
let error_message = DT.error_message
