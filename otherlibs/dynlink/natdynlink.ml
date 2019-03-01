#3 "otherlibs/dynlink/natdynlink.ml"
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

(* Dynamic loading of .cmx files *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DC = Dynlink_common
module DT = Dynlink_types

type global_map = {
  name : string;
  crc_intf : Digest.t option;
  crc_impl : Digest.t option;
  syms : string list
}

module Native = struct
  type handle

  external ndl_open : string -> bool -> handle * Cmx_format.dynheader
    = "caml_natdynlink_open"
  external ndl_run : handle -> string -> unit = "caml_natdynlink_run"
  external ndl_getmap : unit -> global_map list = "caml_natdynlink_getmap"
  external ndl_globals_inited : unit -> int = "caml_natdynlink_globals_inited"

  module Unit_header = struct
    type t = Cmx_format.dynunit

    let name (t : t) = t.dynu_name
    let crc (t : t) = Some t.dynu_crc

    let interface_imports (t : t) = t.dynu_imports_cmi
    let implementation_imports (t : t) = t.dynu_imports_cmx

    let defined_symbols (t : t) = t.dynu_defines
    let unsafe_module _t = false
  end

  let init () = ()

  let is_native = true
  let adapt_filename f = Filename.chop_extension f ^ ".cmxs"

  let num_globals_inited () = ndl_globals_inited ()

  (* Copied from config.ml -- this file cannot depend on that. *)
  let cmxs_magic_number = "Caml1999D025"

  let fold_initial_units ~init ~f =
    let rank = ref 0 in
    List.fold_left (fun acc { name; crc_intf; crc_impl; syms; } ->
        rank := !rank + List.length syms;
        let implementation =
          match crc_impl with
          | None -> None
          | Some _ as crco -> Some (crco, DT.Check_inited !rank)
        in
        f acc ~comp_unit:name ~interface:crc_intf
            ~implementation ~defined_symbols:syms)
      init
      (ndl_getmap ())

  let run handle ~unit_header ~priv:_ =
    ndl_run handle "_shared_startup";
    List.iter (fun cu ->
        try ndl_run handle cu
        with exn -> raise (DT.Error (Library's_module_initializers_failed exn)))
      (Unit_header.defined_symbols unit_header)

  let load ~filename ~priv =
    let handle, header =
      try ndl_open filename (not priv)
      with exn -> raise (DT.Error (Cannot_open_dynamic_library exn))
    in
    if header.dynu_magic <> cmxs_magic_number then begin
      raise (DT.Error (Not_a_bytecode_file filename))
    end;
    handle, header.dynu_units

  let finish _handle = ()
end

include DC.Make (Native)

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
