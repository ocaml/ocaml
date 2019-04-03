(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Misc.Stdlib.String

type kind = Text | Data

let create ?compilation_unit ~base_name _kind =
  ignore compilation_unit;
  Compilenv.make_symbol (Some base_name)

(* This is deliberately not exposed in the .mli.  (Add things to [Names],
   below, instead.) *)
let of_external_name _object_file name _kind = name

let add_suffix t new_suffix = t ^ new_suffix

let add_suffixes t new_suffixes =
  List.fold_left (fun t new_suffix -> add_suffix t new_suffix) t new_suffixes

let add_int_suffix t new_suffix = add_suffix t (string_of_int new_suffix)

let name_for_asm_symbol t = t

module Names = struct
  let runtime_or_external kind name =
    of_external_name Object_file.runtime_and_external_libs name kind
  let startup kind name = of_external_name Object_file.startup name kind

  let atan = runtime_or_external Text "atan"
  let atan2 = runtime_or_external Text "atan2"
  let cos = runtime_or_external Text "cos"
  let log = runtime_or_external Text "log"
  let log10 = runtime_or_external Text "log10"
  let sin = runtime_or_external Text "sin"
  let sqrt = runtime_or_external Text "sqrt"
  let tan = runtime_or_external Text "tan"

  let __aeabi_idivmod = runtime_or_external Text "__aeabi_idivmod"
  let __aeabi_idiv = runtime_or_external Text "__aeabi_idiv"
  let __aeabi_dadd = runtime_or_external Text "__aeabi_dadd"
  let __aeabi_dsub = runtime_or_external Text "__aeabi_dsub"
  let __aeabi_dmul = runtime_or_external Text "__aeabi_dmul"
  let __aeabi_ddiv = runtime_or_external Text "__aeabi_ddiv"
  let __aeabi_i2d = runtime_or_external Text "__aeabi_i2d"
  let __aeabi_d2iz = runtime_or_external Text "__aeabi_d2iz"
  let __aeabi_dcmpeq = runtime_or_external Text "__aeabi_dcmpeq"
  let __aeabi_dcmplt = runtime_or_external Text "__aeabi_dcmplt"
  let __aeabi_dcmple = runtime_or_external Text "__aeabi_dcmple"
  let __aeabi_dcmpgt = runtime_or_external Text "__aeabi_dcmpgt"
  let __aeabi_dcmpge = runtime_or_external Text "__aeabi_dcmpge"
  let __aeabi_f2d = runtime_or_external Text "__aeabi_f2d"
  let __aeabi_d2f = runtime_or_external Text "__aeabi_d2f"

  let caml_exception_pointer = runtime_or_external Data "caml_exception_pointer"
  let caml_backtrace_pos = runtime_or_external Data "caml_backtrace_pos"
  let caml_exn_Division_by_zero = startup Data "caml_exn_Division_by_zero"

  let caml_nativeint_ops = runtime_or_external Data "caml_nativeint_ops"
  let caml_int32_ops = runtime_or_external Data "caml_int32_ops"
  let caml_int64_ops = runtime_or_external Data "caml_int64_ops"

  let caml_curry_n n = add_int_suffix (startup Text "caml_curry") n

  let caml_curry_m_to_n m n =
    add_suffixes (startup Text "caml_curry")
      [string_of_int m; "_"; string_of_int n]

  let caml_curry_m_to_n_app m n =
    add_suffixes (startup Text "caml_curry")
      [string_of_int m; "_"; string_of_int n; "_app"]

  let caml_tuplify n = add_int_suffix (startup Text "caml_tuplify") n
  let caml_apply n = add_int_suffix (startup Text "caml_apply") n
  let caml_send n = add_int_suffix (startup Text "caml_send") n

  let caml_ba_get n = add_int_suffix (runtime_or_external Text "caml_ba_get_") n
  let caml_ba_set n = add_int_suffix (runtime_or_external Text "caml_ba_set_") n

  let caml_call_gc = runtime_or_external Text "caml_call_gc"
  let caml_modify = runtime_or_external Text "caml_modify" 
  let caml_initialize = runtime_or_external Text "caml_initialize"
  let caml_get_public_method = runtime_or_external Text "caml_get_public_method"
  let caml_alloc = runtime_or_external Text "caml_alloc"
  let caml_ml_array_bound_error =
    runtime_or_external Text "caml_ml_array_bound_error"
  let caml_raise_exn = runtime_or_external Text "caml_raise_exn"
  let caml_make_array = runtime_or_external Text "caml_make_array"
  let caml_bswap16_direct = runtime_or_external Text "caml_bswap16_direct"

  type bswap_arg = Int32 | Int64 | Nativeint

  let caml_direct_bswap ty =
    let ty =
      match ty with
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Nativeint -> "nativeint"
    in
    runtime_or_external Text (Printf.sprintf "caml_%s_direct_bswap" ty)

  let caml_alloc_dummy = runtime_or_external Text "caml_alloc_dummy"
  let caml_alloc_dummy_float = runtime_or_external Text "caml_alloc_dummy_float"
  let caml_update_dummy = runtime_or_external Text "caml_update_dummy"
  let caml_program = startup Text "caml_program"
  let caml_startup = runtime_or_external Text "caml_startup"
  let caml_globals_inited = runtime_or_external Data "caml_globals_inited"
  let caml_globals = startup Data "caml_globals"
  let caml_plugin_header = startup Data "caml_plugin_header"
  let caml_globals_map = startup Data "caml_globals_map"
  let caml_code_segments = startup Data "caml_code_segments"
  let caml_data_segments = startup Data "caml_data_segments"
  let caml_frametable = startup Data "caml_frametable"

  let caml_afl_area_ptr = runtime_or_external Data "caml_afl_area_ptr"
  let caml_afl_prev_loc = runtime_or_external Data "caml_afl_prev_loc"
  let caml_setup_afl = runtime_or_external Text "caml_setup_afl"

  let caml_spacetime_shapes = startup Data "caml_spacetime_shapes"
  let caml_spacetime_allocate_node =
    runtime_or_external Text "caml_spacetime_allocate_node"
  let caml_spacetime_indirect_node_hole_ptr =
    runtime_or_external Text "caml_spacetime_indirect_node_hole_ptr"
  let caml_spacetime_generate_profinfo =
    runtime_or_external Text "caml_spacetime_generate_profinfo"
end
