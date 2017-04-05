(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = string

include Identifiable.Make (struct
  include String
  let hash = Hashtbl.hash
  let print ppf t = Format.pp_print_string ppf t
  let output chan t = output_string chan t
end)

module List = struct
  let mem ts t =
    Set.mem t (Set.of_list ts)
end

let create t = t
let to_string t = t

let prefix t ~with_ = with_ ^ t
let append_int t i = t ^ (string_of_int i)
let append t ~suffix = t ^ suffix

let got t = t ^ "@GOT"
let gotpcrel t = t ^ "@GOTPCREL"
let plt t = t ^ "@PLT"

let mcount = create "mcount"
let sqrt = create "sqrt"

let _GLOBAL_OFFSET_TABLE_ = create "_GLOBAL_OFFSET_TABLE_"

let caml_young_ptr = create "caml_young_ptr"
let caml_young_limit = create "caml_young_limit"
let caml_exception_pointer = create "caml_exception_pointer"
let caml_negf_mask = create "caml_negf_mask"
let caml_absf_mask = create "caml_absf_mask"
let caml_backtrace_pos = create "caml_backtrace_pos"
let caml_exn_Division_by_zero = create "caml_exn_Division_by_zero"
let caml_nativeint_ops = create "caml_nativeint_ops"
let caml_int32_ops = create "caml_int32_ops"
let caml_int64_ops = create "caml_int64_ops"

let caml_call_gc = create "caml_call_gc"
let caml_modify = create "caml_modify"
let caml_initialize = create "caml_initialize"
let caml_send = create "caml_send"
let caml_get_public_method = create "caml_get_public_method"
let caml_c_call = create "caml_c_call"
let caml_curry = create "caml_curry"
let caml_tuplify = create "caml_tuplify"
let caml_apply = create "caml_apply"
let caml_alloc = create "caml_alloc"
let caml_alloc1 = create "caml_alloc1"
let caml_alloc2 = create "caml_alloc2"
let caml_alloc3 = create "caml_alloc3"
let caml_allocN = create "caml_allocN"
let caml_ml_array_bound_error = create "caml_ml_array_bound_error"
let caml_raise_exn = create "caml_raise_exn"
let caml_make_array = create "caml_make_array"
let caml_bswap16_direct = create "caml_bswap16_direct"
let caml_nativeint_direct_bswap = create "caml_nativeint_direct_bswap"
let caml_int32_direct_bswap = create "caml_int32_direct_bswap"
let caml_int64_direct_bswap = create "caml_int64_direct_bswap"
let caml_alloc_dummy = create "caml_alloc_dummy"
let caml_alloc_dummy_float = create "caml_alloc_dummy_float"
let caml_update_dummy = create "caml_update_dummy"
let caml_program = create "caml_program"
let caml_globals_inited = create "caml_globals_inited"
let caml_exn_ = create "caml_exn_"
let caml_globals = create "caml_globals"
let caml_plugin_header = create "caml_plugin_header"
let caml_globals_map = create "caml_globals_map"
let caml_code_segments = create "caml_code_segments"
let caml_data_segments = create "caml_data_segments"

let caml_frametable = create "caml_frametable"
let caml_spacetime_shapes = create "caml_spacetime_shapes"

let caml_code_begin = create "caml_code_begin"
let caml_code_end = create "caml_code_end"
let caml_data_begin = create "caml_data_begin"
let caml_data_end = create "caml_data_end"

let caml_afl_area_ptr = "caml_afl_area_ptr"
let caml_afl_prev_loc = "caml_afl_prev_loc"
let caml_setup_afl = "caml_setup_afl"

let caml_spacetime_allocate_node = create "caml_spacetime_allocate_node"
let caml_spacetime_indirect_node_hole_ptr =
  create "caml_spacetime_indirect_node_hole_ptr"
let caml_spacetime_generate_profinfo =
  create "caml_spacetime_generate_profinfo"
