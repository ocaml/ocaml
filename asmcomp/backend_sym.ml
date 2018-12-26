(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Names of object file symbols. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type kind = Text | Data

type t = {
  kind : kind;
  compilation_unit : Compilation_unit.t;
  name : string;
  (* The [name] uniquely determines the symbol.  The [compilation_unit] is
     there so we can later easily distinguish between symbols from different
     units. *)
}

type backend_sym = t

let print ppf { name; _ } = Format.pp_print_string ppf name

let to_string { name; _ } = name

let of_symbol sym =
  { kind = Data;
    compilation_unit = Symbol.compilation_unit sym;
    name = Linkage_name.to_string (Symbol.label sym);
  }

let of_external_name compilation_unit name kind =
  { kind;
    compilation_unit;
    name;
  }

let create ~base_name kind =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let unit_name =
    Linkage_name.to_string (Compilation_unit.get_linkage_name compilation_unit)
  in
  let name = unit_name ^ "__" ^ base_name in
  { kind;
    compilation_unit;
    name;
  }

let compilation_unit t = t.compilation_unit

let kind t = t.kind

let add_suffix t new_suffix =
  { kind = t.kind;
    compilation_unit = t.compilation_unit;
    name = t.name ^ new_suffix;
  }

let add_suffixes t new_suffixes =
  List.fold_left (fun t new_suffix -> add_suffix t new_suffix) t new_suffixes

let add_int_suffix t new_suffix =
  add_suffix t (string_of_int new_suffix)

let to_escaped_string ?suffix ~symbol_prefix ~escape t =
  let suffix =
    match suffix with
    | None -> ""
    | Some suffix -> suffix
  in
  symbol_prefix ^ (escape t.name) ^ suffix

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = String.compare (to_string t1) (to_string t2)

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = Hashtbl.hash (to_string t)

  let print t = print t

  let output chn t = output_string chn (to_string t)
end)

module Names = struct
  let runtime = Compilation_unit.runtime
  let startup = Compilation_unit.startup
  let shared_startup = Compilation_unit.shared_startup

  let check_compilation_unit_is_startup_or_shared_startup comp_unit =
    if (not (Compilation_unit.equal comp_unit startup))
      || (not (Compilation_unit.equal comp_unit shared_startup))
    then begin
      Misc.fatal_errorf "Compilation unit for symbol must be specified as \
        either [startup] or [shared_startup]"
    end

  let sqrt =
    of_external_name runtime "sqrt" Text

  let caml_exception_pointer =
    of_external_name runtime "caml_exception_pointer" Data

  let caml_backtrace_pos =
    of_external_name runtime "caml_backtrace_pos" Data

  let caml_exn_Division_by_zero =
    of_external_name startup "caml_exn_Division_by_zero" Data

  let caml_nativeint_ops =
    of_external_name runtime "caml_nativeint_ops" Data

  let caml_int32_ops =
    of_external_name runtime "caml_int32_ops" Data

  let caml_int64_ops =
    of_external_name runtime "caml_int64_ops" Data

  let caml_send n =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    add_int_suffix (of_external_name comp_unit "caml_send" Text) n

  let caml_curry_n n =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    add_int_suffix (of_external_name comp_unit "caml_curry" Text) n

  let caml_curry_m_to_n m n =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    add_suffixes (of_external_name comp_unit "caml_curry" Text)
      [string_of_int m; "_"; string_of_int n]

  let caml_curry_m_to_n_app m n =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    add_suffixes (of_external_name comp_unit "caml_curry" Text)
      [string_of_int m; "_"; string_of_int n; "_app"]

  let caml_tuplify n =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    add_int_suffix (of_external_name comp_unit "caml_tuplify" Text) n

  let caml_apply n =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    add_int_suffix (of_external_name comp_unit "caml_apply" Text) n

  let caml_ba_get n =
    add_int_suffix (of_external_name runtime "caml_ba_get_" Text) n

  let caml_ba_set n =
    add_int_suffix (of_external_name runtime "caml_ba_set_" Text) n

  let caml_exn name =
    add_suffix (of_external_name startup "caml_exn_" Data) name

  let caml_call_gc =
    of_external_name runtime "caml_call_gc" Text

  let caml_modify =
    of_external_name runtime "caml_modify" Text

  let caml_initialize =
    of_external_name runtime "caml_initialize" Text

  let caml_get_public_method =
    of_external_name runtime "caml_get_public_method" Text

  let caml_alloc =
    of_external_name runtime "caml_alloc" Text

  let caml_ml_array_bound_error =
    of_external_name runtime "caml_ml_array_bound_error" Text

  let caml_raise_exn =
    of_external_name runtime "caml_raise_exn" Text

  let caml_make_array =
    of_external_name runtime "caml_make_array" Text

  let caml_bswap16_direct =
    of_external_name runtime "caml_bswap16_direct" Text

  type bswap_arg = Int32 | Int64 | Nativeint

  let caml_direct_bswap ty =
    let ty =
      match ty with
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Nativeint -> "nativeint"
    in
    of_external_name runtime (Printf.sprintf "caml_%s_direct_bswap" ty) Text

  let caml_alloc_dummy =
    of_external_name runtime "caml_alloc_dummy" Text

  let caml_alloc_dummy_float =
    of_external_name runtime "caml_alloc_dummy_float" Text

  let caml_update_dummy =
    of_external_name runtime "caml_update_dummy" Text

  let caml_program =
    of_external_name startup "caml_program" Text

  let caml_startup =
    of_external_name runtime "caml_startup" Text

  let caml_globals_inited =
    of_external_name runtime "caml_globals_inited" Data

  let caml_globals () =
    let comp_unit = Compilation_unit.get_current_exn () in
    check_compilation_unit_is_startup_or_shared_startup comp_unit;
    of_external_name comp_unit "caml_globals" Data

  let caml_plugin_header =
    of_external_name shared_startup "caml_plugin_header" Data

  let caml_globals_map =
    of_external_name startup "caml_globals_map" Data

  let caml_code_segments =
    of_external_name startup "caml_code_segments" Data

  let caml_data_segments =
    of_external_name startup "caml_data_segments" Data

  let caml_frametable =
    of_external_name startup "caml_frametable" Data

  let caml_spacetime_shapes =
    of_external_name startup "caml_spacetime_shapes" Data

  let caml_afl_area_ptr =
    of_external_name runtime "caml_afl_area_ptr" Data

  let caml_afl_prev_loc =
    of_external_name runtime "caml_afl_prev_loc" Data

  let caml_setup_afl =
    of_external_name runtime "caml_setup_afl" Text

  let caml_spacetime_allocate_node =
    of_external_name runtime "caml_spacetime_allocate_node" Text

  let caml_spacetime_indirect_node_hole_ptr =
    of_external_name runtime "caml_spacetime_indirect_node_hole_ptr" Text

  let caml_spacetime_generate_profinfo =
    of_external_name runtime "caml_spacetime_generate_profinfo" Text
end
