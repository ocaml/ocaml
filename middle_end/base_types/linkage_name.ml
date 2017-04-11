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

module TS = Target_system

module Kind = struct
  type t =
    | Normal
    | GOT
    | GOTPCREL
    | PLT
    | I386_stub
    | I386_non_lazy_ptr
    | POWER_tocbase

  let prefix = function
    | Normal
    | GOT
    | GOTPCREL
    | PLT
    | POWER_tocbase -> ""
    | I386_stub
    | I386_non_lazy_ptr -> "L"

  let to_string = function
    | Normal -> ""
    | GOT ->
      begin match TS.architecture () with
      | IA32 | IA64 | AArch64 | POWER | SPARC | Z -> "@GOT"
      | ARM -> "(GOT)"
      end
    | GOTPCREL ->
      begin match TS.architecture () with
      | IA64 -> "@GOTPCREL"
      | IA32 | ARM | AArch64 | POWER | SPARC | Z ->
        Misc.fatal_error "GOTPCREL relocations only supported on IA64"
      end
    | PLT ->
      begin match TS.architecture () with
      | IA32 | IA64 | AArch64 | POWER | SPARC | Z -> "@PLT"
      | ARM -> "(PLT)"
      end
    | I386_stub ->
      begin match TS.architecture () with
      | IA32 -> "$stub"
      | IA64 | ARM | AArch64 | POWER | SPARC | Z ->
        Misc.fatal_error "Cannot use IA32-specific stub markers on non-IA32 \
          platforms"
      end
    | I386_non_lazy_ptr ->
      begin match TS.architecture () with
      | IA32 -> "$non_lazy_ptr"
      | IA64 | ARM | AArch64 | POWER | SPARC | Z ->
        Misc.fatal_error "Cannot use IA32-specific non lazy pointer markers \
          on non-IA32 platforms"
      end
    | POWER_tocbase ->
      begin match TS.architecture () with
      | POWER -> "@tocbase"
      | IA32 | IA64 | ARM | AArch64 | SPARC | Z ->
        Misc.fatal_error "tocbase relocations only supported on POWER"
      end

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Pervasives.compare
    let equal (t1 : t) t2 = (t1 = t2)
    let hash = Hashtbl.hash

    let print ppf t = Format.fprintf ppf "%s" (to_string t)
    let output chan t = output_string chan (to_string t)
  end)

  let check_normal t =
    match t with
    | Normal -> ()
    | _ -> Misc.fatal_error "Cannot change symbol kind twice"
end

type t = {
  name : string;
  kind : Kind.t;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    let c = Pervasives.compare t1.name t2.name in
    if c <> 0 then c
    else Kind.compare t1.kind t2.kind

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t =
    Hashtbl.hash (t.name, Kind.hash t.kind)

  let print ppf t =
    (* CR mshinwell: think a bit more about this; is it misleading that it's
       different from [to_string]? *)
    Format.fprintf ppf "%s%s%a" (Kind.prefix t.kind) t.name Kind.print t.kind

  let output chan t =
    output_string chan t.name;
    output_string chan (Kind.to_string t.kind)
end)

let symbol_prefix =
  match TS.architecture () with
  | IA32 ->
    begin match TS.system () with
    | Linux _
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Other_BSD
    | Solaris
    | BeOS
    | GNU -> ""
    | MacOS_like
    | Windows _
    | Unknown -> "_"
    end
  | IA64 ->
    begin match TS.system () with
    | MacOS_like -> "_"
    | Linux _
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Other_BSD
    | Solaris
    | BeOS
    | GNU
    | Windows _
    | Unknown -> ""
    end
  | POWER -> "."
  | ARM
  | AArch64
  | Z
  | SPARC -> ""

let _GLOBAL_OFFSET_TABLE_ = "_GLOBAL_OFFSET_TABLE_"
let __dummy__ = "__dummy__"

let create name =
  if String.length name > 1 then begin
    (* It's not technically a problem for this condition to fail, but we
       shouldn't need to be in that position, and this check should catch
       bugs. *)
    if String.length symbol_prefix > 0
      && Misc.Stdlib.String.is_prefix symbol_prefix ~of_:name
      && not (String.equal name _GLOBAL_OFFSET_TABLE_)
      && not (String.equal name __dummy__)
    then begin
      Misc.fatal_errorf "Suspicious creation of [Linkage_name.t]: has this \
          symbol name already been mangled? '%s'"
        name
    end
  end;
  { name; kind = Normal; }

let to_string t =
  let s = t.name in
  let spec = ref false in
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
    | _ -> spec := true;
  done;
  let without_kind =
    if not !spec then if symbol_prefix = "" then s else symbol_prefix ^ s
    else
      let b = Buffer.create (String.length s + 10) in
      Buffer.add_string b symbol_prefix;
      String.iter
        (function
          | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
          | c -> Printf.bprintf b "$%02x" (Char.code c)
          (* CR mshinwell: may need another char instead of '$' *)
        )
        s;
      Buffer.contents b
  in
  (Kind.prefix t.kind) ^ without_kind ^ (Kind.to_string t.kind)

let name t = t.name

let prefix t ~with_ =
  { t with
    name = with_ ^ t.name;
  }

let append_int t i =
  { t with
    name = t.name ^ (string_of_int i);
  }

let append t ~suffix =
  { t with
    name = t.name ^ suffix;
  }

let got t =
  Kind.check_normal t.kind;
  { t with kind = GOT; }

let gotpcrel t =
  Kind.check_normal t.kind;
  { t with kind = GOTPCREL; }

let plt t =
  Kind.check_normal t.kind;
  { t with kind = PLT; }

let i386_stub t =
  Kind.check_normal t.kind;
  { t with kind = I386_stub; }

let i386_non_lazy_ptr t =
  Kind.check_normal t.kind;
  { t with kind = I386_non_lazy_ptr; }

let power_tocbase t =
  Kind.check_normal t.kind;
  { t with kind = POWER_tocbase; }

let mcount = create "mcount"
let _mcount = create "_mcount"
let __gnu_mcount_nc = create "__gnu_mcount_nc"
let sqrt = create "sqrt"

let __dummy__ = create __dummy__

let _GLOBAL_OFFSET_TABLE_ = create _GLOBAL_OFFSET_TABLE_

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

let caml_afl_area_ptr = create "caml_afl_area_ptr"
let caml_afl_prev_loc = create "caml_afl_prev_loc"
let caml_setup_afl = create "caml_setup_afl"

let caml_spacetime_allocate_node = create "caml_spacetime_allocate_node"
let caml_spacetime_indirect_node_hole_ptr =
  create "caml_spacetime_indirect_node_hole_ptr"
let caml_spacetime_generate_profinfo =
  create "caml_spacetime_generate_profinfo"

let caml_extra_params = create "caml_extra_params"

let is_generic_function t =
  let name = to_string t in
  List.exists (fun p -> Misc.Stdlib.String.is_prefix p ~of_:name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

module List = struct
  let mem ts t =
    Set.mem t (Set.of_list ts)
end
