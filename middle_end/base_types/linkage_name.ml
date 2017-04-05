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

let create t = t
let to_string t = t

let prefix t ~with_ = with_ ^ t

let got t = t ^ "@GOT"
let gotpcrel t = t ^ "@GOTPCREL"
let plt t = t ^ "@PLT"

let mcount = create "mcount"

let _GLOBAL_OFFSET_TABLE_ = create "_GLOBAL_OFFSET_TABLE_"

let caml_young_ptr = create "caml_young_ptr"
let caml_young_limit = create "caml_young_limit"
let caml_exception_pointer = create "caml_exception_pointer"
let caml_negf_mask = create "caml_negf_mask"
let caml_absf_mask = create "caml_absf_mask"

let caml_call_gc = create "caml_call_gc"
let caml_c_call = create "caml_c_call"
let caml_allocN = create "caml_allocN"
let caml_alloc1 = create "caml_alloc1"
let caml_alloc2 = create "caml_alloc2"
let caml_alloc3 = create "caml_alloc3"
let caml_ml_array_bound_error = create "caml_ml_array_bound_error"
let caml_raise_exn = create "caml_raise_exn"

let caml_frametable = create "caml_frametable"
let caml_spacetime_shapes = create "caml_spacetime_shapes"

let caml_code_begin = create "caml_code_begin"
let caml_code_end = create "caml_code_end"
let caml_data_begin = create "caml_data_begin"
let caml_data_end = create "caml_data_end"
