open Std_internal

type t =
  | DW_TAG_compile_unit
  | DW_TAG_subprogram
  | DW_TAG_subprogram__no_children
  | DW_TAG_formal_parameter
  | DW_TAG_variable
  | DW_TAG_base_type

let encode t =
  let code =
    match t with
    | DW_TAG_compile_unit -> 0x11
    | DW_TAG_subprogram -> 0x2e
    | DW_TAG_subprogram__no_children -> 0x2e
    | DW_TAG_formal_parameter -> 0x05
    | DW_TAG_variable -> 0x34
    | DW_TAG_base_type -> 0x24
  in
  Value.as_uleb128 code

(* CR mshinwell: "__no_children" is a hack *)

let child_determination = function
  | DW_TAG_compile_unit -> Child_determination.yes
  | DW_TAG_subprogram -> Child_determination.yes
  | DW_TAG_subprogram__no_children -> Child_determination.no
  | DW_TAG_formal_parameter -> Child_determination.no
  | DW_TAG_variable -> Child_determination.no
  | DW_TAG_base_type -> Child_determination.no

let compile_unit = DW_TAG_compile_unit
let subprogram = DW_TAG_subprogram
let subprogram_with_no_children = DW_TAG_subprogram__no_children
let formal_parameter = DW_TAG_formal_parameter
let variable = DW_TAG_variable
let base_type = DW_TAG_base_type

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter
