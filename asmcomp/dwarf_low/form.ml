type t = [
| DW_FORM_addr
| DW_FORM_string
| DW_FORM_data1
| DW_FORM_data4
| DW_FORM_data8
| DW_FORM_flag
| DW_FORM_block
| DW_FORM_ref_addr
]

let encode t =
  let code =
    match t with
    | DW_FORM_addr -> 0x01
    | DW_FORM_data1 -> 0x0b
    | DW_FORM_data4 -> 0x06
    | DW_FORM_data8 -> 0x07
    | DW_FORM_string -> 0x08
    | DW_FORM_flag -> 0x0c
    | DW_FORM_block -> 0x09
    | DW_FORM_ref_addr -> 0x10
  in
  Value.as_uleb128 code

let addr = DW_FORM_addr
let data1 = DW_FORM_data1
let data4 = DW_FORM_data4
let data8 = DW_FORM_data8
let string = DW_FORM_string
let flag = DW_FORM_flag
let block = DW_FORM_block
let ref_addr = DW_FORM_ref_addr

let size t =
  Value.size (encode t)

let emit t ~emitter =
  Value.emit (encode t) ~emitter
