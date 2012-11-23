type t =
  | DW_CHILDREN_no
  | DW_CHILDREN_yes

let encode = function
  | DW_CHILDREN_no -> 0x00
  | DW_CHILDREN_yes -> 0x01

let no = DW_CHILDREN_no
let yes = DW_CHILDREN_yes

let size _t = 1

let emit t ~emitter =
  Value.emit (Value.as_byte (encode t)) ~emitter
