type t =
  | Dwarf_2
  | Dwarf_3
  | Dwarf_4

let two = Dwarf_2
let three = Dwarf_3
let four = Dwarf_4

let encode t =
  let code =
    | Dwarf_2 -> 2
    | Dwarf_3 -> 3
    | Dwarf_4 -> 4
  in
  Value.as_two_byte_int code

let size t =
  Value.size (encode t)

let emit t =
  Value.emit (encode t)
