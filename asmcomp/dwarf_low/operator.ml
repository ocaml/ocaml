open Std_internal

type t =
  | DW_op_regx of Value.t

let register ~reg_number ~offset:_ =
  let reg_number = Value.as_uleb128 reg_number in
  DW_op_regx reg_number

let opcode = function
  | DW_op_regx _ -> 0x90

let size t =
  let opcode_size = 1 in
  let args_size =
    match t with
    | DW_op_regx reg_number -> Value.size reg_number
  in
  opcode_size + args_size

let emit t ~emitter =
  Value.emit (Value.as_byte (opcode t)) ~emitter;
  match t with
  | DW_op_regx reg_number -> Value.emit reg_number ~emitter
