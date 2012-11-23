type t = {
  size : Value.t;
  values : Value.t list;
}

let create ~start_of_code_label ~end_of_code_label =
  let address_width_in_bytes_on_target = Value.as_byte 8 in
  let values = [
    Value.as_two_byte_int 2;  (* section version number *)
    Value.as_four_byte_int 0;
    address_width_in_bytes_on_target;
    Value.as_byte 0;
    Value.as_two_byte_int 0;
    Value.as_two_byte_int 0;
    Value.as_code_address_from_label start_of_code_label;
    Value.as_code_address_from_label_diff
      end_of_code_label start_of_code_label;
    Value.as_code_address Int64.zero;
    Value.as_code_address Int64.zero;
  ]
  in
  let size =
    List.fold_left values
      ~init:0
      ~f:(fun size value -> size + Value.size value)
  in
  { size; values; }

let size t = t.size

let emit t ~emitter =
  Value.emit (Value.as_four_byte_int t.size) ~emitter;
  List.iter t.values ~f:(Value.emit ~emitter)
