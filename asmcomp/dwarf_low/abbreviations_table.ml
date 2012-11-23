type t = Abbreviations_table_entry.t list

let create abbrev_table_entries =
  abbrev_table_entries

let emit t ~emitter =
  List.iter t ~f:(Abbreviations_table_entry.emit ~emitter);
  Value.emit (Value.as_uleb128 0) ~emitter
