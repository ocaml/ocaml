type t = {
  emit_string : string -> unit;
  emit_symbol : string -> unit;
  emit_label_declaration : label_name:string -> unit;
}

let create ~emit_string ~emit_symbol ~emit_label_declaration =
  { emit_string; emit_symbol; emit_label_declaration; }

let emit_string t = t.emit_string
let emit_symbol t = t.emit_symbol
let emit_label_declaration t = t.emit_label_declaration
