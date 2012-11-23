type t = {
  abbreviation_code : Abbreviation_code.t;
  tag : Tag.t;
  attributes : Attribute.t list;
}

let create ~abbreviation_code ~tag ~attributes =
  { abbreviation_code;
    tag;
    attributes;
  }

let emit t ~emitter =
  Abbreviation_code.emit t.abbreviation_code ~emitter;
  Tag.emit t.tag ~emitter;
  Child_determination.emit (Tag.child_determination t.tag) ~emitter;
  List.iter t.attributes ~f:(Attribute.emit_followed_by_form ~emitter);
  Value.emit (Value.as_uleb128 0) ~emitter;
  Value.emit (Value.as_uleb128 0) ~emitter
