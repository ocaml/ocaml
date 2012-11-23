open Std_internal

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

let size t =
  Abbreviation_code.size t.abbreviation_code
    + Tag.size t.tag
    + Child_determination.size (Tag.child_determination t.tag)
    + List.fold t.attributes ~init:0 ~f:(fun size attr -> size + Attribute.size attr)
    + Value.size (Value.as_uleb128 0)
    + Value.size (Value.as_uleb128 0)

let emit t ~emitter =
  Abbreviation_code.emit t.abbreviation_code ~emitter;
  Tag.emit t.tag ~emitter;
  Child_determination.emit (Tag.child_determination t.tag) ~emitter;
  List.iter t.attributes ~f:(Attribute.emit ~emitter);
  Value.emit (Value.as_uleb128 0) ~emitter;
  Value.emit (Value.as_uleb128 0) ~emitter
