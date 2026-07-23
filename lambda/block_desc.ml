type approx = Obj.Tag_descriptor.approx =
  | Any
  | Char
  | Int
  | Constants of string array
  | Polymorphic_variants

type t = Obj.Tag_descriptor.t =
  | Unknown
  | Array of approx
  | Tuple  of { name: string; tag: int; fields: approx array }
  | Record of { name: string; tag: int; fields: (string * approx) array }
  | Polymorphic_variant
  | Polymorphic_variant_constant of string

let dump = Intro.Desc.dump

let format ppf t =
  dump (Format.pp_print_string ppf) t

let mask = (1 lsl Config.reserved_header_bits) - 1

let index t = mask land (Obj.Tag_descriptor.hash t)

external compiler_tags : unit -> t list ref = "caml_compiler_tags"
let library = compiler_tags ()

let register t = library := t :: !library; t

let make_array approx =
  register (Array approx)

let make_tuple tag name fields =
  register (Tuple {name; tag; fields})

let make_record tag name fields =
  register (Record {name; tag; fields})

let register_polymorphic_variant name =
  ignore (register (Polymorphic_variant_constant name) : t)

let make_polymorphic_variant name =
  register_polymorphic_variant name;
  register Polymorphic_variant

let empty = Unknown

let emit_tags () =
  library := List.sort_uniq compare !library;
  empty :: !library

let reset_tags () =
  library := []
