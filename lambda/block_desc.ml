(* Descriptors definition and construction *)

open Introspect

type approx = Introspect.Desc.approx

type t = Introspect.Desc.t

let dump = Desc.dump

let compare = Desc.compare

let format ppf t =
  dump (Format.pp_print_string ppf) t

let mask = (1 lsl Config.reserved_header_bits) - 1

let index t =
  if Sys.introspection_enabled then
    mask land (Desc.hash t)
  else
    0

external compiler_descriptors : unit -> Desc.t list ref = "caml_compiler_block_descs"
let library = compiler_descriptors ()

let register t =
  if Sys.introspection_enabled then
    library := t :: !library; t

let make_array approx =
  register (Desc.Array approx)

let make_tuple tag name fields =
  register (Desc.Tuple {name; tag; fields})

let make_record tag name fields =
  register (Desc.Record {name; tag; fields})

let register_polymorphic_variant name =
  ignore (register (Desc.Polymorphic_variant_constant name) : Desc.t)

let make_polymorphic_variant name =
  register_polymorphic_variant name;
  register Desc.Polymorphic_variant

let empty = Desc.Unknown
(* Manage collections of descriptors *)

let pending_descriptors () = !library

type library = Desc.t list

let empty_library = []

let iter_library = List.iter

let emit () =
  library := List.sort_uniq compare !library;
  empty :: !library

let reset () =
  library := []

let[@tail_mod_cons] rec merge_uniq x xs = function
  | [] -> x :: xs
  | y :: ys ->
      let c = compare x y in
      if c = 0 then
        merge_uniq x xs ys
      else if c < 0 then
        x :: merge_uniq y ys xs
      else
        y :: merge_uniq x xs ys

let[@tail_mod_cons] rec merge_all = function
  | [] | [_] as list -> list
  | [] :: rest -> merge_all rest
  | (x :: xs) :: ys :: rest ->
      let xy = merge_uniq x xs ys in
      xy :: merge_all rest

let rec merge_loop = function
  | [] -> []
  | [x] -> x
  | list -> merge_loop (merge_all list)

let link libraries = merge_loop libraries
