(*
 * https://github.com/ocamllabs/ocaml-multicore/issues/131
 *)
let section = ref None

let create_section_option n =
  let o = Some n in
  section := Some o;
  o

let b = ref 42
let autoscan = create_section_option (!b)

let () =
  print_int (match !section with Some (Some n) -> n | _ -> 100)
