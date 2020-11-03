(* TEST
 * naked_pointers
 ** bytecode
 ** native
*)

let native =
  match Sys.backend_type with
  | Sys.Native -> true
  | Sys.Bytecode -> false
  | Sys.Other s -> print_endline s; assert false

let size x = Obj.reachable_words (Obj.repr x)

let expect_size s x =
  let i = size x in
  if i <> s then
    Printf.printf "size = %i; expected = %i\n%!" i s

let () =
  expect_size (if native then 0 else 3) (1, 2)
