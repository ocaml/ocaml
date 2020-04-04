(* TEST

flags = "-w +A-70"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** check-ocamlopt.byte-output
**** run
***** check-program-output
*)

type a = { mutable a : int }

let alloc {a} b = a + b

let noalloc b {a} = b + a

let measure name f =
  let a = {a = 1} in
  let b = 2 in
  let before = Gc.minor_words () in
  let (_ : int) = f ~a ~b in
  let after = Gc.minor_words () in
  let alloc = int_of_float (after -. before) in
  match alloc with
  | 0 -> Printf.printf "%S doesn't allocate\n" name
  | _ -> Printf.printf "%S allocates\n" name

let () =
  measure "noalloc" (fun ~a ~b -> noalloc b a);
  measure "alloc" (fun ~a ~b -> alloc a b)


let dont_warn_with_partial_match None x = x
