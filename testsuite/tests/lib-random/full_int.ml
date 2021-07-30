(* TEST *)

(* Ensure that a bound which is negative on 31-bit OCaml but positive on 32-bit
   OCaml produces the same result on 64-bit OCaml. *)
let bound = 0x6FFFFFFF in
if bound < 0 then (* 31-bit integers *)
  print_endline "6beb775a"
else (* 32 or 64-bit integers *)
  let s = Random.State.make [| 42 |] in
  Printf.printf "%x\n" (Random.State.full_int s bound)
