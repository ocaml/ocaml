(* TEST
 *)

effect Foo : int -> int

let r =
  try
    perform (Foo 3)
  with effect (Foo i) k ->
    (* continuation called outside try/with *)
    try
    continue k (i + 1)
    with effect (Foo i) k -> failwith "NO"

let () = Printf.printf "%d\n" r
