(* TEST
 *)

effect Foo : int -> int

let f () = (perform (Foo 3)) (* 3 + 1 *)
         + (perform (Foo 3)) (* 3 + 1 *)

let r =
  try
    f ()
  with effect (Foo i) k ->
    (* continuation called outside try/with *)
    try
    continue k (i + 1)
    with effect (Foo i) k -> failwith "NO"

let () = Printf.printf "%d\n" r
