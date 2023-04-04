(* TEST *)

let[@inline never][@local never] f n =
  let n = Int64.of_int n in
  let open Int64 in
  to_int (add n (of_int Int.min_int))

let _ = Printf.printf "0x%x\n%!" (f 1)
