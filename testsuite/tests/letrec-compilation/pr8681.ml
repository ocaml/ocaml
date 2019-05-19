(* TEST *)
let rec h =
  let rec f n = if n >= 0 then g (n - 1)
  and g n = h n; f n in
  f

let () = Gc.minor ()
let () = ignore (h 10)
