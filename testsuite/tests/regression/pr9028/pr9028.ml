(* TEST *)

let f n = ((n lsl 1) + 1) / 2
let g n = (n lsl 1) / 2
let h n = Int64.of_int (n * 2 + 1)
let i n = Int64.of_int (Int64.to_int n)

let r = Sys.opaque_identity max_int
let s = Sys.opaque_identity Int64.max_int
let () = Printf.printf "%d\n%d\n%Ld\n%Ld\n" (f r) (g r) (h r) (i s)
