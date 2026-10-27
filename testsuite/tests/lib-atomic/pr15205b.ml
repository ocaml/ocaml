(* TEST *)
let a = Atomic.make 0
let () =
  let n = Sys.opaque_identity 2 in
  let x = Sys.opaque_identity 1 in
  let y0 = Sys.opaque_identity 10 in
  let y1 = Sys.opaque_identity 11 in
  let y2 = Sys.opaque_identity 12 in
  let y3 = Sys.opaque_identity 13 in
  let y4 = Sys.opaque_identity 14 in
  let y5 = Sys.opaque_identity 15 in
  let y6 = Sys.opaque_identity 16 in
  let y7 = Sys.opaque_identity 17 in
  let old = Atomic.fetch_and_add a 1 in
  for _ = x downto n lor 0 do
    if sin 1. = 0. then
      ignore (Sys.opaque_identity (if n = 0 then old + y0 + y1 + y2 + y3 + y4 + y5 + y6 + y7 else 0))
  done;
  print_int (Atomic.get a)
