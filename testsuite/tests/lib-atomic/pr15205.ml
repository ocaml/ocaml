(* TEST *)

let a = Atomic.make 0
let () =
  let n = Sys.opaque_identity 2 in
  let x = Sys.opaque_identity 1 in
  let old = Atomic.exchange a x in
  for _ = x downto n lor 0 do
    if sin 1. = 0. then ignore (Sys.opaque_identity (if n = 0 then old else 0))
  done;
  print_int (Atomic.get a)
