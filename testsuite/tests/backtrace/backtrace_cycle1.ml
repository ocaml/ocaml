
let rec f x =
  if !x = 0 then raise Not_found;
  1+ f (ref (!x-1))

let _ =
  Printexc.record_backtrace true;
  f (ref 2000)
