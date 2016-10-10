
let rec f x =
  if !x = 0 then raise Not_found;
  1 + g (!x-1)

and g x =
  1 + f (ref x)

let _ =
  Printexc.record_backtrace true;
  f (ref 1000)
