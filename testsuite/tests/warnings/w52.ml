let good () =
  Printexc.record_backtrace true;
  try raise Not_found
  with _ -> Printexc.print_backtrace stderr

let bad () =
  Printexc.print_backtrace stderr
