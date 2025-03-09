(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

let _ =
  try Unix.utimes "does-not-exist" 0.0 0.0
  with Unix.(Unix_error(ENOENT, _, _)) -> ()
