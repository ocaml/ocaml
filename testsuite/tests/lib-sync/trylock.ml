(* TEST
*)

(* Test Mutex.try_lock *)

let () =
  let m = Mutex.create () in
  Mutex.lock m;
  let res = Mutex.try_lock m in
  Mutex.unlock m;
  if res = false then
    print_endline "passed"
  else
    print_endline "FAILED (try_lock returned true)"
