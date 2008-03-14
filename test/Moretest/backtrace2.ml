(* A test for stack backtraces *)

exception Error of string

let rec f msg n =
  if n = 0 then raise(Error msg) else 1 + f msg (n-1)

let g msg =
  try
    f msg 5
  with Error "a" -> print_string "a"; print_newline(); 0
     | Error "b" as exn -> print_string "b"; print_newline(); raise exn
     | Error "c" -> raise (Error "c")

let run args =
  try
    ignore (g args.(0)); print_string "No exception\n"
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout

let _ =
  Printexc.record_backtrace true;
  run [| "a" |];
  run [| "b" |];
  run [| "c" |];
  run [| "d" |];
  run [| |]

(* Expected results:

a
No exception
b
Uncaught exception Backtrace2.Error("b")
Raised at file "backtrace2.ml", line 6, characters 21-32
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 10, characters 4-11
Re-raised at file "backtrace2.ml", line 12, characters 68-71
Called from file "backtrace2.ml", line 17, characters 11-23
Uncaught exception Backtrace2.Error("c")
Raised at file "backtrace2.ml", line 13, characters 26-37
Called from file "backtrace2.ml", line 17, characters 11-23
Uncaught exception Backtrace2.Error("d")
Raised at file "backtrace2.ml", line 6, characters 21-32
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 6, characters 42-53
Called from file "backtrace2.ml", line 10, characters 4-11
Called from file "backtrace2.ml", line 17, characters 11-23
Uncaught exception Invalid_argument("index out of bounds")
Raised by primitive operation at file "backtrace2.ml", line 17, characters 14-22
*)
