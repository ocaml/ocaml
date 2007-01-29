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

let _ =
  ignore (g Sys.argv.(1))

(* Expected results:

OCAMLRUNPARAM=b=1 ./backtrace.out a
a

OCAMLRUNPARAM=b=1 ./backtrace.out b
b
Fatal error: exception Backtrace.Error("b")
Raised at file "backtrace.ml", line 6, characters 21-32
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 10, characters 4-11
Re-raised at file "backtrace.ml", line 12, characters 68-71
Called from file "backtrace.ml", line 16, characters 9-25

OCAMLRUNPARAM=b=1 ./backtrace.out c
Fatal error: exception Backtrace.Error("c")
Raised at file "backtrace.ml", line 13, characters 26-37
Called from file "backtrace.ml", line 16, characters 9-25

OCAMLRUNPARAM=b=1 ./backtrace.out d
Fatal error: exception Backtrace.Error("d")
Raised at file "backtrace.ml", line 6, characters 21-32
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 6, characters 42-53
Called from file "backtrace.ml", line 10, characters 4-11
Called from file "backtrace.ml", line 16, characters 9-25

OCAMLRUNPARAM=b=1 ./backtrace.out
Fatal error: exception Invalid_argument("index out of bounds")
Raised at file "backtrace.ml", line 16, characters 12-24

*)
