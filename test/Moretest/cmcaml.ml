(* Caml part of the code *)

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let format_result n =
  "Result = " ^ string_of_int n

(* Registration *)

let _ =
  Callback.register "fib" fib;
  Callback.register "format_result" format_result

