(* input: 100000. Triggers stack overflow which invokes caml_call_realloc_stack,
 * which inturn causes GC to be invoked. *)

let rec sum n = if n = 0 then 0 else n + sum (n-1)

let n =
  if Array.length Sys.argv != 2 then 100000
  else int_of_string @@ Sys.argv.(1)
let () = Printf.printf "sum(%d) = %d\n" n (sum n)
