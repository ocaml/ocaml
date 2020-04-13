(* TEST
 *)

effect Peek : int
effect Poke : unit

let rec a i = perform Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  Random.int i +
  match c i with
  | v -> v
  | effect Poke k -> continue k ()

let rec e i =
  Random.int i +
  match d i with
  | v -> v
  | effect Peek k ->
      ignore (Printexc.get_continuation_callstack k 100);
      continue k 42

let _ =
  ignore (e 1);
  print_string "ok\n"
