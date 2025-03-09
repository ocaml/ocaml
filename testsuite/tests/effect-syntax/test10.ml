(* TEST *)

open Effect
open Effect.Deep

type _ eff += Peek : int eff
            | Poke : unit eff

let rec a i = perform Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  Random.int i +
  begin match c i with
  | v -> v
  | effect Poke, k -> continue k ()
  end

let rec e i =
  Random.int i +
  begin match d i with
  | v -> v
  | effect Peek, k ->
          ignore (Deep.get_callstack k 100);
          continue k 42
  end

let _ =
  ignore (e 1);
  print_string "ok\n"
