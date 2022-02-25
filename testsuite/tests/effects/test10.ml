(* TEST
 *)

open Effect
open Effect.Deep

type _ t += Peek : int t
type _ t += Poke : unit t

let rec a i = perform Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  Random.int i +
  try_with c i
  { effc = fun (type a) (e : a t) ->
      match e with
      | Poke -> Some (fun (k : (a,_) continuation) -> continue k ())
      | _ -> None }

let rec e i =
  Random.int i +
  try_with d i
  { effc = fun (type a) (e : a t) ->
      match e with
      | Peek -> Some (fun (k : (a,_) continuation) ->
          ignore (Deep.get_callstack k 100);
          continue k 42)
      | _ -> None }

let _ =
  ignore (e 1);
  print_string "ok\n"
