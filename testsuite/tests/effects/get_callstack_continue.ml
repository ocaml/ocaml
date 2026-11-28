(* TEST
 multicore;
 {
   bytecode;
 }{
   native;
 }
*)

(* A [continue] on one domain must succeed while another domain runs
   [Effect.Deep.get_callstack] on the same continuation. *)

open Effect
open Effect.Deep

type _ t += E : unit t

let gen () : (unit, int) continuation =
  let r : (unit, int) continuation option ref = ref None in
  let rec deep n = if n = 0 then (perform E; 0) else 1 + deep (n - 1) in
  ignore (match deep 2000 with
    | x -> x
    | effect E, k -> r := Some k; 0);
  Option.get !r

let trial () =
  let k = gen () in
  let stop = Atomic.make false and started = Atomic.make false in
  let reader = Domain.spawn (fun () ->
    Atomic.set started true;
    try
      while not (Atomic.get stop) do
        ignore (Sys.opaque_identity (get_callstack k 50))
      done
    with Continuation_already_resumed -> ()) in
  while not (Atomic.get started) do Domain.cpu_relax () done;
  for _ = 1 to 2000 do Domain.cpu_relax () done;
  let result =
    match continue k () with
    | _ -> true
    | exception Continuation_already_resumed -> false
  in
  Atomic.set stop true;
  Domain.join reader;
  result

let () =
  let failures = ref 0 in
  for _ = 1 to 50 do if not (trial ()) then incr failures done;
  Printf.printf "continue during get_callstack: %d failures of 50\n" !failures
