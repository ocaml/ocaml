(* TEST
 *)

exception E
effect Yield : unit
effect Fork : (unit -> string) -> unit
effect Ping : unit
exception Pong

let say = print_string

let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then `Finished
    else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    match f () with
    | "ok" -> say "."; dequeue ()
    | s -> failwith ("Unexpected result: " ^ s)
    | exception E ->
        say "!"; dequeue ()
    | effect Yield k ->
        say ","; enqueue k; dequeue ()
    | effect (Fork f) k ->
        say "+"; enqueue k; spawn f
    | effect Ping k ->
        say "["; discontinue k Pong
  in
  spawn main

let test () =
  say "A";
  perform (Fork (fun () ->
     perform Yield; say "C"; perform Yield;
     (try (perform Ping; failwith "no pong?")
      with effect Yield k -> failwith "what?" | Pong -> say "]");
     raise E));
  perform (Fork (fun () -> say "B"; "ok"));
  say "D";
  perform Yield;
  say "E";
  "ok"

let () =
  let `Finished = run test in
  say "\n"
