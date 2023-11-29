(* TEST *)

open Effect

exception E

type sched = effect
  | Yield : unit
  | Fork : (unit -> string) -> unit
  | Ping : unit

let sched = Effect.create ()

exception Pong

let say = print_string

let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then `Finished
    else Effect.continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    Effect.run_with sched f ()
      { result = (function
          | "ok" -> say "."; dequeue ()
          | s -> failwith ("Unexpected result: " ^ s));
        exn = (function
          | E -> say "!"; dequeue ()
          | e -> raise e);
        operation =
          (fun (type a) (op : (a, _) operation) (k : (a, _) continuation) ->
            match op with
            | Yield ->
                say ","; enqueue k; dequeue ()
            | Fork f ->
                say "+"; enqueue k; spawn f
            | Ping ->
                say "["; Effect.discontinue k Pong) }
  in
  spawn main

let test () =
  say "A";
  Effect.perform sched (Fork (fun () ->
     Effect.perform sched Yield; say "C"; perform sched Yield;
     Effect.run_with sched
       (fun () -> Effect.perform sched Ping; failwith "no pong?") ()
       { result = (fun x -> x);
         exn = (function
           | Pong -> say "]"
           | e -> raise e);
         operation =
           (fun (type a) (op : (a, _) operation) k ->
             match op with
             | Yield -> failwith "what?"
             | _ -> Effect.reperform sched op k) };
     raise E));
  Effect.perform sched (Fork (fun () -> say "B"; "ok"));
  say "D";
  Effect.perform sched Yield;
  say "E";
  "ok"

let () =
  let `Finished = run test in
  say "\n"
