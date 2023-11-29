(* TEST *)

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
    else handle (Effect.continue (Queue.pop run_q) ())
  and handle = function
    | Effect.Result "ok" ->
        say "."; dequeue ()
    | Effect.Result s ->
        failwith ("Unexpected result: " ^ s)
    | Effect.Exn E ->
        say "!"; dequeue ()
    | Effect.Exn e ->
        raise e
    | Effect.Operation(Yield, k) ->
        say ","; enqueue k; dequeue ()
    | Effect.Operation(Fork f, k) ->
        say "+"; enqueue k; handle (Effect.run sched f ())
    | Effect.Operation(Ping, k) ->
        say "["; handle (Effect.discontinue k Pong)
  in
  handle (Effect.run sched main ())

let test () =
  say "A";
  Effect.perform sched (Fork (fun () ->
     Effect.perform sched Yield; say "C"; Effect.perform sched Yield;
     let rec handle = function
       | Effect.Result x -> x
       | Effect.Exn Pong -> say "]"
       | Effect.Exn e -> raise e
       | Effect.Operation(Yield, k) -> failwith "what?"
       | Effect.Operation(op, k) -> handle (Effect.reperform sched op k)
     in
     handle
       (Effect.run sched
          (fun () -> Effect.perform sched Ping; failwith "no pong?")
          ());
     raise E));
  Effect.perform sched (Fork (fun () -> say "B"; "ok"));
  say "D";
  Effect.perform sched Yield;
  say "E";
  "ok"

let () =
  let `Finished = run test in
  say "\n"
