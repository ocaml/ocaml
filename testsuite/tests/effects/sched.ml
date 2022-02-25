(* TEST
 *)

open Effect
open Effect.Deep

exception E
type _ t += Yield : unit t
          | Fork : (unit -> string) -> unit t
          | Ping : unit t
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
    match_with f ()
    { retc = (function
        | "ok" -> say "."; dequeue ()
        | s -> failwith ("Unexpected result: " ^ s));
      exnc = (function
        | E -> say "!"; dequeue ()
        | e -> raise e);
      effc = fun (type a) (e : a t) ->
        match e with
        | Yield -> Some (fun (k : (a, _) continuation) ->
            say ","; enqueue k; dequeue ())
        | Fork f -> Some (fun (k : (a, _) continuation) ->
            say "+"; enqueue k; spawn f)
        | Ping -> Some (fun (k : (a, _) continuation) ->
            say "["; discontinue k Pong)
        | _ -> None }
  in
  spawn main

let test () =
  say "A";
  perform (Fork (fun () ->
     perform Yield; say "C"; perform Yield;
     begin match_with (fun () -> perform Ping; failwith "no pong?") ()
      { retc = (fun x -> x);
        exnc = (function
          | Pong -> say "]"
          | e -> raise e);
        effc = fun (type a) (e : a t) ->
          match e with
          | Yield -> Some (fun (k : (a,_) continuation) -> failwith "what?")
          | _ -> None }
     end;
     raise E));
  perform (Fork (fun () -> say "B"; "ok"));
  say "D";
  perform Yield;
  say "E";
  "ok"

let () =
  let `Finished = run test in
  say "\n"
