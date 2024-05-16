(* TEST *)

open Printf
open Effect
open Effect.Deep

(** {1 Coroutines} *)

type 'a channel = {
    senders: ('a * (unit, unit) continuation) Queue.t;
    receivers: ('a, unit) continuation Queue.t
  }

let new_channel () = { senders = Queue.create(); receivers = Queue.create() }

type _ eff += Spawn : (unit -> unit) -> unit eff
            | Yield : unit eff
            | Send  : 'a channel * 'a -> unit eff
            | Recv  : 'a channel -> 'a eff

exception Terminate

let spawn f = perform (Spawn f)

let yield () = perform Yield

let terminate () = raise Terminate

let send ch v = perform (Send(ch, v))

let recv ch = perform (Recv ch)

(** The queue of runnable tasks *)

let runnable : (unit -> unit) Queue.t = Queue.create()

let suspend f = Queue.add f runnable

let restart () =
  match Queue.take_opt runnable with
  | None -> ()
  | Some f -> f ()

(** The scheduler *)

let rec corun (f: unit -> unit) =
  match f () with
  | () | exception Terminate -> restart ()
  | effect Spawn f, k -> suspend (continue k); corun f
  | effect Yield, k -> suspend (continue k); restart ()
  | effect Send(ch, v), k ->
          begin match Queue.take_opt ch.receivers with
          | Some rc -> suspend (continue k); continue rc v
          | None    -> Queue.add (v, k) ch.senders; restart()
          end
  | effect Recv ch, k ->
          begin match Queue.take_opt ch.senders with
          | Some(v, sn) -> suspend (continue sn); continue k v
          | None        -> Queue.add k ch.receivers; restart()
          end

(** Example of use. *)

let task name n =
  for i = 1 to n do
    if i >= 7 then terminate();
    printf "%s%d " name i;
    yield()
  done

let _ =
  corun (fun () ->
    spawn (fun () -> task "a" 8);
    spawn (fun () -> task "b" 3);
    spawn (fun () -> task "c" 4));
  print_newline()

let _ =
  let ch = new_channel() in
  corun (fun () ->
    spawn (fun () -> send ch "a");
    spawn (fun () -> send ch "b");
    printf "%s " (recv ch);
    printf "%s\n" (recv ch))

(** Eratosthenes' sieve using a pipeline of filters. *)

let rec eratosthenes input =
  let p = recv input in
  printf "%d " p;
  let output = new_channel() in
  spawn (fun () -> eratosthenes output);
  while true do
    let n = recv input in
    if n mod p <> 0 then send output n
  done

let _ =
  corun (fun () ->
    let ints = new_channel() in
    spawn (fun () -> eratosthenes ints);
    for i = 2 to 1000 do send ints i done);
  print_newline()
