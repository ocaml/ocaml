(* TEST
* bytecode
* native
*)

open Effect
open Effect.Deep

let num_domains = 2

type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t

let fork f = perform (Fork f)

let with_mutex m f =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) f

let rec work n () = if n = 0 then () else fork (work (n - 1))

let run =
  let run_q : (unit, unit) continuation Queue.t = Queue.create () in
  let run_m = Mutex.create () in
  let enqueue k = with_mutex run_m (fun () -> Queue.push k run_q) in
  let dequeue () =
    with_mutex run_m (fun () -> Queue.take_opt run_q)
    |> Option.iter (fun k -> continue k ())
  in

  let rec spawn f =
    (* Effect handler => instantiates fiber *)
    match_with f ()
      {
        retc = (fun () -> dequeue ());
        exnc =
          (fun e ->
            print_string (Printexc.to_string e);
            dequeue ());
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Fork f ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k;
                    spawn f)
            | _ -> None);
      }
  in
  let domains =
    Array.init num_domains (fun _ ->
        Domain.spawn (fun () -> spawn (work 100000)))
  in
  Array.iter Domain.join domains;
  print_endline "OK"
