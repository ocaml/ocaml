(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

open Effect

let num_domains = 2

type eff = effect Fork : (unit -> unit) -> unit

let eff = Effect.create ()

let fork f = perform eff (Fork f)

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
    run_with eff f ()
      {
        result = (fun () -> dequeue ());
        exn =
          (fun e ->
            print_string (Printexc.to_string e);
            dequeue ());
        operation =
          (fun (type a) (Fork f : (a, eff) operation)
               (k : (a, _) continuation) ->
            enqueue k;
            spawn f)
      }
  in
  let domains =
    Array.init num_domains (fun _ ->
        Domain.spawn (fun () -> spawn (work 100000)))
  in
  Array.iter Domain.join domains;
  print_endline "OK"
