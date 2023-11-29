(* TEST *)

type e = effect E : unit

exception X

let eff = Effect.create ()

let () =
  Printf.printf "%d\n%!" @@
  Effect.run_with eff (fun () ->
    Printf.printf "in handler. raising X\n%!";
    raise X) ()
    { result = (fun v -> v);
      exn = (function
        | X -> 10
        | e -> raise e);
      operation = (fun (type k) (E : (k, e) operation) k -> 11) }
