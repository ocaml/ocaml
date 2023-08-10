(* TEST *)

open Effect

type e = effect E : unit

let eff = Effect.create ()

exception Done

let handle_partial f =
  Effect.run_with eff f ()
    { result = Fun.id;
      exn = raise;
      operation = (fun (type a) (E : (a, _) operation) k -> assert false) }

let f () x = Effect.perform eff E

let () =
  Effect.run_with eff (handle_partial f) ()
    { result = (fun x -> assert false);
      exn = (function
        | Done -> print_string "ok\n"
        | e -> raise e);
      operation =
        (fun (type a) (E : (a, _) operation) (k : (a, _) continuation) ->
          Effect.discontinue k Done) }
