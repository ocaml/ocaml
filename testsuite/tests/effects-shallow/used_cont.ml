(* TEST *)

type e = effect E : unit

let eff = Effect.create ()

let r = ref None
let () =
  let rec handle = function
    | Effect.Result n -> assert (n = 42)
    | Effect.Exn e -> raise e
    | Effect.Operation(E, k) ->
        handle (Effect.continue k ());
        r := Some (k : (unit, _) Effect.continuation);
        Gc.full_major ();
        print_string "ok\n"
  in
  handle (Effect.run eff (fun _ -> Effect.perform eff E; 42) ())

