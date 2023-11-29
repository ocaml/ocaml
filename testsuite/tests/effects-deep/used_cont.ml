(* TEST *)

open Effect

type e = effect E : unit

let eff = Effect.create ()

let r = ref None
let () =
  Effect.run_with eff (fun _ -> Effect.perform eff E; 42) ()
    { result = (fun n -> assert (n = 42));
      exn = (fun e -> raise e);
      operation =
        (fun (type a) (E : (a, _) operation) (k : (a,_) continuation) ->
          Effect.continue k ();
          r := Some (k : (unit, unit) continuation);
          Gc.full_major ();
          print_string "ok\n") }
