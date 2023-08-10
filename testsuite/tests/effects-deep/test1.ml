(* TEST *)

type e = effect E : unit

let eff : e Effect.t = Effect.create ()

let () =
  Printf.printf "%d\n%!" @@
    Effect.run_with eff (fun x -> x) 10
      { result = Fun.id;
        exn = raise;
        operation = (fun op k -> 11); }
