(* TEST *)

type e = effect E : unit

let eff : e Effect.t = Effect.create ()

let () =
  Printf.printf "%d\n%!" @@
    (match Effect.run eff (fun x -> x) 10 with
     | Effect.Result x -> x
     | Effect.Exn e -> raise e
     | Effect.Operation(E, _) -> 11)
