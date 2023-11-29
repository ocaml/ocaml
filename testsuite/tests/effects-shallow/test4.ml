(* TEST *)

type foo = effect Foo : int -> int

let foo = Effect.create ()

let r =
  let rec handle = function
  | Effect.Result v -> v
  | Effect.Exn e -> raise e
  | Effect.Operation(Foo i, k) ->
      match Effect.run foo (fun () -> handle (Effect.continue k (i+1))) () with
      | Result x -> x
      | Exn e -> raise e
      | Operation(Foo _, _) -> failwith "NO"
  in
  handle (Effect.run foo (Effect.perform foo) (Foo 3))

let () = Printf.printf "%d\n" r
