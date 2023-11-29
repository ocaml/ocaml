(* TEST *)

open Effect

type foo = effect Foo : int -> int

let foo = Effect.create ()

let r =
  Effect.run_with foo (Effect.perform foo) (Foo 3)
    { result = Fun.id;
      exn = raise;
      operation =
        (fun (type a) (Foo i : (a, foo) operation) (k : (a, _) continuation) ->
          Effect.run_with foo (Effect.continue k) (i+1)
            { result = Fun.id;
              exn = raise;
              operation =
                (fun (type a) (Foo i : (a, foo) operation) k ->
                  failwith "NO") })}

let () = Printf.printf "%d\n" r
