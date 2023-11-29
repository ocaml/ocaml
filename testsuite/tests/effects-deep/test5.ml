(* TEST *)

open Effect

type foo = effect Foo : int -> int

let foo = Effect.create ()

let f () = (Effect.perform foo (Foo 3)) (* 3 + 1 *)
         + (Effect.perform foo (Foo 3)) (* 3 + 1 *)

let r =
  Effect.run_with foo f ()
  { result = Fun.id;
    exn = raise;
    operation =
      (fun (type a) (Foo i : (a, foo) operation) (k : (a, _) continuation) ->
        Effect.run_with foo (Effect.continue k) (i + 1)
          { result = Fun.id;
            exn = raise;
            operation =
              (fun (type a) (e : (a, foo) operation) k ->
                failwith "NO")})}

let () = Printf.printf "%d\n" r
