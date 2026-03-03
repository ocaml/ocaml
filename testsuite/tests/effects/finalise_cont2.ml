(* TEST *)

open Effect
open Effect.Deep

type _ t += Foo : int -> int t

let f () =
  let _ =
      try perform (Foo 42) with
      | Continuation_deadlocked as e ->
            Printf.printf "[%d] caught deadlock\n" (Domain.self () :> int);
            raise e
      | effect (Foo x), _k ->
            print_endline "handling Foo";
            43
  in
  print_endline "finishing f"

let _ =
    let d = Domain.spawn f in
    ignore (Domain.join d);
    Gc.full_major ();
    print_endline "Ok"
