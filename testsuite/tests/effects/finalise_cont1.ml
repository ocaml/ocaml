(* TEST *)

open Effect
open Effect.Deep

type _ t += Foo : int -> int t

let f () =
  try perform (Foo 42) with
  | Continuation_deadlocked as e ->
        print_endline "caught deadlock";
        raise e
  | effect (Foo x), _k ->
      print_endline "handling Foo";
      43

let _ =
    ignore (f ());
    Gc.full_major ();
    print_endline "Ok"
