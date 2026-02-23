(* TEST *)

open Effect
open Effect.Deep

type _ t += Foo : int -> int t

let f n =
  try perform (Foo 42) with
  | Continuation_deadlocked as e ->
        Printf.printf "f(%d): caught deadlock\n%!" n;
        Gc.full_major ();
        raise e
  | effect (Foo x), _k ->
      Printf.printf "f(%d): handling Foo\n%!" n;
      43

let _ =
    ignore (f 0);
    ignore (f 1);
    Gc.full_major ();
    print_endline "Ok"
