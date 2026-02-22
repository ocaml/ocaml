(* TEST *)

open Effect
open Effect.Shallow

type _ t += Foo : int -> int t


let f x =
  try perform x with
  | Continuation_deadlocked as e ->
      print_endline "caught deadlock";
      raise e
  | _ -> assert false

let g () =
  let k = fiber f in
  continue_with k (Foo 42)
  {
      retc = (fun x -> x);
      exnc = (fun e ->
        print_endline "should not see this";
        raise e);
      effc = (fun (type b) (eff : b t) ->
        match eff with
        | Foo x ->
              Some (fun (_k : (b, _) continuation) ->
                  print_endline "handling Foo";
                  43)
        | _ -> None)
  }

let _ =
    ignore (g ());
    Gc.full_major ();
    print_endline "Ok"
