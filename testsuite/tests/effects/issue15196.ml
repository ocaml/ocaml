(* TEST *)

(* #15196: the GC run while evaluating the argument of the outer [perform]
   promoted its continuation before [caml_perform] initialised it. *)

type _ Effect.t += E : int -> int Effect.t

let rec go : type c. (c, int) Effect.Shallow.continuation -> c -> int =
  fun k v ->
  Effect.Shallow.continue_with k v
    { retc = (fun x -> x);
      exnc = (fun e -> ignore (Sys.opaque_identity k); raise e);
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | E n ->
            Some (fun (k : (a, int) Effect.Shallow.continuation) ->
              Gc.compact (); go k n)
        | _ -> None) }

let () =
  let k =
    Effect.Shallow.fiber
      (fun () -> Effect.perform (E (Effect.perform (E 0))))
  in
  print_int (go k ());
  print_newline ()
