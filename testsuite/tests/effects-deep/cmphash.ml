(* TEST *)

type e = effect E : unit

let e = Effect.create ()

let () =
  Effect.run_with e (Effect.perform e) E
  { result = Fun.id;
    exn = raise;
    operation =
      (fun (type a) (E : (a, _) operation) k ->
        begin match k = k with
        | _ -> assert false
        | exception (Invalid_argument _) -> print_endline "ok"
        end;
        begin match Hashtbl.hash k with
        | _ -> print_endline "ok"
        end) }
