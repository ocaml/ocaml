(* TEST *)

type e = effect E : unit

let e = Effect.create ()

let () =
  match Effect.run e (Effect.perform e) E with
  | Result x -> x
  | Exn e -> raise e
  | Operation(E, k) ->
      begin match k = k with
      | _ -> assert false
      | exception (Invalid_argument _) -> print_endline "ok"
      end;
      begin match Hashtbl.hash k with
      | _ -> print_endline "ok"
      end
