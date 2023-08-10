(* TEST *)

type e = effect E : unit

type f = effect F : unit

let eff_e : e Effect.t = Effect.create ()

let eff_f : f Effect.t = Effect.create ()

let () =
  let ok1 = ref false
  and ok2 = ref true
  and ok3 = ref false in
  let f eff op r =
    try Effect.perform eff op with
    | Effect.Unhandled(eff', op') as exn ->
        match Effect.equal eff_e eff', op' with
        | None, _ -> raise exn
        | Some Equal, E ->
            r := not !r
  in
  f eff_e E ok1;
  Printf.printf "%b\n%!" !ok1;

  begin try f eff_f F ok2 with Effect.Unhandled _ -> () end;
  Printf.printf "%b\n%!" !ok2;

  begin match Effect.run eff_f (f eff_e E) ok3 with
  | Effect.Result x -> x
  | Effect.Exn e -> raise e
  | Effect.Operation(_, _) -> assert false
  end;
  Printf.printf "%b\n%!" !ok3
