(* TEST *)

type e = effect E : int -> int

type f = effect F : unit

let eff_e = Effect.create ~name:"e" ()

let eff_f = Effect.create ~name:"f" ()

let rec nest = function
  | 0 -> Effect.perform eff_e (E 42)
  | n ->
      match Effect.run eff_f (fun _ -> Printf.printf "[%d\n" n; nest (n - 1)) () with
      | Result x -> Printf.printf " %d]\n" n; x
      | Exn e -> Printf.printf " !%d]\n" n; raise e
      | Operation(F, k) -> assert false

let () =
  let rec handle = function
    | Effect.Result x -> Printf.printf "= %d\n" x
    | Effect.Exn e -> raise e
    | Effect.Operation(E n, k) -> handle (Effect.continue k (n + 100))
  in
  handle (Effect.run eff_e nest 5)

let () =
  match Effect.run eff_f nest 5 with
  | Result _ -> assert false
  | Exn e -> Printf.printf "%s\n" (Printexc.to_string e)
  | Operation(F, k) -> assert false
