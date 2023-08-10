(* TEST *)

open Effect

type e = effect E : int -> int

type f = effect F : unit

let eff_e = Effect.create ~name:"e" ()

let eff_f = Effect.create ~name:"f" ()

let rec nest = function
  | 0 -> Effect.perform eff_e (E 42)
  | n ->
     Effect.run_with eff_f (fun _ -> Printf.printf "[%d\n" n; nest (n - 1)) ()
       { result = (fun x -> Printf.printf " %d]\n" n; x);
         exn = (fun e -> Printf.printf " !%d]\n" n; raise e);
         operation =
           (fun (type a) (F : (a, _) operation) k -> assert false) }

let () =
  Effect.run_with eff_e nest 5
    { result = (fun x -> Printf.printf "= %d\n" x);
      exn = (fun e -> raise e);
      operation =
        (fun (type a) (E n : (a, _) operation) (k : (a, _) continuation) ->
          Effect.continue k (n + 100)) }

let () =
  Effect.run_with eff_f nest 5
    { result = (fun x -> assert false);
      exn = (fun e -> Printf.printf "%s\n" (Printexc.to_string e));
      operation = (fun (type a) (F : (a, _) operation) k -> assert false) }
