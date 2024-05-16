(* TEST *)

open Effect
open Effect.Deep

type _ eff += E : unit eff
            | F : unit eff

let () =
  let ok1 = ref false
  and ok2 = ref true
  and ok3 = ref false in
  let f e r =
    try perform e with
    | Unhandled E -> r := not !r
  in
  f E ok1;
  Printf.printf "%b\n%!" !ok1;

  begin try f F ok2 with Unhandled _ -> () end;
  Printf.printf "%b\n%!" !ok2;

  begin match f E ok3 with
  | v -> v
  | effect F, k -> assert false
  end;
  Printf.printf "%b\n%!" !ok3
