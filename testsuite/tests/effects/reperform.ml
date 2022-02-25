(* TEST
 *)

open Effect
open Effect.Deep

type _ t += E : int -> int t
          | F : unit t

let rec nest = function
  | 0 -> perform (E 42)
  | n ->
     match_with (fun _ -> Printf.printf "[%d\n" n; nest (n - 1)) ()
     { retc = (fun x -> Printf.printf " %d]\n" n; x);
       exnc = (fun e -> Printf.printf " !%d]\n" n; raise e);
       effc = fun (type a) (e : a t) ->
         match e with
         | F -> Some (fun k -> assert false)
         | _ -> None }

let () =
  match_with nest 5
  { retc = (fun x -> Printf.printf "= %d\n" x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
      match e with
      | E n -> Some (fun (k : (a, _) continuation) -> continue k (n + 100))
      | _ -> None }

let () =
  match_with nest 5
  { retc = (fun x -> assert false);
    exnc = (fun e -> Printf.printf "%s\n" (Printexc.to_string e));
    effc = fun (type a) (e : a t) ->
      match e with
      | F -> Some (fun k -> assert false)
      | _ -> None }
