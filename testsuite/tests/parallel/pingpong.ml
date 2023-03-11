(* TEST
*)

let r = ref (Some 0)

let () = Gc.minor ()


let rec even lim put =
  match !r with
  | Some n when n = lim -> ()
  | (Some n) when n mod 2 == 0 ->
     let next = Some (n + 1) in
     put next;
     even lim put
  | _ -> let _ = [!r] in even lim put

let rec odd lim put =
  match !r with
  | Some n when n = lim -> ()
  | (Some n) when n mod 2 == 1 ->
     let next = Some (n + 1) in
     put next;
     odd lim put
  | _ -> let _ = [!r] in odd lim put


let go n put =
  r := Some 0;
  let d = Domain.spawn (fun () -> even n put) in
  odd n put;
  (match !r with
  | Some n ->
     Printf.printf "%d\n%!" n
  | None ->
     assert false);
  Domain.join d


let () =
  go 100_000 (fun x -> r := x)
