let r = ref (Some 0)

let () = Gc.minor ()

                            
let rec even lim put =
  match !r with
  | Some n when n = lim -> ()
  | (Some n) as v when n mod 2 == 0 ->
     assert (Obj.is_shared (Obj.repr v));
     let next = Some (n + 1) in
     assert (not (Obj.is_shared (Obj.repr next)));
     put next;
     even lim put
  | _ -> let _ = [!r] in even lim put

let rec odd lim put =
  match !r with
  | Some n when n = lim -> ()
  | (Some n) as v when n mod 2 == 1 ->
     assert (Obj.is_shared (Obj.repr v));
     let next = Some (n + 1) in
     assert (not (Obj.is_shared (Obj.repr next)));
     put next;
     odd lim put
  | _ -> let _ = [!r] in odd lim put

                                         


let go n put =
  r := Some 0;
  Domain.spawn (fun () -> even n put);
  odd n put;
  match !r with
  | Some n ->
     Printf.printf "%d\n%!" n
  | None ->
     assert false
  

let () =
  go 100_000 (fun x -> r := Gc.promote_to x r);
  go 100_000 (fun x -> r := x)

