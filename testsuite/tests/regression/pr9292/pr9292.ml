(* TEST *)

let () =
  Gc.set { (Gc.get ()) with allocation_policy = 2 };
  ignore (Array.init 5_000 (fun _ -> Array.make 10_000 0));
  Gc.full_major ()
