(* TEST *)

let _ =
  let key_array =
    Array.init 128 (fun i -> Thread_local_storage.Key.create (fun _ -> i))
  in
  assert (Thread_local_storage.get (key_array.(42)) = 42);
  let d = Domain.spawn (fun _ ->
    assert (Thread_local_storage.get (key_array.(63)) = 63))
  in
  Domain.join d;
  print_endline "OK"
