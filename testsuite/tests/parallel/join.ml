let main_join n =
  let a = Array.init n (fun _ -> false) in
  Array.init n (fun i -> Domain.spawn (fun () ->
    a.(i) <- true
  )) |>
  Array.iter Domain.join;
  let ok = Array.to_list a |> List.for_all (fun x -> x) in
  assert ok

let rec other_join cnt n domain =
  if n > 0 then
    Domain.spawn (fun () -> incr cnt; Domain.join domain)
    |> other_join cnt (n-1)
  else
    Domain.join domain

let join2 () =
  let r = ref false in
  let t = Domain.spawn (fun () -> r := true) in
  Domain.join t;
  assert !r;
  try
    Domain.join t;
    assert false
  with Invalid_argument _ ->
    assert !r

let () =
  main_join 100;
  let cnt = ref 0 in
  other_join cnt 100 (Domain.spawn ignore);
  assert (!cnt = 100);
  join2 ();
  Gc.full_major ();
  Gc.full_major ()
