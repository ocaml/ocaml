(* TEST
*)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0

let num_domains =
  if test_size >= 2 then 100 else 14

let main_join n =
  let a = Array.init n (fun _ -> false) in
  Array.init n (fun i -> Domain.spawn (fun () ->
    a.(i) <- true;
    Some i
  )) |>
  Array.iteri (fun i d -> let v = Domain.join d in assert (v = Some i));
  let ok = Array.to_list a |> List.for_all (fun x -> x) in
  assert ok

let rec other_join flags n domain =
  if n > 0 then
    Domain.spawn (fun () -> flags.(n-1) <- true; Domain.join domain)
    |> other_join flags (n-1)
  else
    Domain.join domain

let join2 () =
  let r = ref false in
  let t = Domain.spawn (fun () -> r := true; true) in
  assert (Domain.join t);
  assert !r;
  assert (Domain.join t)

exception Ex of string
let join_exn () =
  match Domain.(join (spawn (fun () -> raise (Ex (String.make 5 '!'))))) with
  | _ -> assert false
  | exception (Ex "!!!!!") -> ()

let burn () =
  let rec loop l =
    if List.hd l > 14 then ()
    else loop (l @ l |> List.map (fun x -> x + 1))
  in
  loop [0];
  42

let join_slow () =
  assert (Domain.(join (spawn burn)) = 42)

let join3 () =
  let d1 = Domain.spawn burn in
  let d2 = Domain.(spawn (fun _ -> Domain.join d1)) in
  assert (Domain.join d1 = 42);
  assert (Domain.join d2 = 42)

let () =
  main_join num_domains;
  let flags = Array.make num_domains false in
  other_join flags (Array.length flags) (Domain.spawn ignore);
  assert (Array.for_all (fun x -> x) flags);
  join2 ();
  join_exn ();
  join_slow ();
  join3 ();
  Gc.full_major ();
  Gc.full_major ()
